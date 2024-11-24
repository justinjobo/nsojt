# This code is copied from flask-saml2
# See LICENSE for further details.

import warnings
with warnings.catch_warnings():
    warnings.filterwarnings("ignore", category=DeprecationWarning)
    import defusedxml.lxml

import base64
import logging
import lxml.etree
from lxml.etree import ElementBase as XmlNode
import OpenSSL.crypto
from OpenSSL.crypto import PKey, X509
from signxml import XMLVerifier
from typing import ClassVar, Iterable, Mapping, Optional, Sequence, Tuple, Union
from urllib.parse import parse_qs, urlencode
import zlib


# From flask_saml2.codex
def deflate_and_base64_encode(string_val: Union[str, bytes]) -> bytes:
    """zlib-compress and base64-encode some data."""
    if type(string_val) is str:
        string_val = string_val.encode('utf-8')

    zlibbed_str = zlib.compress(string_val)
    compressed_string = zlibbed_str[2:-4]

    return base64.b64encode(compressed_string)


# From flask_saml2.utils
class cached_property(property):

    """A decorator that converts a function into a lazy property.
    The function wrapped is called the first time to retrieve the result
    and then that calculated result is used the next time you access the value:

    .. code-block:: python

        class Foo(object):
            @cached_property
            def foo(self):
                # calculate something important here
                return 42

    The class has to have a ``__dict__`` in order for this property to
    work.
    """

    # implementation detail: A subclass of python's builtin property
    # decorator, we override __get__ to check for a cached value. If one
    # chooses to invoke __get__ by hand the property will still work as
    # expected because the lookup logic is replicated in __get__ for
    # manual invocation.

    def __init__(self, func, name=None, doc=None):
        self.__name__ = name or func.__name__
        self.__module__ = func.__module__
        self.__doc__ = doc or func.__doc__
        self.func = func

    def __get__(self, obj, type=None):
        if obj is None:
            return self
        _missing = object()
        value = obj.__dict__.get(self.__name__, _missing)
        if value is _missing:
            value = self.func(obj)
            obj.__dict__[self.__name__] = value
        return value

    def __set__(self, instance, value):
        raise AttributeError(f"Can not set read-only attribute "
                              "{type(instance).__name__}.{self.name}")

    def __delete__(self, instance):
        raise AttributeError(f"Can not delete read-only attribute "
                              "{type(instance).__name__}.{self.name}")


# From flask_saml2.signing
class Signer:
    #: The URI identifing this signing method
    uri: ClassVar[str]

    def __call__(self, data: bytes) -> str:
        """Sign some binary data and return the string output."""
        raise NotImplementedError


class RsaSha256Signer(Signer):
    uri = 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256'

    def __init__(self, key: Union[X509, PKey]):
        self.key = key

    def __call__(self, data: bytes):
        data = OpenSSL.crypto.sign(self.key, data, "sha256")
        return base64.b64encode(data).decode('ascii')


def sign_query_parameters(
    signer: Signer,
    bits: Sequence[Tuple[str, str]],
) -> str:
    """
    Sign the bits of a query string.

    .. code-block:: python

        >>> signer = ...  # A Signer instance
        >>> bits = [('Foo', '1'), ('Bar', '2')]
        >>> sign_query_parameters(signer, bits)
        "Foo=1&Bar=2&SigAlg=...&Signature=..."
    """
    bits = list(bits)

    # Add the signature algorithm parameter
    bits.append(('SigAlg', signer.uri))

    # Sign the encoded query string
    data = urlencode(bits, encoding='utf-8').encode('utf-8')
    bits.append(('Signature', signer(data)))

    return urlencode(bits, encoding='utf-8')


# From flask_saml2.xml_templates
NAMESPACE_MAP: Mapping[str, str] = {  # Namespace map
    'samlp': 'urn:oasis:names:tc:SAML:2.0:protocol',
    'saml': 'urn:oasis:names:tc:SAML:2.0:assertion',
    'md': 'urn:oasis:names:tc:SAML:2.0:metadata',
    'ds': 'http://www.w3.org/2000/09/xmldsig#',
}


class XmlTemplate:
    """Base XML template class.
    A template can represent a single node, a tree, or a whole XML document.
    """

    #: XML namespace for this node or document
    namespace = None

    def __init__(self, params: dict = {}):
        """Initialize this template using the supplied parameters dict.
        The parameters will be used in :meth:`generate_xml`.
        """
        self.params = params.copy()

    @cached_property
    def xml(self) -> XmlNode:
        """The XML node this template constructed.
        Generated using :meth:`generate_xml`.
        """
        return self.generate_xml()

    def generate_xml(self) -> XmlNode:
        """Generate the XML node for this template.
        Generally accessed through :attr:`xml`.
        """
        raise NotImplementedError

    def get_xml_string(self) -> str:
        """Render the XML node to a string.
        The string representation is rendered as canonical c14n XML,
        to make verification and signing possible.
        """
        return lxml.etree.tostring(self.xml, method='c14n',
                                   exclusive=True).decode('utf-8')

    def element(
        self,
        tag: str,
        *,
        namespace: Optional[str] = None,
        attrs: Optional[Mapping[str, Optional[str]]] = None,
        children: Optional[Iterable[Optional[XmlNode]]] = None,
        text: Optional[str] = None
    ) -> XmlNode:
        """
        Shortcut for creating an ElementTree Element, with optional attributes,
        children, and text.

        :param tag str: tag to give XML element
        :param namespace str: Namespace to use for the element. Defaults to
            :meth:`get_namespace()` if None.
        :param attrs dict: Element attributes. If an attribute value is None,
            the attribute is ignored.
        :param children list: Element children. If an item in children is None,
            the item is ignored.
        :param text str: Element text content, if any.
        :return: xml.etree.ElementTree.Element
        """
        if namespace is None:
            namespace = self.get_namespace()

        tag_name = f'{{{namespace}}}{tag}'
        element = lxml.etree.Element(tag_name, nsmap=self.get_namespace_map())

        if attrs is not None:
            for k, v in attrs.items():
                if v is not None:
                    element.set(k, v)

        if children is not None:
            for child in children:
                if child is not None:
                    element.append(child)

        if text is not None:
            element.text = text

        return element

    def get_namespace_map(self) -> Mapping[str, str]:
        """Get all the namespaces potentially used by this node, as a etree
        nsmap.
        """
        return NAMESPACE_MAP

    def get_namespace(self) -> str:
        """Get the namespace URI for this node.
        Looks up the namespace alias :attr:`namespace`
        in :meth:`get_namespace_map`.
        """
        return self.get_namespace_map()[self.namespace]


# From flask_saml2.sp.parser
class XmlParser:
    """Parse a possibly-signed XML document.
    Subclasses must implement :meth:`is_signed`.
    """
    #: The input XML document as a string
    xml_string: str

    #: The parsed XML document
    xml_tree: XmlNode

    #: The certificate the document is signed with
    certificate: Optional[X509] = None

    def __init__(self, xml_string: str, certificate: Optional[X509]):
        """
        :param xml_string: The XML string to parse
        :param x509cert: A preshared X509 certificate to validate the signed
           XML document with
        """
        self._logger = logging.getLogger(__name__)

        if certificate is not None:
            self.certificate = certificate

        self.xml_string = xml_string
        self.xml_tree = self.parse_request(xml_string)
        if self.is_signed():
            self.xml_tree = self.parse_signed(self.xml_tree, self.certificate)

    def parse_request(self, xml_string) -> None:
        """
        Parse the SAML request.
        :raises: ValueError
        """
        try:
            from io import BytesIO
            xml_bytes = BytesIO(bytes(xml_string, "utf-8"))
            return defusedxml.lxml._etree.parse(xml_bytes)
        except lxml.etree.Error:
            message = "Could not parse request XML"
            self._logger.exception(message)
            raise ValueError(message)

    def is_signed(self):
        """Is this request signed? Looks for a ``<ds:Signature>`` element.
        Different sources will generate different signed XML documents,
        so this method must be implemented differently for each source.
        """
        raise NotImplementedError

    def parse_signed(self, xml_tree: XmlNode, certificate: X509) -> XmlNode:
        """
        Replaces all parameters with only the signed parameters. You should
        provide an x509 certificate obtained out-of-band, usually via the
        SAML metadata. Otherwise the signed data will be verified with only
        the certificate provided in the request. This is INSECURE and
        more-or-less only useful for testing.
        """
        return XMLVerifier().verify(xml_tree, x509_cert=certificate).signed_xml

    def _xpath_xml_tree(self, xpath_statement):
        return self._xpath(self.xml_tree, xpath_statement)

    def _xpath(self, base: XmlNode, xpath_statement: str) -> Iterable:
        return base.xpath(xpath_statement, namespaces=self.get_namespace_map())

    def get_namespace_map(self):
        return NAMESPACE_MAP


# From flask_saml2.sp.idphandler
class ResponseParser(XmlParser):

    def is_signed(self):
        sig = self.xml_tree.xpath('/samlp:Response/ds:Signature',
                                  namespaces=self.get_namespace_map())
        return bool(sig)

    @cached_property
    def issuer(self) -> str:
        return self._xpath_xml_tree('/samlp:Response/saml:Issuer')[0].text

    @cached_property
    def response_id(self) -> str:
        return self._xpath_xml_tree('/samlp:Response/@ID')[0]

    @cached_property
    def destination(self) -> str:
        try:
            return self._xpath_xml_tree('/samlp:Response/@Destination')[0]
        except IndexError:
            return ''

    @cached_property
    def version(self) -> str:
        return self._xpath_xml_tree('/samlp:Response/@Version')[0]

    @cached_property
    def issue_instant(self) -> str:
        return self._xpath_xml_tree('/samlp:Response/@IssueInstant')[0]

    @cached_property
    def assertion(self) -> XmlNode:
        return self._xpath_xml_tree('/samlp:Response/saml:Assertion')[0]

    @cached_property
    def subject(self) -> XmlNode:
        return self._xpath(self.assertion, 'saml:Subject')[0]

    @cached_property
    def nameid(self) -> str:
        return self._xpath(self.subject, 'saml:NameID')[0].text

    @cached_property
    def nameid_format(self) -> str:
        return self._xpath(self.subject, 'saml:NameID/@Format')[0]

    @cached_property
    def attributes(self) -> Mapping[str, str]:
        attributes = self._xpath(self.assertion,
                                 'saml:AttributeStatement/saml:Attribute')
        return {el.get('Name'): self._xpath(el, 'saml:AttributeValue')[0].text
                for el in attributes}

    @cached_property
    def conditions(self) -> Optional[XmlNode]:
        try:
            return self._xpath(self.assertion, './saml:Conditions')[0]
        except IndexError:
            return None


# From flask.sp.xml_templates
class AuthnRequest(XmlTemplate):
    namespace = 'samlp'

    def get_issuer(self):
        namespace = self.get_namespace_map()['saml']
        return self.element('Issuer', namespace=namespace,
                            text=self.params['ISSUER'])

    def generate_xml(self):
        return self.element('AuthnRequest', attrs={
            'ID': self.params['REQUEST_ID'],
            'Version': '2.0',
            'IssueInstant': self.params['ISSUE_INSTANT'],
            'Destination': self.params['DESTINATION'],
            'ProtocolBinding': 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST',
            'AssertionConsumerServiceURL': self.params['ACS_URL'],
        }, children=[
            self.get_issuer(),
        ])

    """
    <samlp:AuthnRequest
        xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"
        xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
        ID="${ID}"
        Version="2.0"${PROVIDER_NAME}
        IssueInstant="${ISSUE_INSTANT}"
        Destination="${DESTINATION}"
        ProtocolBinding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST"
        AssertionConsumerServiceURL="${ACS_URL}">
        <saml:Issuer>${ENTITY_ID}</saml:Issuer>
    </samlp:AuthnRequest>
    """
