let requestId = 0;

const fetchFunc = window && window.fetch;

function JsonRpcError(
    error,
    body,
) {
    this.message = error.message;
    this.internal = error.internal;
    this.type = error.type;
    this.code = error.code;
    this.data = error.data;
    this.body = body;
}
JsonRpcError.prototype = Object.create(Error.prototype);

function HttpError(
    method,
    xhr,
    body,
) {
    this.type = 'http.error';
    this.details = JSON.stringify({ method });
    this.status = xhr.status;
    this.statusText = xhr.statusText;
    this.errorMessage = xhr.responseText;
    this.body = body;
}
HttpError.prototype = Object.create(Error.prototype);

function getNextRequestId() {
    requestId += 1;
    return requestId;
}

function jsonrpcRequest(method, params, id) {
    return { jsonrpc: '2.0', id, method, params };
}

function newRequest(method, params, id) {
    return JSON.stringify(jsonrpcRequest(method, params, id));
}

export const base64DecodeUtf8 = str => decodeURIComponent(
    window.atob(str)
        .split('')
        .map(c => `%${(`00${c.charCodeAt(0).toString(16)}`).slice(-2)}`)
        .join(''),
);

async function jsonrpcCommon(fetch, body, method, baseUrl) {
    const url = `${baseUrl}/jsonrpc/${method}`;
    const headers = {
        Accept: 'application/json;charset=utf-8',
        'Content-Type': 'application/json;charset=utf-8',
    };
    const response = await fetch(url, {
        method: 'POST',
        credentials: 'same-origin',
        headers,
        body,
    });

    if (response.status !== 200) {
        throw new HttpError(method, response, body);
    }

    return response.json();
}

export default async function jsonrpcSimple(
    method,
    params,
    {
        funcs: {
            fetch = fetchFunc,
            getId = getNextRequestId,
        } = {},
    } = {},
) {
    const reqId = getId();
    const baseUrl = '';

    const body = newRequest(method, params, reqId);

    const json = await jsonrpcCommon(fetch, body, method, baseUrl);

    if (json.error) {
        throw new JsonRpcError(json.error, body);
    }

    return json.result;
}

export const login = async ({
    user,
    passwd,
    funcs: {
        jsonrpc = jsonrpcSimple,
    } = {},
} = {}) => jsonrpc('login', { user, passwd });

export const challengeResponse = async ({
    ackWarning,
    challengeId,
    response,
    funcs: {
        jsonrpc = jsonrpcSimple,
    } = {},
} = {}) => jsonrpc('challenge_response', {
    challenge_id: challengeId,
    ack_warning: ackWarning,
    response,
});
