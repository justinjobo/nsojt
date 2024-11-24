# tcsh/csh environment setup for NSO ncs-6.2.5
setenv NCS_DIR /home/developer/nso-6.2.5
setenv PATH $NCS_DIR/bin:${PATH}
if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH $NCS_DIR/lib:${LD_LIBRARY_PATH}
else
    setenv LD_LIBRARY_PATH $NCS_DIR/lib
endif
if ($?PYTHONPATH) then
    setenv PYTHONPATH $NCS_DIR/src/ncs/pyapi:${PYTHONPATH}
else
    setenv PYTHONPATH $NCS_DIR/src/ncs/pyapi
endif
if ($?MANPATH) then
    setenv MANPATH $NCS_DIR/man:${MANPATH}
else
    setenv MANPATH $NCS_DIR/man:/usr/share/man
endif
