NSO Commit Parameters Showcase
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example features examples of how to detect and apply different commit
parameters, such as dry run and commit with trace ID, directly from either Python
or Java packages or code.

Running the Examples
~~~~~~~~~~~~~~~~~~~

You need NSO, Python and Java installed, and the ncsrc file sourced to run this
example.
The example is a shell script that runs both examples by using the make command.
Both Java and Python examples feature a service that tries to modify interface
configuration on 'ex0' device, then adds the dry-run and trace-id commit
parameters, and applies the transaction.
The examples show how to detect arbitrary and specific commit parameters in
service code and how to apply commit parameters from code either as an action
or from a script. It also contains a list of all currently existing commit
parameters available to you.
Be aware though, that some combinations of commit parameters will not work
simultaneously in the same commit, such as no-lsa and use-lsa, or dry_run_cli 
and dry_run_native.

- make showcase-commit-params
  This example uses Java and Python packages to apply commit parameters to
  a transaction and detect them from service code.

Further Reading
~~~~~~~~~~~~~~~

+ NSO User Guide
+ Python API reference documentation (ncs.maapi).
+ Java API reference documentation (com.tailf.maapi.Maapi).
+ The Python and Java packages in ./package-directory directory
