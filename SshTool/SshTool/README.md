This tool will transfer a bunch a files and a script over ssh to multiple remote hosts,
execute the script in parallel on all destinations, then will clean up and return the
stdout, stderr and exit code for each.

The format of the ssh host file (param 1) is

hostname  port   userid   password

.... more 4 tuples optional ....

Ssh private keys are not supported at this time.
Param 2 is the script name, and the remaining params if any are additional files
to transfer.

Solution and project files were created with visual studio.