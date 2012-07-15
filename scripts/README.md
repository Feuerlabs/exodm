prepend_licenses
================

Is a script that prepend licenses files to source code files.
Put the LICENSES directory in the home directory. For each
company or license owner create a directory COMPANY and put the following
files in that directory:

    LICENSE
    copyright.c
    copyright.erl
    copyright.md
    copyright.txt
    
To run the script just cd to the directory where you have the files
and type:

    export LICENSE_OWNER=COMPANY
    prepend_licenses <files>
    
If the LICENSE_OWNER is not defined the name "Default" is used.
prepend_licenses will recursivly go through all the filess and
use the appropriate and prepend that to the file.

If shit happends do no forget that you have the command

    git checkout -- <files>
    
To undo the changes.

