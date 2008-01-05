@echo off
rem this ends up being a call to a batch file outside the project tree
rem that's because it contains passwords to my slave box where NaturalDocs lives
rem on the slave, v: is mapped to "d:\projects" on the primary computer.
rem 
rem if you're not familiar with psexec, it's part of pstools, ask google.
rem
rem gendoc_newac.bat looks like this:
rem
rem copy d:\projects\newac\doc\config\newac.css \\slave\d$\naturaldocs\styles
rem psexec \\slave -u user -p pass d:\naturaldocs\nd.bat -s newac -img v:\newac\doc\images\ -i v:\newac\ -o html v:\newac\doc\html -p v:\newac\doc\config

call d:\projects\gendoc_newac.bat

