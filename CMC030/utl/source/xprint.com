$! This COM is used to dump out a file to the printer
$! port, bypassing as much of VMS's character translations
$! as possible.
$!
$ SET NOVERIFY
$ SET TERM/NOBROADCAST/NOWRAP/FORM/TAB
$! ESC[0,8] = 27
$ IF P1.NES."" THEN GOTO Good
$ WRITE SYS$OUTPUT "Filename missing"
$ GOTO Exit
$Good:
$! WRITE SYS$OUTPUT ESC + "[5i"
$! COPY xprint.init SYS$OUTPUT
$! COPY 'P1 SYS$OUTPUT
$! WRITE SYS$OUTPUT ESC + "[4i"
$ XPRINT :== $DKA0:[KEVIN]XPRINT.EXE
$ XPRINT -C 'P1
$Exit:
$ SET TERM/BROADCAST
