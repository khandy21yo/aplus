10	! &
	! PPNBLD - Build accounts on another disk. &
	! &
	! This program will create (using wildcards) accounts that exist on &
	! one disk device onto another disk.  The format of the command line &
	! is -- devto:=devfrom:[p,q] -- where devto: is the device where the &
	! accounts are to be built on, devfrom: is the device which they &
	! already exist, and [p,q] is the account numbers that are to be &
	! built in wildcard format. &
	! &
	! Example -- DL0:=DB1:[1,*] will cause all of the [1,*] accounts &
	! that already exist on DB1: to be built on DL0: &
	!
100	! &
	! Program initilization &
	!
105	  ON ERROR GOTO 19000 &
	! Trap any errors which may occur &

110	  OPEN "KB:" FOR INPUT AS FILE 1% &
	! Open keyboard for non-question mark input &

120	  DIM S0%(30%),S1%(30%),S2%(30%) &
	! Arrays used to lookup and create accounts &

1000	! &
	! Enter connamd line &
	!
1010	  PRINT "PPNBLD> "; &
	\ INPUT LINE #1%, P0$ &
	\ P0$=CVT$$(P0$,-1%) &
	\ GOTO 1010 IF P0$="" &
	\ GOTO 32767 IF P0$="EXIT" &
	! Enter command line &

1020	  I%=INSTR(1%,P0$,"=") &
	\ IF I%=0% &
	  THEN	  PRINT "Unable to locate equal sign '='." &
		\ GOTO 1000 &
	! Find devto: in command line &

1030	  T0$=LEFT(P0$,I%-1%) &
	\ P0$=RIGHT(P0$,I%+1%) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+T0$) TO S1% &
	\ S1%=S1%(29%)+SWAP%(S1%(30%)) &
	\ IF (S1% AND 8192%)=0% &
	  THEN	  PRINT "Unable to find output device name!" &
		\ GOTO 1000 &
	! Strip devto off file name and parse into SYS() format. &

1035	  IF S1%<0% &
	  THEN	  PRINT "Undefined output device name!" &
		\ GOTO 1000 &
	! Undefined device name &

1040	  IF (STATUS AND 255%)<>0% &
	  THEN	  PRINT "Output device is not a disk!" &
		\ GOTO 1000 &
	! Verify that the output goes to a disk &

1200	  CHANGE SYS(CHR$(6%)+CHR$(-23%)+P0$) TO S0% &
	\ P1$=LEFT(P0$,LEN(P0$)-RECOUNT) &
	\ P0$=RIGHT(P0$,LEN(P0$)-RECOUNT+1%) &
	! Get account number part of command to use to read account numbers &

1205	  S1%=S0%(29%)+SWAP%(S0%(30%)) &
	\ IF (S1% AND 128%)=0% &
	  THEN	  PRINT "Specify account numbers on input '";P1$;"'." &
		\ GOTO 1000 &
	! Check for account number &

1210	  S0%(1%)=6% &
	\ S0%(2%)=25% &
	\ S0%(3%)=-1% &
	! Initilize wildcard ppn lookup &

1300	  S0%(3%)=S0%(3%)+1% &
	\ S0%(4%)=SWAP%(S0%(3%)) &
	\ CHANGE S0% TO S0$ &
	\ CHANGE SYS(S0$) TO S2% &
	! Get an account number &

1320	  S2%(1%)=6% &
	\ S2%(2%)=14% &
	\ S2%(3%),S2%(4%)=0% &
	\ S2%(7%)=S2%(5%) &
	\ S2%(8%)=S2%(6%) &
	\ S2%(5%),S2%(6%)=0% &
	\ S2%(9%)=1% &
	\ CHANGE S2% TO S2$ &
	\ CHANGE SYS(S2$) TO S2% &

1400	  PRINT "[";NUM1$(S2%(8%));",";NUM1$(S2%(7%));"]	"; &
		  "Quota";S2%(27%)+SWAP%(S2%(28%));"	UFD"; &
		  S2%(29%)+SWAP%(S2%(30%));"	"; &
	! Show account number found &

1500	  S1%(1%)=6% &
	\ S1%(2%)=0% &
	\ S1%(3%),S1%(4%),S1%(5%),S1%(6%)=0% &
	\ S1%(7%)=S2%(7%) &
	\ S1%(8%)=S2%(8%) &
	\ S1%(I%)=S2%(I%) FOR I%=9% TO 12% &
	\ S1%(13%)=S2%(27%) &
	\ S1%(14%)=S2%(28%) &
	\ S1%(27%)=0% &
	\ S1%(28%)=S2%(30%) &

1505	  IF S2%(8%)=0% &
	  THEN	  PRINT "Unable to build [0,*]!" &
		\ GOTO 1800 &

1510	  CHANGE S1% TO S1$ &
	\ V$=SYS(S1$) &
	\ PRINT "Built!" &

1800	  GOTO 1300 &
	! Loop back for another account &

1900	  GOTO 1200 IF P0$<>"" &
	! Get another account group &

1910	  PRINT "Done . . ." &
	\ GOTO 1000 &
	! Finished &

16000	! &
	! Function definitions &
	!
16010	  DEF FNE0$(E0%)=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E0%)),3%),4%) &

19000	! &
	! Error traps &
	!
19010	  IF ERR=11% AND ERL=1010% &
	  THEN	  GOTO 32767 &
	! ^Z, so end program and quit &

19020	  IF ERL=1030% &
	  THEN	  PRINT "Error in reading output device name! ";FNE0$(ERR) &
		\ RESUME 1000 &

19030	  IF ERL=1200% &
	  THEN	  PRINT "Error on source file '";P0$ &
		\ PRINT "   Error: "FNE0$(ERR) &
		\ RESUME 1000 &

19040	  IF ERL=1300% AND ERR=5% &
	  THEN	  RESUME 1900 &
	! Read last account in group &

19045	  IF ERL=1510% AND ERR=16% &
	  THEN	  PRINT "Already exists!" &
		\ RESUME 1300 &

19050	  IF ERL=1320% &
	  THEN	  PRINT "Unable to read accounting data!" &
		\ RESUME 1000 &

19060	  IF ERL=1510% &
	  THEN	  PRINT "Unable to build account! ";FNE0$(ERR) &
		\ RESUME 1000 &

19990	  ON ERROR GOTO 0 &

32767	  END
