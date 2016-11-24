10	  ! &
	  ! Program name: bcr1		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 03:11 AM
50	  !
60	  DIM NAMES$(128%), SIZES%(128%), FIP0%(30%), FIP1%(30%)
100	  !
105	  DEFAULT$="/SOU/SUP/OFF/FUN/KEY/WID:"
110	  ON ERROR GOTO 19000
120	  PRINT  &
	\ PRINT "Enter file name(s) to print "; &
	\ INPUT LINE NAMES$ &
	\ NAMES$=CVT$$(NAMES$,4%)
130	  NAMES%=0%
200	  INPUT "WIDTH = (80/132) <80>";WID$ &
	\ IF WID$="" THEN  &
		  WID$="80"
205	  DEFAULT$=DEFAULT$+WID$
210	  OPTIONS$=DEFAULT$ &
	\ PRINT "Output to KBnn: "; &
	\ INPUT LINE KBNUM$ &
	\ KBNUM$=CVT$$(KBNUM$,4%) &
	\ I%=INSTR(1%,KBNUM$,"/") &
	\ IF I% THEN  &
		  OPTIONS$=RIGHT(KBNUM$,I%) &
		\ KBNUM$=LEFT(KBNUM$,I%-1%)
1000	  !
1010	  CHANGE SYS(CHR$(6%)+CHR$(-23%)+NAMES$) TO FIP0% &
	\ THIS%=LEN(NAMES$)-RECOUNT &
	\ THIS$=LEFT(NAMES$,THIS%) &
	\ NAMES$=RIGHT(NAMES$,THIS%+2%) &
	\ JOB$=RIGHT(NUM1$(100%+FIP0%(1%)/2%),2%) &
	\ IF FIP0%(11%)=0% THEN  &
		  FIP0%(11%)=187% &
		\ FIP0%(12%)=12%
1020	  FIP0%(1%)=6% &
	\ FIP0%(2%)=17% &
	\ FIP0%(3%)=0% &
	\ FIP0%(4%)=0%
1030	  CHANGE FIP0% TO FIP0$ &
	\ CHANGE SYS(FIP0$) TO FIP1%
1040	  NAMES%=NAMES%+1% &
	\ DEVICE$="" &
	\ DEVICE$=CHR$(FIP1%(23%))+CHR$(FIP1%(24%)) &
	\ DEVICE$=DEVICE$+NUM1$(FIP1%(25%) IF FIP1%(26%) &
	\ DEVICE$=CVT$$(DEVICE$,-1%) &
	\ DEVICE$=DEVICE$+":" UNLESS DEVICE$="" &
	\ A1$="   " &
	\ RSET A1$=NUM1$(FIP1%(6%)) &
	\ A2$="   " &
	\ LSET A2$=NUM1$(FIP1%(5%)) &
	\ NAMES$(NAMES%)=DEVICE$+"["+A1$+","+A2$+"]"+RAD$(FIP1%(7%)+SWAP%(FIP1%(8%)))+RAD$(FIP1%(9%)+SWAP%(FIP1%(10%)))+"."+RAD$(FIP1%(11%)+SWAP%(FIP1%(12%))) &
	\ SIZES%(NAMES%)=FIP1%(13%)+SWAP%(FIP1%(14%))
1050	  FIP0%(3%)=FIP0%(3%)+1% &
	\ FIP0%(4%)=SWAP%(FIP0%(3%)) &
	\ GOTO 1030
1060	  GOTO 1010 IF NAMES$<>"" &
	\ PRINT "File names looked up"
2000	  !
2010	  FOR I%=1% TO NAMES%
2020		  FOR J%=1% TO NAMES%-I%
2030			  IF NAMES$(J%)>NAMES$(J%+1%) THEN  &
				  J$=NAMES$(J%) &
				\ NAMES$(J%)=NAMES$(J%+1%) &
				\ NAMES$(J%+1%)=J$
2040				  NEXT J%
2050			  NEXT I%
2090	  PRINT "Names sorted"
5000	  !
5010	  OPEN "BCR"+JOB$+"B.TMP" FOR OUTPUT AS FILE 2% &
	\ GOSUB 5100
5020	  FOR I%=1% TO NAMES% &
		\ THIS$=NAMES$(I%) &
		\ PRINT #2%, USING "\             \ ##### BLOCKS   ", RIGHT(THIS$,INSTR(1%,THIS$,"[")),SIZES%(I%);
5030		  OPEN NAMES$(I%) FOR INPUT AS FILE 3%
5040		  INPUT LINE #3%, TITLE$ &
		\ TITLE$=CVT$$(TITLE$,4%) &
		\ J%=INSTR(1%,TITLE$,"REM") &
		\ IF J%=0% THEN  &
			  J%=INSTR(1%,TITLE$,"!") &
			\ J%=J%+1%
5050		  TITLE$=CVT$$(RIGHT(TITLE$,J%),8%) &
		\ GOTO 5040 IF TITLE$="" OR TITLE$="&" OR RIGHT(TITLE$,LEN(TITLE$)-5%)="EXTEND"
5060		  PRINT #2%, TITLE$
5070			  NEXT I%
5080	  CLOSE 2% &
	\ PRINT "Index built" &
	\ GOTO 6000
5100	  !
5110	  PRINT #2%, CHR$(12%) &
	\ PRINT #2% &
	\ PRINT #2% &
	\ PRINT #2%
5120	  PRINT #2%, "* * * * * LIBRARY OF PROGRAMS ON SYSTEM DISK FILE ";"IN BASIC LANGUAGE * * * * *" &
	\ PRINT #2%, "TABLE OF CONTENTS";TAB(30%);TIME$(0%);" ON ";DATE$(0%);TAB(60%);"TABLE OF CONTENTS" &
	\ PRINT #2% &
	\ PRINT #2%, " PROGRAM           SIZE     DESCRIPTION" &
	\ PRINT #2%, STRING$(76%,ASCII("-"))
5130	  RETURN
6000	  !
6010	  OPEN "[1,7]BCRB"+JOB$+".CTL" FOR OUTPUT AS FILE 3% &
	\ PRINT #3%, "&&"
6020	  PRINT #3%, "ASSIGN ";KBNUM$ &
	\ PRINT #3%, "PIP ";KBNUM$;"=BCR";JOB$;"B.TMP"
6030	  FOR I%=1% TO NAMES% &
		\ PRINT #3%, "B1C BCR";JOB$;"B.TMP=";NAMES$(I%);OPTIONS$ &
		\ PRINT #3%, "PIP ";KBNUM$;"=BCR";JOB$;"B.TMP" &
	\ NEXT I%
6040	  PRINT #3%, "KILL 'BCR";JOB$;"B.TMP'" &
	\ PRINT #3%, "KILL '[1,7]BCRB";JOB$;".CTL'" &
	\ PRINT #3%, "PIP ";KBNUM$;"=$BCR.END"
6050	  PRINT "Control file built" &
	\ CLOSE 3%
7000	  !
7010	  PRINT  &
	\ PRINT "Now que the command file 'BCRB";JOB$;"'." &
	\ PRINT "and make sure that the keyboard '";KBNUM$;"'" &
	\ PRINT "is free before starting." &
	\ PRINT 
7020	  GOTO 32767
19000	  !
19010	  IF ERL=1030% AND ERR=5% THEN  &
		  RESUME 1060
19999	  ON ERROR GOTO 0
32767	  END
