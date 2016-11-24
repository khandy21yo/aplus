10	  ! &
	  ! Program name: filcon		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  OPEN "KB:" FOR INPUT AS FILE 1%
30	  DIM A%(100%,2%)
40	  DIM A$(100%,1%)
1000	  !
1010	  PRINT "EXISTING FILE - "; &
	\ INPUT LINE #1%, E$ &
	\ E$=CVT$$(E$,4%) &
	\ IF E$="?" THEN  &
		  GOSUB 8010 &
		\ GOTO 1010
1020	  PRINT "NEW FILE - "; &
	\ INPUT LINE #1%, N$ &
	\ N$=CVT$$(N$,4%) &
	\ IF N$="?" THEN  &
		  GOSUB 8020 &
		\ GOTO 1020
1030	  I%=INSTR(1%,N$,"/") &
	\ IF I%<>0% THEN  &
		  C$=RIGHT(N$,I%+1%) &
		\ N$=LEFT(N$,I%-1%) &
		\ I%=-1%
1040	  E$=E$+".DAT" UNLESS INSTR(1%,E$,".")<>0% &
	\ N$=N$+".DAT" UNLESS INSTR(1%,N$,".")<>0%
1050	  IF I%=-1% THEN  &
		  I%=INSTR(1%,C$,",") &
		\ GOTO 8030 IF I%=0% &
		\ C1%=VAL(LEFT(C$,I%-1%)) &
		\ C2%=VAL(RIGHT(C$,I%+1%)) &
		\ RETURN IF C1%<4% OR C2%<4% OR C1%>512% OR C2%>512% OR C1%>C2% &
		\ OPEN N$ FOR OUTPUT AS FILE 12% &
		\ PRINT #12%, CVT%$(0%)+CVT%$(C1%)+"S"+CHR$(128%); &
		\ CLOSE 12% &
		\ OPEN LEFT(N$,LEN(N$)-1%)+"1" FOR OUTPUT AS FILE 12% &
		\ PRINT #12%, CVT%$(0%)+CVT%$(C2%)+"S"+CHR$(128%); &
		\ CLOSE 12%
2000	  PRINT "ENTER FIELDS FOR OLD FILE. . ."
2010	  L%=1%
2020	  PRINT USING "##. ", L%;
2030	  INPUT LINE #1%, F$ &
	\ F$=CVT$$(F$,4%)
2040	  IF LEFT(F$,1%)="G" THEN  &
		  F$=RIGHT(F$,2%) &
		\ L%=VAL(F$) &
		\ GOTO 2020
2050	  IF F$="?" THEN  &
		  GOSUB 8040 &
		\ GOTO 2020
2055	  IF F$="" THEN  &
		  GOTO 2080
2060	  A%(L%,0%)=VAL(F$)
2070	  L%=L%+1% &
	\ GOTO 2020 UNLESS L%=101%
2080	  A%(0%,0%)=L%-1% &
	\ T%=0% &
	\ T%=T%+A%(L%,0%) FOR L%=1% TO A%(0%,0%)
2090	  GOTO 2110 IF (T% XOR 2%^L%)=0% FOR L%=0% TO 15%
2100	  I%=0% &
	\ I%=T% AND 2%^L% FOR L%=15% STEP -1% UNTIL I%<>0% OR L%<0% &
	\ I%=I%*2% &
	\ PRINT "CONFIRM ADDING A";I%-T%;"CHARACTER FIELD ON THE" &
	\ PRINT "END TO MAKE THE RECORD LENGTH";I%;" (Y/N) ? "; &
	\ INPUT LINE #1%, C$ &
	\ C$=CVT$$(C$,-1%) &
	\ L%=A%(0%,0%) UNLESS C$="Y" &
	\ GOTO 2070 UNLESS C$="Y" &
	\ A%(A%(0%,0%)+1%,0%)=I%-T% &
	\ A%(0%,0%)=A%(0%,0%)+1% &
	\ T%=I%
2110	  PRINT 
2120	  PRINT "ENTER FIELDS FOR NEW FILE. . ."
2130	  L%=1%
2140	  PRINT USING "NEW ##. ", L%;
2150	  INPUT LINE #1%, F$ &
	\ F$=CVT$$(F$,4%)
2160	  IF LEFT(F$,1%)="G" THEN  &
		  L%=VAL(RIGHT(F$,2%)) &
		\ GOTO 2140
2170	  IF F$="?" THEN  &
		  GOSUB 8050 &
		\ GOTO 2140
2180	  GOTO 2250 IF F$=""
2190	  I%=INSTR(1%,F$,",") &
	\ IF I%=0% THEN  &
		  A%(L%,2%)=-1% &
		\ GOTO 2210
2200	  A%(L%,2%)=VAL(RIGHT(F$,I%+1%)) &
	\ F$=LEFT(F$,I%-1%)
2210	  A%(L%,1%)=VAL(F$)
2220	  L%=L%+1% &
	\ GOTO 2140 UNLESS L%=101%
2250	  A%(0%,1%)=L%-1% &
	\ T1%=0% &
	\ T1%=T1%+A%(L%,1%) FOR L%=1% TO A%(0%,1%)
2260	  GOTO 2280 IF (T1% XOR 2%^L%)=0% FOR L%=0% TO 15%
2270	  I%=0% &
	\ I%=T1% AND 2%^L% FOR L%=15% STEP -1% UNTIL I%<>0% OR L%<0% &
	\ I%=I%*2% &
	\ PRINT "CONFIRM ADDING A";I%-T1%;"CHARACTER FIELD ON THE" &
	\ PRINT "END TO MAKE THE RECORD LENGTH";I%;" (Y/N) ? "; &
	\ INPUT LINE #1%, C$ &
	\ C$=CVT$$(C$,-1%) &
	\ GOTO 2070 UNLESS C$="Y" &
	\ A%(A%(0%,1%)+1%,1%)=I%-T1% &
	\ A%(A%(0%,1%)+1%,2%)=-1% &
	\ A%(0%,1%)=A%(0%,1%)+1% &
	\ T1%=I%
2280	  PRINT 
3000	  OPEN "NL:" AS FILE 5%, RECORDSIZE T%
3010	  C%=0%
3020	  FOR L%=1% TO A%(0%,0%) &
		\ FIELD #5%, C% AS G$,A%(L%,0%) AS A$(L%,0%) &
		\ C%=C%+A%(L%,0%) &
	\ NEXT L% &
	\ FIELD #5%, T% AS T$
3030	  OPEN "NL:" AS FILE 6%, RECORDSIZE T1%
3040	  C%=0%
3050	  FOR L%=1% TO A%(0%,1%) &
		\ FIELD #6%, C% AS G$,A%(L%,1%) AS A$(L%,1%) &
		\ C%=C%+A%(L%,1%) &
	\ NEXT L% &
	\ FIELD #6%, T1% AS T1$
3060	  IF FNO%(7%,E$,"","")<>0% THEN  &
		  GOTO 8060
3070	  IF FNO%(9%,N$,"","")<>0% THEN  &
		  GOTO 8070
4000	  PRINT "STARTING CONVERSION. . ." &
	\ PRINT 
4010	  IF FNG%(7%,"")<>0% THEN  &
		  GOTO 4200
4020	  LSET T$=FNL$
4030	  FOR L%=1% TO A%(0%,1%)
4040		  IF A%(L%,2%)=-1% THEN  &
			  LSET A$(L%,1%)="" &
			\ GOTO 4100
4050		  IF A%(L%,2%)=0% THEN  &
			  LSET A$(L%,1%)=STRING$(A%(L%,1%),0%) &
			\ GOTO 4100
4060		  LSET A$(L%,1%)=A$(A%(L%,2%),0%)
4100			  NEXT L%
4105	  PRINT "!"; &
	\ PRINT  IF POS(0%)>70%
4110	  IF FNA%(9%,T1$) THEN  &
		  PRINT "ERROR";FNS%;"IN FNA%()." &
		\ STOP
4120	  GOTO 4020 UNLESS FNN%(7%)
4200	  PRINT "COMPLETED." &
	\ GOTO 10000
8000	  !
8010	  PRINT  &
	\ PRINT "Enter the name of the old file you are converting from." &
	\ PRINT  &
	\ RETURN
8020	  PRINT  &
	\ PRINT "Enter the name of the new file to be created. If you want this" &
	\ PRINT "program to create the file for you, type '/n,m' where" &
	\ PRINT "n is the key length and m is the data record length." &
	\ PRINT  &
	\ RETURN
8030	  PRINT  &
	\ PRINT "Type '?' for instructions on how to build a file." &
	\ PRINT  &
	\ GOTO 1020
8040	  PRINT  &
	\ PRINT "Define each field in the old file by entering the" &
	\ PRINT "lengths of each. Type <RETURN> after the last field" &
	\ PRINT "is entered. If you make a mistake and wish to go back," &
	\ PRINT "enter 'G##' where '##' is the number of the field you wish" &
	\ PRINT "to return to.  Note: 100 is the maximum number of fields." &
	\ PRINT  &
	\ RETURN
8050	  PRINT  &
	\ PRINT "Enter the field length, followed by a comma, followed by" &
	\ PRINT "the field in the old file the data should come from." &
	\ PRINT "To create a blank field, just enter the field length." &
	\ PRINT  &
	\ RETURN
8060	  PRINT "FILE ";E$;" NOT FOUND. PROGRAM ENDED." &
	\ GOTO 10000
8070	  PRINT "FILE ";N$;" NOT FOUND. PROGRAM ENDED." &
	\ GOTO 10000
10000	  !
10010	  V%=FNC%(7%)+FNC%(9%) &
	\ CLOSE 1% &
	\ CLOSE 5% &
	\ CLOSE 6% &
	\ CLOSE 12% &
	\ V%=FNX%("",0%,"")
32767	  END
