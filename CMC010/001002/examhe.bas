100	  ON ERROR GOTO 19000 &
	\ P$="\               \ :" &
	\ P1$="##### " &
	  ! Program name: examhe		Compiled with SCALE 0 on V06.C &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
110	  PRINT "FILE NAME"; &
	\ INPUT LINE F$(1%) &
	\ F%=1% &
	\ F$(1%)=CVT$$(F$(1%),-1%) &
	\ GOTO 32767 IF F$(1%)=""
120	  F$(1%)=F$(1%)+".DAT" IF INSTR(1%,F$(1%),".")=0%
130	  IF RIGHT(F$(1%),LEN(F$(1%)))<>"1" THEN  &
		  F%=2% &
		\ F$(2%)=LEFT(F$(1%),LEN(F$(1%))-1%)+"1"
140	  FOR I%=1% TO F% &
		\ OPEN F$(I%) FOR INPUT AS FILE I%
150		  FIELD #I%, 2% AS R$(1%,I%),2% AS R$(2%,I%),1% AS R$(3%,I%),1% AS R$(4%,I%)
160		  GET #I%, RECORD 1%
170			  NEXT I% &
	\ IF F%=0% THEN  &
		  PRINT "NO FILES PRINTED" &
		\ PRINT  &
		\ GOTO 100
200	  !
210	  PRINT USING P$, ""; &
	\ FOR I%=1% TO F% &
		\ F$="          " &
		\ RSET F$=RIGHT(F$(I%),INSTR(1%,F$(I%),")")+1%) &
		\ PRINT " ";F$;" "; &
	\ NEXT I% &
	\ PRINT 
220	  PRINT  &
	\ I%=1% &
	\ T$="NUMBER OF RECORDS" &
	\ GOSUB 10000
230	  PRINT  &
	\ I%=2% &
	\ T$="FIELD LENGTH" &
	\ GOSUB 10000
240	  PRINT  &
	\ I%=3% &
	\ T$="FILE STATUS" &
	\ GOSUB 10100
250	  PRINT  &
	\ I%=4% &
	\ T$="ADDITIONAL RECORDS" &
	\ GOSUB 10200
260	  PRINT  &
	\ PRINT  &
	\ GOTO 100
10000	  PRINT USING P$, T$; &
	\ PRINT USING " ########## ", CVT$%(R$(I%,J%)); FOR J%=1% TO F% &
	\ RETURN
10100	  PRINT USING P$, T$; &
	\ PRINT USING "          ! ", R$(I%,J%); FOR J%=1% TO F% &
	\ RETURN
10200	  PRINT USING P$, T$; &
	\ PRINT USING " ########## ", ASCII(R$(I%,J%)); FOR J%=1% TO F% &
	\ RETURN
19000	  !
19010	  IF ERL=140% THEN  &
		  PRINT "UNABLE TO OPEN FILE ";F$(I%);" - ";CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),128%+4%) &
		\ F$(1%)=F$(2%) IF I%=1% &
		\ F%=F%-1% &
		\ RESUME 140
19020	  IF ERL=160% THEN  &
		  PRINT "FILE ";F$(I%);" IS EMPTY" &
		\ F%=F%-1% &
		\ F$(1%)=F$(2%) IF I%=1% &
		\ RESUME 140
32767	  END
