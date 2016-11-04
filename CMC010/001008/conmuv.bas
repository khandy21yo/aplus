10	! &
	! MSTREV.BAS - To update payroll amounts from the check data files &
	! &
	! Written by - Mike Feinauer 1981 &
	!
20 EXTEND
30 DIM ACC$(15%),CODE$(15%)
200	! &
	! Open null device for fielding &
	!
210 OPEN "KB:" AS FILE 2%,RECORDSIZE 64% &
	\ OPEN "NL:" AS FILE 1%,RECORDSIZE 512%+64% &
	\ FIELD #1%,512% AS G$,6% AS CHECK$(1%),8% AS CHECK$(2%), &
		    2% AS CHECK$(3%),8% AS CHECK$(4%),2% AS CHECK$(5%), &
		    2% AS CHECK$(6%),2% AS CHECK$(7%),28% AS CHECK$(8%), &
		    6% AS CHECK$(9%) &
	\ FIELD #1%,512% AS MASTER$,64% AS CHECK$
400	! &
	! Read unique file to find necessary codes &
	!
410 ON ERROR GOTO 499
420 OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ DIM #12%,A0$(255%)=64%
430 STATE$(X%),DED$(X%),DED.ACC$(X%),STATE.ACC$(X%)="" FOR X%=1% TO 10% &
	\ ACC$(X%)="" FOR X%=1% TO 15% &
	\ CODE$(1%)="PAE" \ CODE$(2%)="FED" &
	\ CODE$(3%)="FIC" \ CODE$(4%)="VAC" &
	\ CODE$(5%)="INS" \ CODE$(6%)="DED" &
	\ CODE$(7%)="PA1" \ CODE$(8%)="PA2" &
	\ CODE$(9%)="PA3" \ CODE$(10%)="PA4" &
	\ CODE$(11%)="PA5" \ CODE$(12%)="PA6" &
	\ CODE$(13%)="PA7" \ CODE$(14%)="PA8" &
	\ CODE$(15%)="PA9" \ CODES$=CVT$$(A0$(22%)+A0$(27%)+A0$(28%),-1%) &
	\ FOR SEARCH%=1% TO 15% &
	\ FIND%=INSTR(1%,CODES$,CODE$(SEARCH%)) &
	\ ACC$(SEARCH%)=MID(CODES$,FIND%+4%,6%) IF FIND% &
	\ NEXT SEARCH% &
	\ FIND%,STATE%,DED%=0%
440 FIND%=INSTR(FIND%+1%,CODES$,"S/") &
	\ IF FIND% THEN STATE%=STATE%+1% &
	\ STATE$(STATE%)=MID(CODES$,FIND%+2%,2%) &
	\ STATE.ACC$(STATE%)=MID(CODES$,FIND%+5%,6%) &
	\ GOTO 440
445 UNLESS STATE% THEN PRINT &
	  "You must define state tax codes in UNIQUE file !!" &
	\ PRINT "Call CMC if you need help !!"+STRING$(3%,7%) &
	\ GOTO 10000
450 FIND%=0%
460 FIND%=INSTR(FIND%+1%,CODES$,"D/") &
	\ IF FIND% THEN DED%=DED%+1% &
	\ DED$(DED%)=MID(CODES$,FIND%+2%,2%) &
	\ DED.ACC$(DED%)=MID(CODES$,FIND%+5%,6%) &
	\ GOTO 460
480 FOR ACC%=1% TO 15% &
	\ GOTO 500 IF ACC$(ACC%)<>"" &
	\ NEXT ACC% &
	\ PRINT "You must define codes on lines 21,27 & 28 of your " &
	\ PRINT "UNIQUE file. Call CMC for help !!"+STRING$(3%,7%) &
	\ GOTO 10000
499 PRINT "You must create the UNIQUE file first!!"+STRING$(3%,7%) &
	\ PRINT "Call CMC if you need help !!"+STRING$(3%,7%) &
	\ PRINT &
	\ GOTO 10000
500 ACCOUNTS$="!!!!!" &
	\ ACCOUNTS$=ACCOUNTS$+ACC$(X%)+"!" IF ACC$(X%)<>"" FOR X%=1% TO 15%
600 PRINT "NOTE: THE DESCRIPTION FIELD OF CKDATA SHOULD BE USED" &
	\ PRINT "FOR AN EMPLOYEE CODE (6 CHAR OR LESS) AND" &
	\ PRINT "THE NAME OF THE EMPLOYEE IF DESIRED" &
	\ PRINT "SEPARATE THE CODE AND THE NAME WITH A SPACE OR" &
	\ PRINT "AS ASTRIX.  EXAMPLE" &
	\ PRINT "ADAJ JIM ADAMS" &
	\ PRINT "ADAJ*JIM ADAMS" &
	\ PRINT "THE CODE IS REQUIRED BUT THE NAME IS OPTIONAL" &
	\ PRINT "THE FOLLOWING ACCOUNTS WILL BE USED:" &
	\ FOR X%=1% TO 15% &
	\ PRINT ACC$(X%),CODE$(X%) IF ACC$(X%)<>"" &
	\ NEXT X%
610 PRINT "STATE DEDUCTIONS" &
	\ FOR X%=1% TO STATE% &
	\ PRINT STATE.ACC$(X%),STATE$(X%) &
	\ ACCOUNTS$=ACCOUNTS$+STATE.ACC$(X%)+"!" &
	\ NEXT X%
620 PRINT "MISC. DEDUCTIONS" &
	\ FOR X%=1% TO DED% &
	\ PRINT DED.ACC$(X%),DED$(X%) &
	\ ACCOUNTS$=ACCOUNTS$+DED.ACC$(X%)+"!" &
	\ NEXT X%
1000	! &
	! Options &
	!
1020 INPUT "Option ";OPT$ &
	\ OPT$=LEFT(OPT$,3%) &
	\ GOTO 2000 IF OPT$="REV" &
	\ GOTO 10000 IF OPT$="END"
1040 PRINT "Options are :" &
	\ PRINT "	REV - Reverse entries from ckdata files" &
	\ PRINT "	END - End program" &
	\ GOTO 1020
2000	! &
	! Check for validity of check data files and create a new &
	! master file if one is not found &
	!
2010 MONTH%,TOT.MONTHS%=0% &
	\ MONTHS$,MONTH$="" &
	\ PRINT "Months to update (JAN,FEB,ETC.) => "; &
	\ INPUT LINE #2%,MONTHS$ &
	\ MONTHS$=CVT$$(MONTHS$,-1%) &
	\ GOTO 2010 IF MONTHS$="" &
	\ UNTIL MONTHS$="" &
	\ COMMA%=INSTR(1%,MONTHS$,",") &
	\ COMMA%=LEN(MONTHS$)+1% UNLESS COMMA% &
	\ TOT.MONTHS%=TOT.MONTHS%+1% &
	\ MONTH$(TOT.MONTHS%)=LEFT(MONTHS$,COMMA%-1%) &
	\ MONTHS$=RIGHT(MONTHS$,COMMA%+1%) &
	\ NEXT
2020 FOR MONTH%=1% TO TOT.MONTHS% &
	\ FILE$(MONTH%)="CK"+MONTH$(MONTH%)+".DAT" &
	\ V%=FNC%(6%) &
	\ IF FNO%(6%,FILE$(MONTH%),"/RO","") &
	  THEN FILE$(MONTH%)="CK"+MONTH$(MONTH%)+"."+ &
	  RIGHT(DATE$(0%),8%)+"T" &
	\ IF FNO%(6%,FILE$(MONTH%),"/RO","") &
	  THEN PRINT "File - ";FILE$(month%);" is not found !!"+STRING$(3%,7%) &
	\ PRINT "Since I have not begun any processing re-enter all months" &
	\ PRINT "to be updated!" &
	\ GOTO 2010
2030 NEXT MONTH% &
	\ INPUT "Verify updating ";VERIFY$ &
	\ GOTO 1020 UNLESS LEFT(VERIFY$,1%)="Y"
2040 V%=FNO%(4%,"MSTRFL.DAT","","") &
	\ IF V%=5% THEN INPUT "Verify creating a master file ";VERIFY$ &
	\ GOTO 1020 UNLESS LEFT(VERIFY$,1%)="Y" &
	\ V%=FNO%(4%,"MSTRFL.DAT","/CR:8,512","")
2050 IF V% THEN PRINT "Error has occurred in opening master file - "; &
	  SYS(CHR$(6%)+CHR$(9%)+CHR$(FNS%)) &
	\ PRINT "Call CMC if you need help!!"+STRING$(3%,7%) &
	\ GOTO 10000
3000	! &
	! Make updates to the master file &
	!
3010 FOR MONTH%=1% TO TOT.MONTHS% &
	\ QTR%=INSTR(1%,"!!!JAN!FEB!MAR!APR!MAY!JUN!JUL!AUG!SEP!OCT!NOV!DEC", &
	  MONTH$(MONTH%))/4% &
	\ IF QTR%<4% THEN QTR%=1% ELSE &
	  IF QTR%<7% THEN QTR%=2% ELSE &
	  IF QTR%<10% THEN QTR%=3% ELSE &
	  IF QTR%>9%  THEN QTR%=4%
3020 V%=FNC%(6%) &
	\ STOP IF FNO%(6%,FILE$(MONTH%),"/RO","") &
	\ STOP IF FNG%(6%,"") &
	\ ZERO$=STRING$(8%,0%)+SPACE$(2%) &
	\ ZERO$=ZERO$+ZERO$+ZERO$+ZERO$+ZERO$+ZERO$+ZERO$+ZERO$+ZERO$
3030 LSET CHECK$=FNL$ &

3032 ACC.NO$=CVT$$(CHECK$(2%),-1%) &
	\ GOTO 3150 IF INSTR(1%,ACCOUNTS$,ACC.NO$)=0% &
	\ CKNUM$=CHECK$(1%)+"" &
	\ NET,AMOUNT,OLD.,NEW.=0. &
	\ LSET CHECK$(8%)=RIGHT(CHECK$(8%),11%) &
		IF LEFT(CHECK$(8%),3%)="JE#" &
	\ PERSON$=CVT$$(CHECK$(8%),-1%) &
	\ SPACE%=INSTR(1%,PERSON$,"*") &
	\ SPACE%=INSTR(1%,PERSON$," ") IF SPACE%<2% &
	\ GOTO 3037 IF SPACE%<2% OR SPACE%>6%
3035 KEYY$=LEFT(PERSON$,SPACE%-1%) &
	\ PERSON$=RIGHT(PERSON$,SPACE%+1%) &
	\ V%=FNG%(4%,KEYY$) &
	\ GOTO 3040 UNLESS V%=88%
3037 PRINT "NOTE: NON PAYROLL ITEM - "; &
	\ PRINT "ENTER IN PAYROLL MASTER FILE IF IT IS PAYROLL" &
	\ PRINT KEYY$;" ";PERSON$;" CHECK #:";CHECK$(1%);" ACC. #:";CHECK$(2%); &
	\ PRINT "$"; \ PRINT USING "####.##",CVT$F(CHECK$(4%)) &
	\ GOTO 3150 &
!	  LSET MASTER$=KEYY$+SPACE$(6%-LEN(KEYY$))+ &
!	  PERSON$+SPACE$(30%-LEN(PERSON$))+STRING$(126%,32%)+ZERO$+ &
!	  SPACE$(16%)+STRING$(512%,0%) &
!	\ STOP IF FNA%(4%,MASTER$) &
!	\ V%=FNG%(4%,KEYY$) &
!	\ PRINT "NOTE: SETTING UP NEW EMPLOYEE ";KEYY$,PERSON$
3040 STOP IF V% &
	\ LSET MASTER$=FNL$
3050 UNTIL LEFT(CHECK$(8%),LEN(KEYY$))<>KEYY$ &
	\ AMOUNT=-CVT$F(CHECK$(4%)) &
	\ FOR CODE%=0% TO 15% &
	\ GOTO 3055 IF ACC$(CODE%)=ACC.NO$ &
	\ NEXT CODE% &
	\ GOTO 3110
3055 GOTO 3110 IF CODE%=0% &
	\ IF CODE%=1% OR CODE%>6% &
	  THEN OLD.=CVT$F(MID(MASTER$,269%,8%)) &
	\ NEW.=OLD.+AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,268%)+CVTF$(NEW.)+RIGHT(MASTER$,277%) &
	\ OLD.=CVT$F(MID(MASTER$,276%+(QTR%-1%)*8%+1%,8%)) &
	\ NEW.=OLD.+AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,276%+(QTR%-1%)*8%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,276%+(QTR%-1%)*8%+9%) &
	\ GOTO 3130 &
		! Update earning totals
3060 IF CODE%=2% THEN OLD.=CVT$F(MID(MASTER$,317%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,316%)+CVTF$(NEW.)+RIGHT(MASTER$,325%) &
	\ OLD.=CVT$F(MID(MASTER$,324%+(QTR%-1%)*8%+1%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,324%+(QTR%-1%)*8%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,324%+(QTR%-1%)*8%+9%) &
	\ GOTO 3130 &
		! Update Federal tax totals
3070 IF CODE%=3% THEN OLD.=CVT$F(MID(MASTER$,357%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,356%)+CVTF$(NEW.)+RIGHT(MASTER$,365%) &
	\ OLD.=CVT$F(MID(MASTER$,364%+(QTR%-1%)*8%+1%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,364%+(QTR%-1%)*8%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,364%+(QTR%-1%)*8%+9%) &
	\ STOP IF FNU%(4%,MASTER$) &
	\ GOTO 3130 &
		! Update Fica tax totals
3080 IF CODE%=4% THEN OLD.=CVT$F(MID(MASTER$,437%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,436%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,445%) &
	\ GOTO 3130 &
		! Update Vacation totals
3090 IF CODE%=5% THEN OLD.=CVT$F(MID(MASTER$,445%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,444%)+CVTF$(NEW.)+RIGHT(MASTER$,453%) &
	\ GOTO 3130 &
		! Update Insurance totals
3100 IF CODE%=6% THEN GOTO 3130
3110 FOR ST%=1% TO STATE% &
	\ IF ACC.NO$=STATE.ACC$(ST%) THEN OLD.=CVT$F(MID(MASTER$,397%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,396%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,405%) &
	\ OLD.=CVT$F(MID(MASTER$,404%+(QTR%-1%)*8%+1%,8%)) &
	\ NEW.=OLD.-AMOUNT &
	\ LSET MASTER$=LEFT(MASTER$,404%+(QTR%-1%)*8%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,404%+(QTR%-1%)*8%+9%) &
	\ GOTO 3130
3120 NEXT ST%
3125 FOR D%=1% TO DED% &
	\ GOTO 3130 IF DED.ACC$(D%)=ACC.NO$ &
	\ NEXT D% &
	\ GOTO 3135
3130 NET=NET+AMOUNT &
	\ STOP IF FNU%(4%,MASTER$)
3135 V%=FNN%(6%) &
	\ GOTO 3140 IF V% &
	\ LSET CHECK$=FNL$ &
	\ LSET CHECK$(8%)=RIGHT(CHECK$(8%),11%) &
		IF LEFT(CHECK$(8%),3%)="JE#" &
	\ ACC.NO$=CVT$$(CHECK$(2%),-1%) &
	\ NEXT
3140 OLD.=CVT$F(MID(MASTER$,477%,8%)) &
	\ NEW.=OLD.+NET &
	\ LSET MASTER$=LEFT(MASTER$,476%)+CVTF$(NEW.)+ &
	  RIGHT(MASTER$,485%) &
	\ STOP IF FNU%(4%,MASTER$) &
	\ PRINT IF POS(0%)>75% &
	\ PRINT "!"; &
	\ PRINT CKNUM$;" "; \ PRINT USING "###,###.##",NET &
	\ GOTO 3160 IF V% &
	\ GOTO 3032
3150 V%=FNN%(6%) &
	\ GOTO 3030 UNLESS V%
3160 NEXT MONTH% &
	\ PRINT &
	\ PRINT "Update Complete !!"+STRING$(3%,7%) &
	\ PRINT &
	\ V%=FNX%("",0%,"") &
	\ GOTO 1020
10000	! &
	! Close files, end program &
	!
10100 V%=FNX%("",0%,"")
32767 END
