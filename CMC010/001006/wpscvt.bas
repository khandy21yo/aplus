10	  ! &
	  ! Program name: wpscvt		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 03:11 AM
30	  ON ERROR GOTO 19000 &
	\ JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ JUNK$=SYS(CHR$(6%)+CHR$(9%)) &
	\ JJ%=ASCII(LEFT(JUNK$,1%))/2% &
	\ JJ$=RIGHT(NUM1$(JJ%+100%),2%)
60	  CH%=1% &
	\ OPEN "KB:" AS FILE 1%, MODE 8%+256%
100	  DEVICE.CH%,PRNT.TMP%=2%
120	  OPEN "PRNT"+JJ$+".TMP" FOR INPUT AS FILE PRNT.TMP% &
	\ NEXT.REPORT%=0% &
	\ CHAIN.PROGRAM$="" &
	\ CHAIN.LINE%=0%
130	  INPUT LINE #PRNT.TMP%, A$ &
	\ PR$=LEFT(A$,2%) &
	\ CMD$=CVT$$(RIGHT(A$,4%),140%) &
	\ IF PR$="RN" THEN  &
		  IF NEXT.REPORT% THEN &
			  GOTO 160 &
		  ELSE &
			  NEXT.REPORT%=-1%
140	  LIN.PER.PAGE%=VAL(CMD$) IF PR$="LP" &
	\ SPAGE%=VAL(CMD$) IF PR$="SP" &
	\ EPAGE%=VAL(CMD$) IF PR$="EP" &
	\ COPIES%=VAL(CMD$) IF PR$="CP" &
	\ REPDATE$=CMD$ IF PR$="RD" &
	\ DISPLAY$=CMD$ IF PR$="DP" &
	\ AUTOSCROLL$=CMD$ IF PR$="AS" &
	\ SPOOLR$=CMD$ IF PR$="SL" &
	\ OUTDEV$=CMD$ IF PR$="OD" &
	\ MS$=CMD$ IF PR$="MS" &
	\ U1$=CMD$ IF PR$="U1" &
	\ U2$=CMD$ IF PR$="U2" &
	\ U3$=CMD$ IF PR$="U3" &
	\ U4$=CMD$ IF PR$="U4" &
	\ U5$=CMD$ IF PR$="U5" &
	\ IF PR$="PC" THEN  &
		  TEMP%=1% &
		\ TEMP%=2% IF LEFT(CMD$,1%)="\" &
		\ PC$=PC$+CHR$(VAL(MID(CMD$,TEMP%+(LOOP%-1%)*4%,3%))) FOR LOOP%=1% TO LEN(CMD$)/4%
150	  GOTO 130
160	  KILL "PRNT"+JJ$+".TMP" &
	\ GOTO 190 IF PR$<>"RN"
170	  OPEN "PRNT"+JJ$+".TMP" FOR OUTPUT AS FILE PRNT.TMP%+1% &
	\ CHAIN.LINE%=31000%
180	  PRINT #PRNT.TMP%+1%, A$; &
	\ INPUT LINE #PRNT.TMP%, A$ &
	\ CHAIN.PROGRAM$=CVT$$(RIGHT(A$,4%),140%) IF LEFT(A$,2%)="PG" AND CHAIN.PROGRAM$="" &
	\ GOTO 180
190	  FILENAME$,DOC$=OUTDEV$
910	  DIM SYSCALL%(30%), IN.CORE%(12%), CC.TBL$(32%), BYTES%(4%), STR%(512%)
1000	  GOSUB 18820 &
	\ TRUE%=-1% &
	\ FALSE%=0% &
	\ DEMOCHN%=FALSE% &
	\ BODMKR%=128% &
	\ NEWMKR%=132% &
	\ CARMKR%=146% &
	\ EODMKR%=164% &
	\ RULEND%=255% &
	\ RULMKR%=162% &
	\ TABEND%=254% &
	\ CUR.PPN%=SWAP%(CVT$%(MID(SYS(CHR$(6%)+CHR$(14%)+STRING$(28%,0%)),7%,2%)))
1140	  INFILE%=2% &
	\ OUTFILE%=3% &
	\ INDEXFILE%=4% &
	\ BFILE%=5% &
	\ WPSTSK%=6% &
	\ IN.CORE%(T1%)=0% FOR T1%=1% TO 12% &
	\ L.PREV%=0% &
	\ L.NEXT%=2% &
	\ OFFSET%=512% &
	\ RULERS%=1% &
	\ PS%=54% &
	\ RULNUM%=1% &
	\ TABB%=9% &
	\ ALLDONE%=0%
1300	  VERSION$="1.2" &
	\ PATCH$="000" &
	\ X.STR$="" &
	\ X.STR$=X.STR$+CHR$(T1%) FOR T1%=0% TO 31% &
	\ X.STR$=X.STR$+"." &
	\ X.STR$=X.STR$+CHR$(T1%) FOR T1%=33% TO 127% &
	\ GET.PRIV$=CHR$(6%)+CHR$(-21%)+CHR$(0%)
1600	  OPEN "NL:" AS FILE BFILE%, RECORDSIZE 2048% &
	\ FIELD #BFILE%, 2048% AS BUF$
1900	  Z$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(-1%))
1995	  !
2000	  DEL.FILE%=0%
2140	  !
2200	  CHANGE SYS(CHR$(6%)+CHR$(-10%)+FILENAME$) TO SYSCALL%
2220	  T1%=SYSCALL%(29%)+SWAP%(SYSCALL%(30%)) &
	\ IF T1%<0% THEN  &
		  DUMMY%=FNCER%(6%) &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;LEFT(FILENAME$,45%);" - ?Bad device specification.";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 17750
2225	  IF (T1% AND 4096%)<>0% AND (STATUS AND 255%)=2% THEN  &
		  DUMMY%=FNCER%(6%) &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;LEFT(FILENAME$,10%);" - ?KB input not allowed.";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 17550
2230	  IF (T1% AND 1%)=0% THEN  &
		  DUMMY%=FNCER%(2%) &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;LEFT(FILENAME$,25%);" - ?File name missing.";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 17550
2240	  IF SYSCALL%(23%)=0% THEN &
		  FILESPEC$="SY:" &
	  ELSE &
		  FILESPEC$=CHR$(SYSCALL%(23%))+CHR$(SYSCALL%(24%)) &
		\ FILESPEC$=FILESPEC$+CHR$(SYSCALL%(25%)+48%) IF SYSCALL%(26%)=255% &
		\ FILESPEC$=FILESPEC$+":"
2260	  T1%=SYSCALL%(5%)+SWAP%(SYSCALL%(6%)) &
	\ IF T1%=0% THEN &
		  FILESPEC$=FILESPEC$+FNC.PPN$(CUR.PPN%) &
	  ELSE &
		  FILESPEC$=FILESPEC$+FNC.PPN$(T1%)
2280	  FILESPEC$=FILESPEC$+CVT$$(RAD$(SYSCALL%(7%)+SWAP%(SYSCALL%(8%)))+RAD$(SYSCALL%(9%)+SWAP%(SYSCALL%(10%)))+"."+RAD$(SYSCALL%(11%)+SWAP%(SYSCALL%(12%))),2%)
2300	  OPEN FILESPEC$ FOR INPUT AS FILE INFILE%
2320	  WIDE.FLAG%=-1%
2400	  !
2440	  GOSUB 10500 &
	\ IF DOC.FLAG%=1% THEN  &
		  IF VAL(DOC$)<1% OR VAL(DOC$)>999% THEN  &
			  DUMMY%=FNCER%(52%) &
			\ PRINT #CH%, FNP$("24;1");CLRLIN$;"?Document numbers must be 1 to 999 only.";FNP$("24;55");"Hit any key to continue"; &
			\ INP$=FNINP$(CH%,128%," ",1%,0%) &
			\ GOTO 17550
2460	  DOC.DEV$=USR.DEV$ UNLESS LEN(DOC.DEV$) &
	\ DOC.PPN%=CUR.PPN% IF DOC.PPN%=0%
2500	  T1%=FNC.OPEN.INDEX.WPS%(INDEXFILE%,DOC.PPN%,0%,DOC.DEV$) &
	\ IF T1%<>0% THEN  &
		  GOTO 2950
2540	  T1%=FNC.RETRIEVE.DOC.DATA%(INDEXFILE%,DOC.FLAG%,DOC$) &
	\ IF T1%<>0% THEN  &
		  DUMMY%=FNCER%(16%) &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;LEFT(DOC$,45%)+FNC.PPN$(DOC.PPN%);" - ?Document already exists.";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 17550
2560	  TABCAR%=32%
2600	  T1%=FNC.CREATE.INDEX.ENTRY%(INDEXFILE%,DOC$,DOC.FLAG%) &
	\ IF T1%<>0% THEN &
		  GOTO 2950 &
	  ELSE &
		  CLOSE INDEXFILE%
2620	  WPSSPEC$=DOC.DEV$+FNC.PPN$(DOC.PPN%)+"WPS"+STRING$(3%-LEN(NUM1$(DOC.NUMBER%)),48%)+NUM1$(DOC.NUMBER%)+".WPS"
2640	  OPEN WPSSPEC$ FOR OUTPUT AS FILE OUTFILE% &
	\ GOSUB 18810 &
	\ GOSUB 10200 &
	\ FIELD #OUTFILE%, 2% AS L.PREV$,2% AS L.NEXT$
2655	  CC.TBL$(0%)="<NUL>" &
	\ CC.TBL$(1%)="<^A>" &
	\ CC.TBL$(2%)="<^B>" &
	\ CC.TBL$(3%)="<^C>" &
	\ CC.TBL$(4%)="<^D>" &
	\ CC.TBL$(5%)="<^E>" &
	\ CC.TBL$(6%)="<^F>" &
	\ CC.TBL$(7%)="<^G>" &
	\ CC.TBL$(8%)="<^H>" &
	\ CC.TBL$(9%)=CHR$(TABCAR%) &
	\ CC.TBL$(10%)=CHR$(CARMKR%) &
	\ CC.TBL$(11%)=CHR$(NEWMKR%) &
	\ CC.TBL$(12%)=CHR$(NEWMKR%) &
	\ CC.TBL$(13%)=CHR$(CARMKR%) &
	\ CC.TBL$(14%)="<^N>" &
	\ CC.TBL$(15%)="<^O>" &
	\ CC.TBL$(16%)="<^P>" &
	\ CC.TBL$(17%)="<^Q>" &
	\ CC.TBL$(18%)="<^R>" &
	\ CC.TBL$(19%)="<^S>" &
	\ CC.TBL$(20%)="<^T>" &
	\ CC.TBL$(21%)="<^U>" &
	\ CC.TBL$(22%)="<^V>" &
	\ CC.TBL$(23%)="<^W>" &
	\ CC.TBL$(24%)="<^X>" &
	\ CC.TBL$(25%)="<^Y>" &
	\ CC.TBL$(26%)=CHR$(EODMKR%) &
	\ CC.TBL$(27%)="<ESC>" &
	\ CC.TBL$(28%)="<^\>" &
	\ CC.TBL$(29%)="<^]>" &
	\ CC.TBL$(30%)="<^^>" &
	\ CC.TBL$(31%)="<^_>"
2700	  STR$=CHR$(BODMKR%) &
	\ GOSUB 10600 &
	\ GOSUB 10300 &
	\ FIELD #INFILE%, 512% AS STR$ &
	\ LSET STR$="" &
	\ GOTO 3000
2800	  GOTO 2810 IF ALLDONE%=1% &
	\ STR$=CHR$(EODMKR%) &
	\ GOSUB 10600
2810	  LSET L.PREV$=CVT%$(SWAP%(L.PREV%)) &
	\ LSET L.NEXT$=CVT%$(0%) &
	\ PUT #OUTFILE%, RECORD L.NEXT%-1%
2820	  GET #OUTFILE%, RECORD 1% &
	\ FIELD #OUTFILE%, 76% AS Z$,51% AS FLENAM$,3% AS VER$,3% AS PAT$,1% AS ENC.FLAG$,2% AS WIDE.FLAG$,368% AS TEMP$,1% AS Z$,1% AS Z1$,2% AS Z2$,4% AS Z3$ &
	\ LSET FLENAM$=DOC$+CHR$(0%) &
	\ LSET FLENAM$=CHR$(0%) IF DOC.FLAG%=1% &
	\ LSET VER$=VERSION$ &
	\ LSET PAT$=PATCH$ &
	\ LSET ENC.FLAG$=CHR$(0%) &
	\ LSET WIDE.FLAG$=CVT%$(SWAP%(WIDE.FLAG%)) &
	\ LSET Z$=CHR$(PS%) &
	\ LSET Z1$=CHR$(253%) &
	\ LSET Z2$=CVT%$(SWAP%(RULNUM%+1%)) &
	\ T1.=(L.PREV%-1%)*508.+OFFSET%-4. &
	\ T2.=0.
2840	  IF T1.>0. THEN  &
		  T1.=T1.-65536. &
		\ T2.=T2.+1. &
		\ GOTO 2840
2860	  BYTES%(1%)=FNC.BYTE.VAL%(T2.,8%,15%) &
	\ BYTES%(2%)=FNC.BYTE.VAL%(-1.,0%,7%) &
	\ BYTES%(3%)=FNC.BYTE.VAL%(T1.,8%,15%) &
	\ BYTES%(4%)=FNC.BYTE.VAL%(-1.,0%,7%) &
	\ LSET Z3$=CHR$(BYTES%(2%))+CHR$(BYTES%(1%))+CHR$(BYTES%(4%))+CHR$(BYTES%(3%)) &
	\ PUT #OUTFILE%, RECORD 1%
2870	  KILL FILENAME$ IF DEL.FILE%
2880	  DUMMY%=FNCER%(0%) &
	\ GOTO 17550
2950	  GOTO 2140
3000	  FIELD #INFILE%, 511% AS STR$,1% AS STR$ &
	\ OVERLAP%=ASCII(STR$)=13% &
	\ GET #INFILE% &
	\ FIELD #INFILE%, 1% AS STR$ &
	\ LSET STR$=CHR$(0%) IF OVERLAP% AND ASCII(STR$)=10% &
	\ FIELD #INFILE%, 512% AS STR$ &
	\ STR$=CVT$$(STR$,1%)
3040	  GOSUB 12000 &
	\ GOSUB 10600 &
	\ GOTO 2800 IF ALLDONE%=1% &
	\ GOTO 3000
10200	  FIELD #OUTFILE%, 512% AS Z$ &
	\ LSET Z$=STRING$(512%,0%)
10290	  RETURN
10300	  OPEN DOC.DEV$+FNC.PPN$(DOC.PPN%)+"WPS.TSK" FOR INPUT AS FILE WPSTSK% &
	\ GET #WPSTSK%, RECORD RULERS% &
	\ FIELD #WPSTSK%, 2% AS TEMP$,3% AS RUL1$,1% AS Z$,3% AS RUL2$,1% AS Z$,41% AS RUL3$ &
	\ Z%=INSTR(1%,RUL3$,CHR$(TABEND%)) &
	\ STR$=CHR$(RULMKR%)+CHR$(RULEND%)+CVT%$(SWAP%(RULNUM%))+CHR$(0%)+CHR$(0%)+CHR$(0%)+CHR$(129%)+CHR$(0%)+CHR$(248%)+LEFT(RUL3$,Z%)+CHR$(RULEND%) &
	\ GOSUB 10600 &
	\ RETURN
10500	  DOC$=CVT$$(DOC$,140%) &
	\ DOC.FLAG%=2% &
	\ DOC.PPN%=0% &
	\ DOC.DEV$=USR.DEV$
10510	  T1%=INSTR(1%,DOC$,"[") &
	\ IF T1%<>0% THEN  &
		  T2%=INSTR(T1%+1%,DOC$,"]") &
		\ IF T2%<>0% THEN  &
			  DOC.PPN%=FNC.CALC.PPN%(MID(DOC$,T1%,T2%-T1%+1%)) &
			\ DOC$=LEFT(DOC$,T1%-1%)+RIGHT(DOC$,T2%+1%)
10520	  IF LEFT(DOC$,1%)="(" THEN  &
		  T1%=INSTR(1%,DOC$,")") &
		\ IF T1%<>0% THEN  &
			  DOC.DEV$=FNDEV$(LEFT(DOC$,T1%)) &
			\ DOC$=RIGHT(DOC$,T1%+1%)
10540	  DOC$=CVT$$(DOC$,136%) &
	\ IF (DOC$<"A" OR DOC$>="[") AND (DOC$<"a" OR DOC$>="{") OR INSTR(2%,DOC$,"[")<>0% OR LEN(DOC$)>50% OR LEN(DOC$)<1% THEN  &
		  DUMMY%=FNCER%(2%) &
		\ PRINT #CH%, "?Invalid document name." &
		\ GOTO 17550
10590	  RETURN
10600	  IF LEN(STR$)+OFFSET%<=512% THEN  &
		  GOTO 10640
10620	  T1%=512%-OFFSET% &
	\ FIELD #OUTFILE%, OFFSET% AS Z$,T1% AS INSERT$ &
	\ LSET INSERT$=LEFT(STR$,T1%) &
	\ LSET L.PREV$=CVT%$(SWAP%(L.PREV%)) &
	\ LSET L.NEXT$=CVT%$(SWAP%(L.NEXT%)) &
	\ PUT #OUTFILE%, RECORD L.NEXT%-1% &
	\ L.PREV%=L.PREV%+1% &
	\ L.NEXT%=L.NEXT%+1% &
	\ OFFSET%=4% &
	\ STR$=RIGHT(STR$,T1%+1%) &
	\ BLK%=BLK%+1% &
	\ GOTO 10600
10640	  FIELD #OUTFILE%, OFFSET% AS Z$,LEN(STR$) AS INSERT$ &
	\ LSET INSERT$=STR$ &
	\ OFFSET%=OFFSET%+LEN(STR$) &
	\ STR$=""
10690	  RETURN
11995	  !
12000	  CHANGE STR$ TO STR% &
	\ LSET BUF$=STR$+"" &
	\ STR.LEN%=LEN(STR$) &
	\ ADDEDCHR%=0%
12020	  FOR POSIT%=1% TO LEN(STR$) &
		\ GOTO 12030 IF STR%(POSIT%)=127% &
		\ GOTO 12080 IF STR%(POSIT%)>=32% &
		\ Z1$=CC.TBL$(STR%(POSIT%)) &
		\ Z1%=LEN(Z1$) &
		\ GOTO 12040 IF STR%(POSIT%)>0% &
		\ Z%=FNC.BUF.REM%(POSIT%+ADDEDCHR%) &
		\ ADDEDCHR%=ADDEDCHR%-1% &
		\ GOTO 12080
12030		  Z%=FNC.BUF.REM%(POSIT%+ADDEDCHR%) &
		\ ADDEDCHR%=ADDEDCHR%-1% &
		\ GOTO 12080
12040		  Z%=FNC.INSP.BUF%(POSIT%+ADDEDCHR%,Z1$,Z1%) &
		\ ADDEDCHR%=ADDEDCHR%+Z1%-1% &
		\ GOTO 12080 IF STR%(POSIT%)<>13% AND STR%(POSIT%)<>10%
12060		  POSIT%=POSIT%+1% &
		\ GOTO 12070 IF POSIT%>LEN(STR$) &
		\ GOTO 12070 IF STR%(POSIT%)<>0% AND STR%(POSIT%)<>10% AND STR%(POSIT%)<>13% &
		\ Z%=FNC.BUF.REM%(POSIT%+ADDEDCHR%) &
		\ ADDEDCHR%=ADDEDCHR%-1% &
		\ POSIT%=POSIT%+1%
12070		  POSIT%=POSIT%-1%
12080		  IF STR%(POSIT%)=26% THEN  &
			  STR.LEN%=POSIT%+ADDEDCHR% &
			\ ALLDONE%=1% &
			\ GOTO 12100
12090			  NEXT POSIT%
12100	  STR$=LEFT(BUF$,STR.LEN%) &
	\ RETURN
17500	  !
17540	  PRINT #CH%, "^Z" IF MAIN.MENU%=FALSE% &
	\ FINISH$="aborted."+CHR$(7%)
17550	  CLOSE CH% &
	\ CLOSE INFILE% &
	\ CLOSE OUTFILE% &
	\ CLOSE INDEXFILE% &
	\ CLOSE BFILE% &
	\ V%=FNX%(CHAIN.PROGRAM$,CHAIN.LINE%,"")
17750	  !
18800	  !
18810	  Z$=SYS(CHR$(6%)+CHR$(-17%)+CHR$(OUTFILE%)+CHR$(203%)+CHR$(25%)+CHR$(148%)+CHR$(146%)) &
	\ RETURN
18820	  Z$=SYS(CHR$(12%)) &
	\ WPS.PPN%=ASCII(RIGHT(Z$,26%)) &
	\ WPS.DEV$="SY" UNLESS WPS.PPN% AND 1% &
	\ WPS.DEV$=MID(Z$,23%,2%) IF WPS.PPN% AND 1% &
	\ WPS.DEV$=WPS.DEV$+NUM1$(ASCII(RIGHT(Z$,25%))) IF (WPS.PPN% AND 3%)=3% &
	\ WPS.DEV$=WPS.DEV$+":" &
	\ WPS.PPN%=CVT$%(RIGHT(Z$,5%)) &
	\ WPS$=WPS.DEV$+"["+NUM1$(WPS.PPN% AND 255%)+","+NUM1$(SWAP%(WPS.PPN%) AND 255%)+"]" &
	\ RETURN
19000	  E.MES$=RIGHT(CVT$$(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),128%),3%) &
	\ RESUME 160 IF ERL=130% &
	\ RESUME 190 IF ERL=170% OR ERL=180%
19005	  IF ERL=120% THEN  &
		  PRINT #CH%, FNP$("24;1");CLRLIN$;"Unable to find print control file.  Aborting";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ RESUME 17500
19010	  IF ERL=2140% AND ERR=11% THEN  &
		  RESUME 17540
19020	  IF ERL=2200% THEN  &
		  DUMMY%=FNCER%(-ERR) &
		\ PRINT #CH%, LEFT(FILENAME$,50%);" - ?Invalid input file specification." &
		\ RESUME 17550
19030	  IF ERL=2300% THEN  &
		  DUMMY%=FNCER%(-ERR) &
		\ PRINT #CH%, FILESPEC$+" - "+E.MES$ &
		\ RESUME 17550
19040	  IF ERL=2400% AND ERR=11% THEN  &
		  RESUME 32740
19050	  IF ERL=2440% THEN  &
		  DUMMY%=FNCER%(-52%) &
		\ PRINT #CH%, LEFT(DOC$,45%);" - ?Invalid document number, range is 1 to 999." &
		\ RESUME 17550
19060	  IF ERL=2640% THEN  &
		  DUMMY%=FNCER%(-ERR) &
		\ PRINT #CH%, WPSSPEC$+" - "+E.MES$ &
		\ RESUME 17550
19070	  IF ERL=2560% AND ERR=11% THEN  &
		  PRINT #CH%, "^Z not allowed here."; &
		\ RESUME 2560
19080	  IF ERL=3000% AND ERR=11% THEN  &
		  RESUME 2800
19090	  IF ERL=10140% THEN  &
		  PRINT "?Command data table missing or invalid." &
		\ RESUME 17550
19100	  IF ERL=24000% AND ERR=2% THEN  &
		  RESUME 24080
19110	  IF ERL=25310% THEN  &
		  DUMMY%=FNCER%(-ERR) &
		\ PRINT #CH%, Q9$+" - ?Invalid account number in specification." &
		\ RESUME 17550
19120	  IF ERL=25720% OR ERL=25740% THEN  &
		  DUMMY%=FNCER%(-ERR) &
		\ PRINT #CH%, "?Index file open error - "+E.MES$ &
		\ RESUME 17550
19130	  IF ERL=29510% THEN  &
		  RESUME 29520
19140	  IF ERR=63% AND (ERL>29000% AND ERL<29200%) THEN  &
		  LSET BUF$=CHR$(EODMKR%) &
		\ STR.LEN%=1% &
		\ GOSUB 10600 &
		\ PRINT #CH%, CHR$(13%)+CHR$(10%);"Block ";BLK%;"has an excess number of control characters.";CHR$(13%)+CHR$(10%);"WPSDAT is aborting." &
		\ RESUME 2800
19150	  IF ERL=29710% THEN  &
		  PRINT #CH%, E.MES$;' while chaining to "';CHN.PRG$;'" at line';CHN.LINE% &
		\ RESUME 17550
19160	  IF ERL=2870% THEN  &
		  RESUME 2880
19999	  PRINT #CH%, E.MES$ &
	\ PRINT #CH%, "Aborting due to fatal error." &
	\ RESUME 17540
24000	  DEF FNDEV$(Q1$) &
	\ Q1$=CVT$$(MID(Q1$,2%,LEN(Q1$)-2%),-1%) &
	\ Q1$=Q1$+":" UNLESS INSTR(1%,Q1$,":") &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+Z$) TO SYSCALL% &
	\ GOTO 24080 IF (SYSCALL%(30%) AND 128%)<>0% OR (STATUS AND 255%)<>0% &
	\ FNDEV$=Q1$ &
	\ GOTO 24090
24080	  DOC$=""
24090	  FNEND
25300	  DEF FNC.CALC.PPN%(Q9$) &
	\ FNC.CALC.PPN%=0%
25310	  CHANGE SYS(CHR$(6%)+CHR$(-10%)+Q9$) TO SYSCALL% &
	\ IF SYSCALL%(5%)+SYSCALL%(6%)=0% THEN &
		  FNC.CALC.PPN%=CUR.PPN% &
	  ELSE &
		  FNC.CALC.PPN%=SYSCALL%(5%)+SWAP%(SYSCALL%(6%))
25320	  FNEND
25500	  DEF FNC.BYTE.VAL%(Q.,Q1%,Q2%) &
	\ T1%=0% &
	\ T2%=7% &
	\ IF Q.>=0. THEN  &
		  BITS.=Q.
25520	  IF BITS.-2.^Q2%>=0. THEN  &
		  BITS.=BITS.-2.^Q2% &
		\ T1%=T1%+2%^T2%
25540	  T2%=T2%-1% &
	\ Q2%=Q2%-1% &
	\ IF Q2%<Q1% THEN &
		  FNC.BYTE.VAL%=T1% &
	  ELSE &
		  GOTO 25520
25590	  FNEND
25700	  DEF FNC.OPEN.INDEX.WPS%(Q1%,Q2%,Q3%,Q4$) &
	\ FNC.OPEN.INDEX.WPS%=0% &
	\ IN.CORE%(Q1%)=0%
25720	  IF Q3%<>1% THEN  &
		  OPEN Q4$+FNC.PPN$(Q2%)+"INDEX.WPS" FOR INPUT AS FILE Q1% &
		\ GOTO 25790 IF Q3%=0% AND (STATUS AND 512%)=0% AND (STATUS AND 1024%)=0% &
		\ GOTO 25790 IF Q3%=2% AND (STATUS AND 1024%)=0%
25740	  IF Q3%=1% THEN  &
		  OPEN Q4$+FNC.PPN$(Q2%)+"INDEX.WPS" FOR INPUT AS FILE Q1%, MODE 8192% &
		\ GOTO 25790 IF Q3%=1% AND (STATUS AND 512%)=0%
25760	  DUMMY%=FNCER%(3%) &
	\ PRINT #CH%, "?Index file in use - cannot update."
25780	  FNC.OPEN.INDEX.WPS%=1%
25790	  FNEND
28700	  DEF FNC.PPN$(Q5%) &
	\ FNC.PPN$="["+NUM1$(SWAP%(Q5%) AND 255%)+","+NUM1$(Q5% AND 255%)+"]" &
	\ FNEND
29000	  DEF FNC.FROM.LOG.REC%(Q%) &
	\ Q%=Q%+1% &
	\ FNC.FROM.LOG.REC%=0% &
	\ PHY.BLK%=(Q%+7%)/8% &
	\ PHY.BLK.OFFSET%=Q%-8%*(PHY.BLK%-1%) &
	\ FNEND
29100	  DEF FNC.INSP.BUF%(Q1%,Q1$,Q2%) &
	\ FNC.INSP.BUF%=0% &
	\ FIELD #BFILE%, Q1%-1% AS X1$,Q2% AS X2$,STR.LEN%-Q1% AS X3$ &
	\ FIELD #BFILE%, Q1%-1% AS Y1$,1% AS TEMP$,STR.LEN%-Q1% AS Y3$ &
	\ RSET X3$=Y3$ &
	\ LSET X2$=Q1$ &
	\ STR.LEN%=STR.LEN%+Q2%-1% &
	\ FNEND
29200	  DEF FNC.BUF.REM%(Q1%) &
	\ FNC.BUF.REM%=0% &
	\ FIELD #BFILE%, Q1%-1% AS X1$,STR.LEN%-Q1% AS X2$,1% AS X3$ &
	\ FIELD #BFILE%, Q1%-1% AS Y1$,1% AS TEMP$,STR.LEN%-Q1% AS Y2$ &
	\ LSET X2$=Y2$ &
	\ LSET X3$=CHR$(32%) &
	\ STR.LEN%=STR.LEN%-1% &
	\ FNEND
29300	  DEF FNC.RETRIEVE.DOC.DATA%(Q1%,Q2%,Q3$) &
	\ FNC.RETRIEVE.DOC.DATA%=0% &
	\ GET #Q1%, RECORD 1% UNLESS IN.CORE%(Q1%)=1% &
	\ IN.CORE%(Q1%)=1% &
	\ FIELD #Q1%, 2% AS Z$ &
	\ IF CVT$%(Z$)=0% THEN &
		  GOTO 29390 &
	  ELSE &
		  LOG.REC%=CVT$%(Z$)
29330	  T1%=FNC.FROM.LOG.REC%(LOG.REC%) &
	\ GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK% &
	\ IN.CORE%(Q1%)=PHY.BLK% &
	\ FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$,2% AS Z1$,60% AS Z2$ &
	\ T1$=CVT$$(LEFT(Z2$,INSTR(1%,Z2$,CHR$(0%))-1%),32%)
29340	  IF Q2%=1% AND Q3$=NUM1$(CVT$%(Z1$)) OR Q2%=2% AND CVT$$(Q3$,32%)=CVT$$(T1$,32%) THEN  &
		  GOTO 29380
29350	  IF CVT$%(Z$)=0% THEN &
		  GOTO 29390 &
	  ELSE &
		  LOG.REC%=CVT$%(Z$) &
		\ GOTO 29330
29380	  FNC.RETRIEVE.DOC.DATA%=LOG.REC% &
	\ IF Q2%=2% THEN  &
		  T1$=NUM1$(CVT$%(Z1$))
29390	  FNEND
29400	  DEF FNC.CREATE.INDEX.ENTRY%(Q1%,Q2$,Q3%) &
	\ FNC.CREATE.INDEX.ENTRY%=0%
29404	  GET #Q1%, RECORD 1% UNLESS IN.CORE%(Q1%)=1% &
	\ IN.CORE%(Q1%)=1% &
	\ FIELD #Q1%, 2% AS Z$,2% AS Z1$,2% AS Z2$ &
	\ HDR.PTR%=CVT$%(Z$) &
	\ HDR.FREE.PTR%=CVT$%(Z1$) &
	\ HDR.MAX.NUM%=CVT$%(Z2$)
29410	  IF Q3%=1% THEN  &
		  GOTO 29480
29420	  IF HDR.FREE.PTR%=0% THEN  &
		  GOTO 29450
29430	  T1%=FNC.FROM.LOG.REC%(HDR.FREE.PTR%) &
	\ GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK% &
	\ IN.CORE%(Q1%)=PHY.BLK% &
	\ FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$,2% AS Z1$,60% AS Z2$ &
	\ SAVE.PTR%=CVT$%(Z$) &
	\ LSET Z$=CVT%$(HDR.PTR%) &
	\ LSET Z1$=CVT%$(HDR.FREE.PTR%) &
	\ LSET Z2$=Q2$+CHR$(0%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%)
29440	  GET #Q1%, RECORD 1% UNLESS IN.CORE%(Q1%)=1% &
	\ IN.CORE%(Q1%)=1% &
	\ FIELD #Q1%, 2% AS Z$,2% AS Z1$,2% AS Z2$ &
	\ LSET Z$=CVT%$(HDR.FREE.PTR%) &
	\ LSET Z1$=CVT%$(SAVE.PTR%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%) &
	\ DOC.NUMBER%=HDR.FREE.PTR% &
	\ GOTO 29690
29450	  LOG.REC%=HDR.MAX.NUM% &
	\ LSET Z$=CVT%$(LOG.REC%) &
	\ LSET Z2$=CVT%$(HDR.MAX.NUM%+1%) &
	\ IF HDR.MAX.NUM%<1000% THEN &
		  PUT #Q1%, RECORD IN.CORE%(Q1%) &
	  ELSE &
		  PRINT #CH%, "?There are 999 documents in this ";"account, no more can be created." &
		\ GOTO 29650
29460	  T1%=FNC.FROM.LOG.REC%(LOG.REC%) &
	\ IF PHY.BLK.OFFSET%>1% THEN  &
		  GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK%
29470	  IN.CORE%(Q1%)=PHY.BLK% &
	\ FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$,2% AS Z1$,60% AS Z2$ &
	\ LSET Z$=CVT%$(HDR.PTR%) &
	\ LSET Z1$=CVT%$(LOG.REC%) &
	\ LSET Z2$=Q2$+CHR$(0%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%) &
	\ DOC.NUMBER%=LOG.REC% &
	\ GOTO 29690
29480	  DOC.NUMBER%=VAL(Q2$)
29490	  Q2$=CHR$(0%) &
	\ IF DOC.NUMBER%<HDR.MAX.NUM% THEN  &
		  GOTO 29550
29500	  LSET Z1$=CVT%$(DOC.NUMBER%) &
	\ LSET Z2$=CVT%$(DOC.NUMBER%+1%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%) &
	\ CNT1%=DOC.NUMBER%
29510	  T1%=FNC.FROM.LOG.REC%(CNT1%) &
	\ GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK%
29520	  IN.CORE%(Q1%)=PHY.BLK% &
	\ FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$ &
	\ IF CNT1%=HDR.MAX.NUM% THEN &
		  LSET Z$=CVT%$(HDR.FREE.PTR%) &
	  ELSE &
		  LSET Z$=CVT%$(CNT1%-1%)
29530	  PUT #Q1%, RECORD IN.CORE%(Q1%) &
	\ IF CNT1%>HDR.MAX.NUM% THEN  &
		  CNT1%=CNT1%-1% &
		\ GOTO 29510
29540	  HDR.FREE.PTR%=DOC.NUMBER% &
	\ GOTO 29430
29550	  IF DOC.NUMBER%=HDR.FREE.PTR% THEN  &
		  GOTO 29430
29560	  LSET Z$=CVT%$(DOC.NUMBER%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%)
29570	  T1%=FNC.FROM.LOG.REC%(DOC.NUMBER%) &
	\ GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK% &
	\ IN.CORE%(Q1%)=PHY.BLK%
29580	  FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$,2% AS Z1$,60% AS Z2$ &
	\ SAVE.PTR%=CVT$%(Z$) &
	\ LSET Z$=CVT%$(HDR.PTR%) &
	\ LSET Z1$=CVT%$(DOC.NUMBER%) &
	\ LSET Z2$=CHR$(0%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%)
29590	  T2%=HDR.FREE.PTR%
29600	  T1%=FNC.FROM.LOG.REC%(T2%) &
	\ GET #Q1%, RECORD PHY.BLK% UNLESS IN.CORE%(Q1%)=PHY.BLK% &
	\ IN.CORE%(Q1%)=PHY.BLK% &
	\ FIELD #Q1%, (PHY.BLK.OFFSET%-1%)*64% AS Z$,2% AS Z$ &
	\ IF CVT$%(Z$)=DOC.NUMBER% THEN &
		  GOTO 29610 &
	  ELSE &
		  T2%=CVT$%(Z$) &
		\ GOTO 29600
29610	  LSET Z$=CVT%$(SAVE.PTR%) &
	\ PUT #Q1%, RECORD IN.CORE%(Q1%) &
	\ GOTO 29690
29650	  FNC.CREATE.INDEX.ENTRY%=1%
29690	  FNEND
29700	  DEF FNCER%(COD%) &
	\ GOTO 29720 IF LEN(CHN.PRG$)=0% OR MAIN.MENU%=TRUE% &
	\ DUMMY$=SYS(CHR$(8%)+CHR$(ABS(COD%))+CHR$(0%)+CVT%$(DOC.NUMBER%)) &
	\ IF COD%<0% THEN  &
		  RESUME 29710
29710	  CHAIN CHN.PRG$ CHN.LINE% &
	\ STOP
29720	  FNEND
30000	  DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%) &
	\ PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$));STRING$(INPUTLEN%,8%); &
	\ PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$; &
	\ GET #CHN% &
	\ FIELD #CHN%, RECOUNT AS BUFFER$ &
	\ BUFFER$="%^C" IF INSTR(1%,BUFFER$,CHR$(3%)) &
	\ FNINP$=CVT$$(BUFFER$,4%) &
	\ V=SQR(-1.) IF BUFFER$="%^C" AND TO.ERR% &
	\ FNEND
30200	  DEF FNP$(ROWCOL$) &
	\ FNP$=ESC$+"["+ROWCOL$+"H" &
	\ FNEND
30250	  DEF FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
	\ MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+DESC$ &
	\ MESG$=DESC$+"" IF ERRNUM%<1% &
	\ IF PRINT.TEST% THEN  &
		  PRINT #CHN%, FNP$("24;1");CLRLIN$;MESG$;FNP$("24;"+NUM1$(PRINT.WIDTH%-25%))+"Hit any key to continue"; &
		\ NW$=FNINP$(CHN%,128%," ",1%,TO.ERR%) &
		\ PRINT #CHN%, CLRBOT$;
30260	  FNMESS$=MESG$ &
	\ FNEND
30280	  DEF FNSR$(BEGEND$) &
	\ FNSR$=ESC$+"["+BEGEND$+"r" &
	\ FNEND
30900	  !
30910	  DEF FNX%(C$,C%,C1$) &
	\ Q0$=SYS(CHR$(7%)) &
	\ Q5%=INSTR(1%,Q0$,CHR$(255%)) &
	\ Q2%=INSTR(1%,Q0$,CHR$(14%)) &
	\ Q2%=Q5%+12% IF Q2%=0% &
	\ Q0$=MID(Q0$+SPACE$(12%),Q5%,Q2%-Q5%) IF Q5% &
	\ Q0$="" IF Q5%=0% &
	\ CLOSE Q1% FOR Q1%=1% TO 12% &
	\ IF C%>=0% THEN  &
		  IF Q9$<>"" THEN  &
			  Q3$=CVT%$(C%)+C$ IF C$<>"" &
			\ Q3$=CVT%$(8100%)+"!MENU.BAC" IF C$="" AND Q5% &
			\ Q3$=Q3$+"  " &
			\ Q9$=Q9$+CHR$(13%)+CHR$(13%)+Q0$ &
			\ Q9$=Q9$+CHR$(14%)+C1$ IF C1$<>"" &
			\ Q$=LEFT(CHR$(LEN(Q3$))+Q3$+Q9$,127%) &
			\ Q$=SYS(CHR$(8%)+Q$) &
			\ CHAIN "ISM:FSTSRS.TSK" 0%
30920	  CLOSE Q1% FOR Q1%=1% TO 12% IF C%<0% &
	\ C%=-C% IF C%<0% &
	\ CHAIN "ISM:MENU" 0. IF Q5% AND C$="" &
	\ V$=SYS(CHR$(8%)+Q0$+"   "+CHR$(14%)+C1$) &
	\ CHAIN C$ C% IF C$<>"" &
	\ GOTO 32767 &
	\ FNEND
30930	  DEF FNX$ &
	\ Q$=SYS(CHR$(7%)) &
	\ Q1%=INSTR(4%,Q$,CHR$(14%)) &
	\ Q$=RIGHT(Q$,Q1%+1%) &
	\ FNX$=Q$ &
	\ FNEND
31000	  GOTO 10
32740	  !
32767	  END
