10	  ! &
	  ! Program name: payreg		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  DIM M$(12%), L$(10%), L1$(10%), L(4%), U$(6%), W$(6%), P0$(10%,3%)
100	  !
105	  IF FNO%(11%,"VENDOR.DAT","/RO","") THEN  &
		  PRINT "VENDOR.DAT CLOSED." &
		\ GOTO 10000
110	  IF FNO%(7%,"PAHOLD.DAT","/RW","") THEN  &
		  PRINT "HOLDING FILE DOES NOT EXIST -- CREATE WITH PAHOLD" &
		\ GOTO 10000
120	  OPEN "KB:" AS FILE 9%
130	  ON ERROR GOTO 140 &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 4% &
	\ DIM #4%, A0$(255%)=64% &
	\ GOTO 150
140	  PRINT "USE !UNIQUE TO BUILD THE NECESSARY FILE." &
	\ RESUME 10000
150	  ON ERROR GOTO 0
160	  V%=FNO%(2%,"PAYABL.DAT","/RW","") &
	\ IF V%=0% THEN &
		  GOTO 190 &
	  ELSE &
		  PRINT "UNABLE TO OPEN PAYABL.DAT IF V%<>5% &
		\ IF V%<>5% THEN &
			  GOTO 10000 &
		  ELSE &
			  PRINT "PAYABLE FILE DOES NOT YET EXIST !!!" &
			\ INPUT "DO YOU WANT TO CREATE A FILE ";K$ &
			\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
			\ IF FNO%(2%,"PAYABL.DAT","/CR:21,64/RW","") THEN  &
				  STOP
190	  P9$=CVT$$(A0$(40%),-1%) &
	\ P8$=CVT$$(A0$(41%),-1%) &
	\ GOTO 200 IF P9$="" AND P8$="" &
	\ IF P8$<>"" THEN  &
		  P9$=P9$+","+P8$
192	  GOTO 200 IF P9$="" &
	\ I%=INSTR(1%,P9$,",") &
	\ I%=LEN(P9$)+1% IF I%=0% &
	\ P3$=LEFT(P9$,I%-1%) &
	\ P9$=RIGHT(P9$,I%+1%)
194	  P9%=P9%+1% &
	\ E%=INSTR(1%,P3$,"=") &
	\ X%=INSTR(1%,P3$,"/") &
	\ P0$(P9%,0%)=LEFT(P3$,E%-1%) &
	\ P0$(P9%,1%)=MID(P3$,E%+1%,X%-E%-1%) &
	\ P0$(P9%,2%)=RIGHT(P3$,X%+1%) &
	\ GOTO 192
200	  !
210	  OPEN "KB:" AS FILE 1%, RECORDSIZE 128% &
	\ FIELD #1%, 6% AS M$(1%),13% AS M$(2%),6% AS M$(3%),3% AS M$(4%),3% AS M$(12%),1% AS M$(5%),6% AS M$(6%),6% AS M$(7%),8% AS M$(8%),8% AS M$(9%),2% AS M$(10%),2% AS M$(11%) &
	\ FIELD #1%, 64% AS T$
220	  OPEN "NL:" AS FILE 10%, RECORDSIZE 192% &
	\ FIELD #10%, 6% AS U$(1%),29% AS U$(2%),29% AS U$(3%),29% AS U$(4%),29% AS U$(5%),5% AS U$(6%),1% AS E$ &
	\ FIELD #10%, 128% AS T3$
300	  !
330	  B$="       \         \ #,###,###.##" &
	\ B1$=" ###.## ###,###,###.##"
340	  FIELD #10%, 128% AS T5$,15% AS L1$(1%),4% AS L1$(2%),10% AS L1$(3%),1% AS L1$(4%),2% AS L1$(5%),6% AS L1$(6%),6% AS L1$(7%),6% AS L1$(8%),6% AS L1$(9%),8% AS L1$(10%) &
	\ FIELD #10%, 128% AS T5$,64% AS T5$
1000	  !
1020	  PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 5500 IF K$="UPD" &
	\ GOTO 6000 IF K$="EXA" &
	\ GOTO 8500 IF K$="TRA" &
	\ GOTO 7500 IF K$="INV" &
	\ GOTO 12000 IF K$="REG" &
	\ GOTO 10000 IF K$="END"
1030	  PRINT  &
	\ PRINT "OPTIONS ARE:" &
	\ PRINT "   EXAMINE  -  EXAMINE VENDOR TRANSACTIONS" &
	\ PRINT "   INVOICE  -  INVOICE ATTACHMENT (AFTER CHECKS BEFORE TRANS)" &
	\ PRINT "   REGISTER -  REGISTER PRINTED" &
	\ PRINT "   UPDATE   -  UPDATE FROM THE HOLDING FILE" &
	\ PRINT "   TRANSFER -  TRANSFER PAID RECORDS TO THE CHECK REGISTER" &
	\ PRINT "   END      -  END THE PROGRAM" &
	\ GOTO 1020
2100	  !
2110	  LSET T$=FNL$ &
	\ L$(I%)=M$(I%)+"" FOR I%=1% TO 7% &
	\ L(I%)=CVT$F(M$(I%+7%)) FOR I%=1% TO 2% &
	\ L$(I%)=FND9$(CVT$%(M$(I%+2%))) FOR I%=8% TO 9% &
	\ L$(4%)=FNA7$(M$(4%)) &
	\ L$(10%)=FNA7$(M$(12%)) &
	\ RETURN
2200	  !
2210	  PRINT USING "\    \ \           \ \    \###,###.## ####.## "+"###,###.## \\ \      \\    \\    \", L$(1%),L$(2%),L$(7%),L(1%),L(2%),L(1%)-L(2%),L$(5%),L$(4%),L$(6%),L$(3%) &
	\ T=T+L(1%) &
	\ T1=T1+L(2%) &
	\ T2=T2+L(1%)-L(2%) &
	\ X1%=X1%+1% &
	\ T0%=T0%+1% &
	\ RETURN
2300	  !
2310	  PRINT "A log file does not yet exist. Shall I create one (Y or N)"; &
	\ INPUT K$ &
	\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
	\ V%=FNO%(7%,"PLOG.DAT","/CR:32,64","") &
	\ IF V%<>0% THEN  &
		  PRINT "Error";V%;"in opening new PLOG.DAT" &
		\ GOTO 10000
2340	  PRINT "CREATED!" &
	\ RETURN
2400	  !
2410	  X%=0% &
	\ V%=FNC%(5%) &
	\ FIELD #1%, 64% AS E$,6% AS P$,8% AS P1$,2% AS P2$,8% AS P3$,2% AS P4$,2% AS P5$,2% AS P6$,28% AS P7$,6% AS P8$ &
	\ FIELD #1%, 64% AS E$,64% AS T1$
2420	  INPUT "ENTER MONTH FOR CHECK FILE NAME ";A$ &
	\ C$="CK"+LEFT(A$,3%)+" .DAT" &
	\ IF FNO%(5%,C$,"/RW","R") THEN  &
		  PRINT "CHECK FILE FOR THIS MONTH HAS NOT BEEN CREATED." IF FNS%<>10% &
		\ PRINT "TRY AGAIN LATER....." IF FNS%=10% &
		\ X%=100%
2430	  RETURN
2600	  !
2610	  D1$=DATE$(0%) &
	\ D1$=RIGHT(NUM1$(100%+INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",CVT$$(MID(D1$,4%,3%),-1%))/3%+1%),2%)+"."+LEFT(D1$,2%)+"."+RIGHT(D1$,8%) &
	\ STOP IF FND7%(D1$) &
	\ PRINT "ENTER DATE FOR SUMMARY (<CR> FOR TODAY--";D1$;")"; &
	\ INPUT K$ &
	\ IF K$<>"" THEN  &
		  K$=FND7$(K$) &
		\ GOTO 2610 IF FND7%(K$)=-1% &
		\ D1$=K$
2620	  D8$=D1$ &
	\ INPUT "CONFIRM UPDATE (Y OR N) ";K$ &
	\ IF LEFT(K$,1%)<>"Y" THEN  &
		  GOTO 1020
2800	  !
2810	  LSET T3$=FNL$ &
	\ W$(I%)=U$(I%)+"" FOR I%=1% TO 6% &
	\ RETURN
2900	  !
2905	  RSET P$="1"
2906	  LSET P2$="AP" &
	\ LSET P8$=L$(6%)
2910	  RSET P1$=A$+" " &
	\ LSET P3$=CVTF$(L) &
	\ LSET P4$=CVT%$(VAL(LEFT(D1$,2%))) &
	\ LSET P5$=CVT%$(VAL(MID(D1$,4%,2%))) &
	\ LSET P6$=CVT%$(VAL(RIGHT(D1$,7%))) &
	\ LSET P7$=A1$ &
	\ IF FNA%(5%,T1$) THEN  &
		  PRINT "UNABLE TO ADD TO CHECK DATA FILE." &
		\ STOP
2920	  RETURN
5500	  !
5510	  GOSUB 2600 &
	\ PRINT  &
	\ A9%=0% &
	\ L,L1,L2=0. &
	\ GOSUB 2400 &
	\ GOTO 1020 IF X%=100% &
	\ IF FNG%(7%,"") THEN  &
		  GOTO 1020
5520	  GOSUB 2100 &
	\ L1=L1+L(1%) &
	\ L2=L2+L(2%) &
	\ GOSUB 5600 &
	\ IF FNA%(2%,T$) THEN  &
		  PRINT "UNABLE TO ADD TO FILE." &
		\ GOTO 10000
5540	  A1$=L$(1%)+" "+LEFT(L$(2%),12%)+" "+L$(7%)+L$(5%) &
	\ A$=CVT$$(L$(4%),-1%) &
	\ L=L(1%) &
	\ D1$=L$(8%) &
	\ GOSUB 2900 &
	\ PRINT "!"; &
	\ PRINT  IF POS(0%)>70% &
	\ IF FNN%(7%)=0% THEN &
		  GOTO 5520 &
	  ELSE &
		  PRINT 
5560	  D1$=D8$ &
	\ A1$="SUMMARY OF INVOICES RECEIVED." &
	\ L$(3%),L$(6%)="" &
	\ FOR I%=1% TO A9% &
		\ A$=" "+A9$(I%) &
		\ L=-A9(I%) &
		\ GOSUB 2900 &
	\ NEXT I% &
	\ IF L2<>0. THEN  &
		  A1$="SUMMARY OF DISCOUNTS TAKEN" &
		\ A$=" "+MID(A0$(23%),INSTR(1%,A0$(23%),"=")+1%,6%) &
		\ L=-L2 &
		\ GOSUB 2900
5590	  PRINT "UPDATE COMPLETE." &
	\ INPUT "SET PAGE/HIT RETURN ",K$ &
	\ PRINT STRING$(6%,10%);STRING$(30%,42%); &
	\ PRINT "UPDATE PAYABLE REGISTER WITH HOLDING FILE" &
	\ PRINT "*" &
	\ PRINT "*",DATE$(0%),TIME$(0%) &
	\ PRINT "*" &
	\ PRINT USING "* CREDIT"+B1$, VAL(A9$(I%)),A9(I%) FOR I%=1% TO A9% &
	\ IF L2<>0. THEN  &
		  PRINT USING "* CREDIT"+B1$, VAL(MID(A0$(23%),INSTR(1%,A0$(23%),"=")+1%,6%)),L2
5594	  PRINT USING "* DEBIT VARIOUS ###,###.##", L1 &
	\ PRINT "*" &
	\ PRINT STRING$(30%,42%);"UPDATE PAYABLE REGISTER WITH HOLDING FILE" &
	\ V%=FNC%(7%) &
	\ STOP IF FNO%(7%,"PAHOLD.DAT","/CR:21,64","") &
	\ GOTO 10000
5600	  !
5610	  FOR I%=1% TO A9% &
		\ IF L$(10%)=A9$(I%) THEN  &
			  A9(I%)=A9(I%)+L(1%)-L(2%) &
			\ RETURN
5620			  NEXT I% &
	\ A9%=A9%+1% &
	\ A9$(A9%)=L$(10%) &
	\ A9(A9%)=L(1%)-L(2%) &
	\ RETURN
6000	  !
6010	  PRINT  &
	\ INPUT "EXAMINE VENDOR # ";L8$ &
	\ GOTO 1020 IF L8$="" &
	\ L8$=L8$+SPACE$(6%-LEN(L8$)) &
	\ IF FNG%(2%,L8$) THEN  &
		  PRINT "NO TRANSACTIONS IN FILE." &
		\ GOTO 6000
6030	  X%=0% &
	\ X1%=70% &
	\ GOSUB 12120 &
	\ T,T1=0. &
	\ GOSUB 2100
6040	  GOSUB 2200 &
	\ IF FNN%(2%)=0% THEN  &
		  GOSUB 2100 &
		\ IF L$(1%)=L8$ THEN  &
			  GOTO 6040
6070	  GOSUB 12400 &
	\ GOTO 6000
7500	  !
7505	  PRINT "RUN THIS OPTION AFTER CHECKS ARE WRITTEN BUT BEFORE THE TRANSFER." &
	\ INPUT "CONFIRM PRINTING ATTACHMENTS (Y OR N)";C$ &
	\ GOTO 1020 IF LEFT(C$,1%)<>"Y" &
	\ A%=INSTR(1%,A0$(21%),"-") &
	\ A%=VAL(RIGHT(A0$(21%),A%+1%)) UNLESS A%=0% &
	\ A%=14% IF A%<=0%
7507	  INPUT "SPECIFIC CHECK NOS. (Y OR N)";Z9$
7508	  IF Z9$<>"Y" THEN &
		  GOTO 7511 &
	  ELSE &
		  INPUT "VENDOR";Z7$ &
		\ Z7$=CVT$$(Z7$,-1%) &
		\ GOTO 1020 IF Z7$="" &
		\ Z7$=Z7$+SPACE$(6%-LEN(Z7$)) &
		\ GOTO 7507 IF FNG%(2%,Z7$) &
		\ INPUT "CHECK NUMBER";Z8$ &
		\ Z8$=CVT$$(Z8$,-1%)
7509	  GOSUB 2100 &
	\ IF CVT$$(L$(3%),-1%)<>Z8$ THEN  &
		  V%=FNN%(2%) &
		\ GOTO 7508 IF M$(1%)<>Z7$ &
		\ GOTO 7509
7511	  X%=62% &
	\ V%=FNG%(2%,"") IF Z9$<>"Y" &
	\ INPUT "SET PAGE ";A$
7515	  GOSUB 2100 &
	\ IF Z9$="Y" THEN  &
		  A$=Z8$ &
		\ A1$=L$(1%)+"" &
		\ GOTO 7545
7520	  IF Z9$<>"Y" AND (ASCII(L$(3%))>=ASCII("0") AND ASCII(L$(3%))<=ASCII("9")) THEN &
		  GOTO 7540 &
	  ELSE &
		  IF Z9$="Y" THEN  &
			  PRINT STRING$(66%-X%,10%) &
			\ GOTO 7508
7530	  IF FNN%(2%) THEN &
		  GOTO 7820 &
	  ELSE &
		  GOSUB 2100 &
		\ GOTO 7520
7540	  A$=L$(3%)+"" &
	\ A1$=L$(1%)+""
7545	  L1$=L$(2%)+"" &
	\ L%=1%
7550	  IF FNN%(2%) THEN &
		  GOTO 7820 &
	  ELSE &
		  GOSUB 2100
7560	  GOTO 7520 IF L$(1%)<>A1$ &
	\ GOTO 7550 IF L$(3%)<>A$ &
	\ L%=L%+1% &
	\ GOTO 7550 IF L%<A% &
	\ STOP IF FNG%(11%,A1$) &
	\ GOSUB 2800 &
	\ T,T1=0. &
	\ GOSUB 7880 &
	\ STOP IF FNG%(2%,A1$+L1$) &
	\ GOSUB 2100 &
	\ GOSUB 7700 IF L$(3%)=A$
7610	  GOTO 7800 IF FNN%(2%) &
	\ GOSUB 2100 &
	\ GOTO 7800 IF L$(1%)<>A1$ &
	\ GOTO 7610 IF L$(3%)<>A$ &
	\ GOSUB 7700 &
	\ GOTO 7610
7700	  X%=X%+1% &
	\ GOSUB 7880 IF X%>55% &
	\ L=0. &
	\ L=L(2%)/L(1%)*100. IF L(1%)>0.014687738381326199 &
	\ L=0. IF L>99. &
	\ PRINT USING "\      \  \    \  \    \ \            \"+"###,###.## ##.#% ###,###.## ###,###.##", L$(4%),L$(6%),L$(7%),L$(2%),L(1%),L,L(2%),L(1%)-L(2%) &
	\ T=T+L(1%) &
	\ T1=T1+L(2%) &
	\ RETURN
7800	  PRINT USING B$, "TOTAL    :",T &
	\ PRINT USING B$, "DEDUCTION :",T1 &
	\ PRINT USING B$, "NET AMOUNT:",T-T1 &
	\ X%=X%+4% &
	\ IF FNS%=0% THEN  &
		  GOTO 7515
7820	  PRINT STRING$(66%-X%,10%) &
	\ GOTO 1020
7880	  PRINT STRING$(66%-X%,10%); &
	\ PRINT "CHECK # ";A$;" for ";W$(1%);"-";W$(2%),DATE$(0%) &
	\ PRINT "ACCOUNT   JOB#    PO#    INVOICE           AMOUNT         ";"DISCOUNT        NET" &
	\ X%=1% &
	\ RETURN
8500	  !
8505	  INPUT "HAVE YOU RUN ATTACHMENTS (INV OPTION)? (Y OR N)",K$ &
	\ GOTO 1020 IF K$<>"Y" &
	\ A9%=0% &
	\ GOSUB 2600 &
	\ PRINT  &
	\ GOSUB 2400 &
	\ GOTO 1020 IF X%=100% &
	\ X$=MID(A0$(20%),INSTR(1%,A0$(20%),"=")+1%,6%) &
	\ A$=" "+X$ &
	\ IF X$="INPUT!" THEN  &
		  X$="" &
		\ INPUT "CASH GL#: (XXX.XX)"X$ UNTIL LEN(X$)=6% &
		\ A$=" "+X$
8510	  LSET P2$="CK" &
	\ T,T2=0. &
	\ C$="" &
	\ V%=FNC%(7%) &
	\ GOSUB 2300 IF FNO%(7%,"PLOG.DAT","/RW","") UNLESS MID(A0$(12%),2%,1%)="N" &
	\ GOTO 1020 IF FNG%(2%,"")
8550	  GOSUB 2100 &
	\ GOTO 8570 IF LEFT(L$(3%),1%)<"0" OR LEFT(L$(3%),1%)>"9" &
	\ IF C$="" OR C$=L$(3%) THEN &
		  GOTO 8560 &
	  ELSE &
		  GOSUB 8595
8560	  STOP IF FNG%(11%,L$(1%)) &
	\ GOSUB 2800 &
	\ GOTO 8568 IF MID(A0$(12%),2%,1%)<>"L" &
	\ LSET L1$(1%)=CVT$$(W$(2%),1%) &
	\ P9$="OT" &
	\ P9$=P0$(P8%,0%) IF L$(4%)=P0$(P8%,I%) FOR P8%=1% TO P9% FOR I%=1% TO 2% &
	\ LSET L1$(2%)=RIGHT(D1$,7%)+LEFT(D1$,2%) &
	\ LSET L1$(3%)=L$(2%) &
	\ LSET L1$(4%)=L$(5%) &
	\ LSET L1$(5%)=P9$ &
	\ LSET L1$(6%)=L$(6%) &
	\ LSET L1$(7%)=L$(3%) &
	\ LSET L1$(8%)=L$(1%) &
	\ LSET L1$(9%)=L$(7%) &
	\ LSET L1$(10%)=CVTF$(L(1%)-L(2%)) &
	\ STOP IF FNA%(7%,T5$)
8568	  C$=L$(3%) &
	\ A1$=L$(1%) &
	\ L1$=L$(9%) &
	\ T2=T2+L(1%)-L(2%) &
	\ T=T+L(1%)-L(2%) &
	\ GOSUB 5600 &
	\ STOP IF FND%(2%,"") &
	\ PRINT "!"; &
	\ PRINT  IF POS(0%)>70%
8570	  IF FNN%(2%)=0% THEN &
		  GOTO 8550 &
	  ELSE &
		  PRINT  &
		\ GOSUB 8595 &
		\ FOR I%=1% TO A9% &
			\ L=A9(I%) &
			\ D1$=D8$ &
			\ A$=" "+A9$(I%) &
			\ L$(3%),L$(6%)="" &
			\ A1$="SUMMARY OF INVOICES PAID" &
			\ RSET P$="7" &
			\ GOSUB 2906 &
		\ NEXT I%
8590	  PRINT "TRANSFER COMPLETE :" &
	\ INPUT "SET PAGE / HIT RETURN ",K$ &
	\ PRINT STRING$(6%,10%);STRING$(30%,42%); &
	\ PRINT "TRANSFER OF PAID CHECKS FROM PAYABLES" &
	\ PRINT "*" &
	\ PRINT "*",DATE$(0%),TIME$(0%) &
	\ PRINT "*" &
	\ PRINT USING "* CREDIT \    \ : ###,###.##", A9$(I%),-A9(I%) FOR I%=1% TO A9% &
	\ PRINT USING "* DEBIT  \    \ : ###,###.##", MID(A0$(20%),INSTR(1%,A0$(20%),"=")+1%,6%),T &
	\ PRINT "*" &
	\ GOTO 10000
8595	  RSET P$=CVT$$(C$,128%) &
	\ LSET P8$="" &
	\ L=-T2 &
	\ A1$=W$(2%)+"" &
	\ D1$=L1$ &
	\ GOSUB 2910 &
	\ T2=0. &
	\ RETURN
10000	  !
10050	  CLOSE 1% &
	\ CLOSE 4% &
	\ CLOSE 9% &
	\ CLOSE 10% &
	\ V%=FNX%("",0%,"")
12000	  !
12010	  T,T1,T2,L,L1,L2=0. &
	\ GOTO 1020 IF FNG%(2%,"") &
	\ INPUT "SET PAGE ";A$ &
	\ X1%=66% &
	\ X%=0% &
	\ GOSUB 12100 &
	\ GOSUB 2100 &
	\ L1$=L$(1%)+""
12020	  V%=FNG%(11%,L$(1%)) &
	\ GOSUB 2800 IF V%=0% &
	\ W$(2%)="* Undefined vendor code *" IF V%<>0% &
	\ GOSUB 12200
12030	  GOSUB 2200 &
	\ GOSUB 12100 IF X1%>52% &
	\ IF FNN%(2%) THEN &
		  GOTO 12300 &
	  ELSE &
		  GOSUB 2100 &
		\ IF L1$=L$(1%) THEN  &
			  GOTO 12030
12040	  GOSUB 12400 &
	\ L1$=L$(1%)+"" &
	\ GOTO 12020
12100	  !
12110	  X%=X%+1% &
	\ PRINT  FOR I%=X1% TO 71% &
	\ PRINT "REGISTER OF ACCOUNTS PAYABLE "; &
	\ PRINT DATE$(0%),TIME$(0%) &
	\ PRINT  &
	\ PRINT "PAGE"; &
	\ PRINT USING "  ##", X%
12120	  PRINT "VENDOR INVOICE #     PO#       AMOUNT  DISCNT        ";"NET TAX GEN CD  JOB    CHECK" &
	\ PRINT  &
	\ X1%=11% &
	\ RETURN
12200	  PRINT USING "\                          \", W$(2%) &
	\ X1%=X1%+1% &
	\ T0%=0% &
	\ RETURN
12300	  GOSUB 12400 &
	\ T=L &
	\ T1=L1 &
	\ T2=L2 &
	\ GOSUB 12400 &
	\ GOTO 1020
12400	  IF T0%>1% THEN  &
		  PRINT "       TOTALS----->";TAB(25%); &
		\ PRINT USING "#,###,###.## ####.## ###,###.##", T,T1,T2 &
		\ X1%=X1%+1%
12410	  T0%=2% &
	\ PRINT  &
	\ X1%=X1%+1% &
	\ L=L+T &
	\ L1=L1+T1 &
	\ L2=L2+T2 &
	\ T,T1,T2=0. &
	\ RETURN
14000	  !
14001	  DEF FND9$(V%) &
	\ FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(V%) AND 254%)/2%+100%),2%) &
	\ FNEND
14020	  DEF FNA7$(K$) &
	\ FNA7$=RIGHT(NUM1$(1000%+CVT$%(K$)),2%)+"."+RIGHT(NUM1$(100%+ASCII(RIGHT(K$,3%))),2%) &
	\ FNEND
14210	  DEF FND7%(K$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,K$,".")<>3% OR INSTR(4%,K$,".")<>6% OR INSTR(7%,K$,".")<>0% OR LEN(K$)<>8% &
	\ D7%=VAL(LEFT(K$,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>12% &
	\ D7%=VAL(MID(K$,4%,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>31% &
	\ D7%=VAL(RIGHT(K$,7%)) &
	\ GOTO 14220 IF D7%<0% &
	\ FND7%=0% &
	\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14260	  DEF FND7$(K$) &
	\ K$=K$+"."+RIGHT(DATE$(0%),8%) IF LEN(K$)<6% &
	\ K$="0"+K$ IF INSTR(1%,K$,".")=2% &
	\ K$=LEFT(K$,3%)+"0"+RIGHT(K$,4%) IF INSTR(4%,K$,".")=5% &
	\ FND7$=K$ &
	\ FNEND
32767	  END
