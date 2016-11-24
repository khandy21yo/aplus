5	  ! &
	  ! Program name: pahold		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 12:11 AM
10	  V$=SYS(CHR$(12%)) &
	\ CHANGE V$ TO Y% &
	\ PRG$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":" &
	\ PRG$=PRG$+"["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRG$=PRG$+RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+RAD$(Y%(11%)+SWAP%(Y%(12%)))
20	  DIM M$(12%), L$(10%), U$(7%), W$(6%), L(2%), S$(12%), A1$(10%,1%), P0$(10%,2%), Y%(30%)
30	  OPEN "KB:" FOR INPUT AS FILE 9% &
	\ IF FNO%(11%,"VENDOR.DAT","/RO","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING VENDOR.DAT." &
		\ GOTO 10000
40	  V%=FNO%(7%,"PAHOLD.DAT","/RW","") &
	\ IF V%=5% THEN &
		  GOSUB 2300 &
	  ELSE &
		  IF V%<>0% THEN  &
			  PRINT "ERROR";V%;" IN OPENING PAHOLD.DAT." &
			\ GOTO 10000
50	  ON ERROR GOTO 60 &
	\ OPEN "SS0:UNIQUE.FIL/RO" FOR INPUT AS FILE 4% &
	\ DIM #4%, A0$(255%)=64% &
	\ GOTO 70
60	  PRINT "Use !UNIQUE to build a UNIQUE file." &
	\ RESUME 10000
70	  ON ERROR GOTO 0 &
	\ P1$=MID(A0$(14%),2%,1%) &
	\ P2$=MID(A0$(21%),INSTR(1%,A0$(21%),"=")+1%,6%) &
	\ P3$=MID(A0$(13%),2%,1%)
80	  P9$=CVT$$(A0$(40%),-1%) &
	\ P8$=CVT$$(A0$(41%),-1%) &
	\ GOTO 110 IF P9$="" AND P8$="" &
	\ IF P8$<>"" THEN  &
		  P9$=P9$+","+P8$
90	  GOTO 110 IF P9$="" &
	\ I%=INSTR(1%,P9$,",") &
	\ I%=LEN(P9$)+1% IF I%=0% &
	\ P3$=LEFT(P9$,I%-1%) &
	\ P9$=RIGHT(P9$,I%+1%)
100	  P9%=P9%+1% &
	\ E%=INSTR(1%,P3$,"=") &
	\ X%=INSTR(1%,P3$,"/") &
	\ P0$(P9%,0%)=LEFT(P3$,E%-1%) &
	\ P0$(P9%,1%)=MID(P3$,E%+1%,X%-E%-1%) &
	\ P0$(P9%,2%)=RIGHT(P3$,X%+1%) &
	\ GOTO 90
110	  OPEN "NL:" AS FILE 1%, RECORDSIZE 64% &
	\ FIELD #1%, 6% AS M$(1%),13% AS M$(2%),6% AS M$(3%),3% AS M$(4%),3% AS M$(12%),1% AS M$(5%),6% AS M$(6%),6% AS M$(7%),8% AS M$(8%),8% AS M$(9%),2% AS M$(10%),2% AS M$(11%) &
	\ FIELD #1%, 64% AS T$ &
	\ OPEN "NL:" AS FILE 10%, RECORDSIZE 128%+64% &
	\ FIELD #10%, 6% AS U$(1%),29% AS U$(2%),29% AS U$(3%),29% AS U$(4%),29% AS U$(5%),5% AS U$(6%),1% AS U$(7%) &
	\ FIELD #10%, 128% AS T3$,6% AS P$(1%),6% AS P$(2%),6% AS P$(3%),6% AS P$(4%) &
	\ FIELD #10%, 128% AS T3$,64% AS T4$
120	  READ S$(L%) FOR L%=1% TO 12% &
	\ DATA	"VENDOR # ","INVOICE # ","CHECK # ","G/L NUMBER ","TAX FLAG ","JOB NUMBER ","PO NUMBER ","AMOUNT ","DISCOUNT","INVOICE DATE ","DATE WRITTEN","PAYABLE ACCOUNT"

130	  K$=CVT$$(RIGHT(A0$(21%),INSTR(1%,A0$(21%)+",",",")+1%),128%)+"," &
	\ K$="" IF K$="," &
	\ A1%=0% &
	\ WHILE K$<>"" &
		\ A1%=A1%+1% &
		\ I%=INSTR(1%,K$,",") &
		\ V$=LEFT(K$,I%-1%) &
		\ K$=RIGHT(K$,I%+1%) &
		\ I%=INSTR(1%,V$,"-") &
		\ A1$(A1%,0%)=LEFT(V$,I%-1%) IF I% &
		\ A1$(A1%,0%)=V$ IF I%=0% &
		\ A1$(A1%,1%)=RIGHT(V$,I%+1%) &
	\ NEXT &
	\ CLOSE 4%
1000	  !
1020	  PRINT  &
	\ INPUT "Option ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 3000 IF K$="ENT" &
	\ GOTO 4000 IF K$="DEL" &
	\ GOTO 5000 IF K$="CHA" &
	\ GOTO 12000 IF K$="LIS" &
	\ GOTO 3020 IF K$="VOI" &
	\ GOTO 10000 IF K$="END" &
	\ PRINT  &
	\ PRINT "Press <CR> for list of options." &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS ARE:" &
	\ PRINT "   ENTER    - Enter invoices in holding file" &
	\ PRINT "   DELETE   - Delete invoices in holding file" &
	\ PRINT "   CHANGE   - Change invoices in holding file" &
	\ PRINT "   LIST     - List invoices in holding file" &
	\ PRINT "   VOID     - Void check entries" &
	\ PRINT "   APPEND   - Append invoices from alternate file" &
	\ PRINT "   SAVE     - Save current holding file to alternate file" &
	\ PRINT "   REPLACE  - Replace alternate file with current hold. file" &
	\ PRINT "   END      - END program" &
	\ GOTO 1020
2000	  LSET M$(I%)=L$(I%) FOR I%=1% TO 7% &
	\ LSET M$(I%+7%)=CVTF$(L(I%)) FOR I%=1% TO 2% &
	\ LSET M$(I%+2%)=CVT%$(FND9%(L$(I%))) FOR I%=8% TO 9% &
	\ LSET M$(4%)=FNA8$(L$(4%)) &
	\ LSET M$(12%)=FNA8$(L$(10%)) &
	\ RETURN
2100	  LSET T$=FNL$ &
	\ L$(I%)=M$(I%)+"" FOR I%=1% TO 7% &
	\ L(I%)=CVT$F(M$(I%+7%)) FOR I%=1% TO 2% &
	\ L$(I%)=FND9$(CVT$%(M$(I%+2%))) FOR I%=8% TO 9% &
	\ L$(4%)=FNA7$(M$(4%)) &
	\ L$(10%)=FNA7$(M$(12%)) &
	\ RETURN
2200	  PRINT USING "\    \ \           \ \    \###,###.## ####.## "+"###,###.## \\"+" \      \\    \\    \", L$(1%),L$(2%),L$(7%),L(1%),L(2%),L(1%)-L(2%),L$(5%),L$(4%),L$(6%),L$(3%) &
	\ T=T+L(1%) &
	\ T1=T1+L(2%) &
	\ T2=T2+L(1%)-L(2%) &
	\ X1%=X1%+1% &
	\ T0%=T0%+1% &
	\ IF P1$="P" AND L$(6%)<>"" THEN  &
		  IF FNG%(5%,L$(6%)) THEN  &
			  PRINT "*************************CHECK ABOVE JOB "+"#****************" &
			\ X1%=X1%+1%
2220	  RETURN
2300	  PRINT "A holding file does not exist. Shall I create one "; &
	\ INPUT K$ &
	\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
	\ V%=FNO%(7%,"PAHOLD.DAT","/CR:21,64","") &
	\ IF V%<>0% THEN  &
		  PRINT "Error";V%;"in creating PAHOLD.DAT." &
		\ GOTO 10000
2340	  PRINT "Created!" &
	\ RETURN
2500	  INPUT LINE #9%, K$ &
	\ K$=CVT$$(K$,4%) &
	\ RETURN
2600	  K$=CVT$$(K$,6%) &
	\ K$="0"+K$ IF INSTR(1%,K$,".")=2% &
	\ K$=K$+"."+RIGHT(DATE$(0%),8%) IF LEN(K$)<6% &
	\ K$=LEFT(K$,3%)+"0"+RIGHT(K$,4%) IF INSTR(4%,K$,".")=5% &
	\ ON ERROR GOTO 2690 &
	\ F%=0% &
	\ GOTO 2690 IF INSTR(1%,K$,".")<>3% OR INSTR(4%,K$,".")<>6% OR INSTR(7%,K$,".")<>0% OR LEN(K$)<>8% &
	\ I%=VAL(LEFT(K$,2%)) &
	\ GOTO 2690 IF I%<1% OR I%>12% &
	\ I%=VAL(MID(K$,4%,2%)) &
	\ GOTO 2690 IF I%<1% OR I%>31% &
	\ I%=VAL(RIGHT(K$,7%)) &
	\ GOTO 2690 IF I%<0% &
	\ GOTO 2695
2690	  F%=-1% &
	\ RESUME 2695
2695	  ON ERROR GOTO 0 &
	\ RETURN
2800	  LSET T3$=FNL$ &
	\ W$(I%)=U$(I%)+"" FOR I%=1% TO 6% &
	\ RETURN
3000	  !
3010	  F2%=0% &
	\ GOTO 3030
3020	  F2%=1%
3030	  IF P1$="P" THEN  &
		  V%=FNO%(5%,"PO.DAT","/RO","") &
		\ IF V%<>0% THEN  &
			  PRINT "ERROR";V%;"IN OPENING PO.DAT." &
			\ GOTO 10000
3040	  PRINT  &
	\ INPUT "DATE (MM.DD.YY) ";K$ &
	\ IF K$="" THEN &
		  A2$="" &
	  ELSE &
		  GOSUB 2600 &
		\ L$(8%),A2$=K$ &
		\ IF F% THEN  &
			  PRINT "BAD DATE !!!" &
			\ GOTO 3040
3050	  L$(10%)=P2$ &
	\ IF A1% THEN  &
		  INPUT "PAYABLE ACCOUNT ";K$ &
		\ IF K$="" THEN &
			  K$=P2$ &
		  ELSE &
			  L$(10%)=K$ &
			\ GOTO 3060 IF K$>=A1$(I%,0%) AND K$<=A1$(I%,1%) FOR I%=1% TO A1% &
			\ PRINT "BAD PAYABLE ACCOUNT NUMBER !!!!!" &
			\ GOTO 3050
3060	  PRINT  &
	\ PRINT "VENDOR #  INVOICE #             PO#          AMOUNT   ";"DISCNT TAX  GEN CD#  JOB #" &
	\ PRINT  &
	\ V$=SYS(CHR$(3%))
3070	  F0%=0% &
	\ L$(6%)="" &
	\ L$(9%)="00.00.00" &
	\ PRINT "> "; &
	\ GOSUB 2500 &
	\ GOTO 3180 IF K$="." &
	\ L$(1%)=K$+"" &
	\ L$(1%)=M$(1%) IF K$="" &
	\ GOTO 3160 IF FNG%(11%,L$(1%)+SPACE$(6%-LEN(L$(1%)))) &
	\ GOSUB 2800 &
	\ PRINT CVT$$(W$(2%),128%); &
	\ IF F2% THEN  &
		  INPUT "   CHECK # ";L$(3%) &
		\ PRINT L$(3%); &
		\ PRINT "  CHECK DATE ?"; &
		\ L$(9%)="******" &
		\ WHILE FND7%(L$(9%))<>0% AND L$(9%)<>"-" &
			\ PRINT "BAD DATE,";CHR$(10%)+CHR$(13%);"CHECK DATE"; IF L$(9%)<>"******" &
			\ INPUT #9%, L$(9%) &
			\ L$(9%)=FND7$(L$(9%)) UNLESS L$(9%)="-" &
		\ NEXT &
		\ GOTO 3170 IF L$(9%)="-" &
		\ PRINT L$(9%);
3080	  IF A2$="" THEN  &
		  INPUT "    DATE ";K$ &
		\ K$=L$(8%) IF K$="" &
		\ GOSUB 2600 &
		\ L$(8%)=K$ IF F%=0% &
		\ IF F% THEN  &
			  PRINT "BAD DATE !!!" &
			\ GOTO 3080
3090	  PRINT L$(8%); IF A2$="" &
	\ L$(3%)="D"+LEFT(L$(8%),5%) UNLESS F2% &
	\ PRINT  &
	\ PRINT USING "\     \", L$(1%); &
	\ GOSUB 2500 &
	\ GOTO 3170 IF K$="-" &
	\ L$(2%)=K$ &
	\ GOTO 3170 IF L$(2%)="" &
	\ PRINT USING "   \         \     ", L$(2%); &
	\ L$(7%),K$="" &
	\ GOSUB 2500 UNLESS P1$="N" &
	\ GOTO 3170 IF K$="-" &
	\ PRINT USING "      \    \   ", K$; &
	\ L$(7%)=K$+""
3110	  GOSUB 2500 &
	\ GOTO 3170 IF K$="-" &
	\ L(1%)=VAL(K$)/100. &
	\ PRINT USING "###,###.## ", L(1%); &
	\ GOSUB 2500 &
	\ GOTO 3170 IF K$="-" &
	\ W$(6%)="0" IF VAL(W$(6%))<=0% &
	\ K$=CVT$$(W$(6%),128%)+"%" IF K$="" &
	\ IF RIGHT(K$,LEN(K$))<>"%" THEN  &
		  L(2%)=VAL(K$)/100. &
		\ PRINT USING "#,###.##   ", L(2%); &
		\ GOTO 3130
3120	  L(2%)=INT(L(1%)*VAL(LEFT(K$,LEN(K$)-1%))+0.5)/100. &
	\ PRINT USING " \      \  ", SPACE$(7%-LEN(K$))+K$;
3130	  GOSUB 2500 &
	\ GOTO 3170 IF K$<>"" AND K$<>"T" AND K$<>"E" &
	\ L$(5%)=K$+"" &
	\ L$(5%)="E" IF K$="" &
	\ PRINT USING "\\ ", L$(5%); &
	\ IF P1$="P" AND L$(7%)<>"" THEN  &
		  F0%=FNG%(5%,L$(7%)) &
		\ GOTO 3131 IF F0%<>0% &
		\ LSET T4$=FNL$ &
		\ K$=CVT$$(P$(4%),-1%) &
		\ GOTO 3132 UNLESS K$=""
3131	  GOSUB 2500
3132	  GOTO 3135 IF P9%=0% &
	\ GOTO 3170 IF K$="-" &
	\ GOTO 3135 IF LEN(K$)<>2% &
	\ GOTO 3134 IF K$=P0$(X%,0%) FOR X%=1% TO P9% &
	\ PRINT "BAD CODE !"+STRING$(3%,7%) &
	\ GOTO 3170
3134	  L$(4%)=P0$(X%,1%) IF L$(7%)<>"" &
	\ L$(4%)=P0$(X%,2%) IF L$(7%)="" &
	\ GOTO 3140
3135	  K$=K$+".00" IF LEN(K$)=3% &
	\ K$=LEFT(K$,3%)+"."+RIGHT(K$,4%) IF INSTR(1%,K$,".")=0% AND LEN(K$)=5% &
	\ L$(4%)=K$ &
	\ IF LEN(K$)<>6% THEN  &
		  PRINT "VOID !!!" &
		\ GOTO 3070
3140	  PRINT USING "\      \", L$(4%); &
	\ GOTO 3150 IF P3$="N" &
	\ GOSUB 2500 &
	\ GOTO 3170 IF K$="-" &
	\ L$(6%)=K$
3150	  PRINT USING " \   \", L$(6%) &
	\ GOTO 3155 IF P1$<>"P" &
	\ GOTO 3155 IF L$(7%)="" &
	\ IF F0% THEN  &
		  PRINT "PO ";L$(7%);" NOT IN PO FILE" &
		\ GOTO 3154
3151	  IF L$(1%)<>P$(3%) THEN  &
		  PRINT "VENDORS DO NOT MATCH" &
		\ PRINT "PO'S VENDOR  =  ";P$(3%);"" &
		\ F0%=-1%
3152	  IF L$(6%)<>P$(2%) THEN  &
		  PRINT "JOB NUMBERS DO NOT MATCH" &
		\ PRINT "PO'S JOB #  = ";P$(2%) &
		\ GOTO 3154
3153	  GOTO 3155 UNLESS F0%
3154	  INPUT "ENTER ANYWAY (Y OR N)",K$ &
	\ GOTO 3170 IF K$<>"Y" &
	\ PRINT 
3155	  GOSUB 2000 &
	\ STOP IF FNA%(7%,T$) &
	\ GOTO 3070
3160	  PRINT L$(1%);" NOT FOUND IN VENDOR FILE"
3170	  PRINT "LINE ABORTED" &
	\ L$(9%)="00.00.00" &
	\ L$(4%)="000.00" &
	\ GOSUB 2000 &
	\ GOTO 3070
3180	  V$=SYS(CHR$(2%)) &
	\ V%=FNC%(5%) &
	\ PRINT  &
	\ GOTO 1020
4000	  PRINT  &
	\ INPUT "DELETE VENDOR  # ";L8$ &
	\ GOTO 1020 IF L8$="" &
	\ L8$=L8$+SPACE$(6%-LEN(L8$)) &
	\ INPUT "INVOICE # ";A2$ &
	\ A$=LEFT(A2$+SPACE$(13%),13%) &
	\ IF FNG%(7%,L8$+A$) THEN  &
		  PRINT "TRANSACTION DOES NOT EXIST." &
		\ GOTO 4000
4050	  PRINT  &
	\ GOSUB 2100 &
	\ GOSUB 2200 &
	\ INPUT "CONFIRM (Y/N/ + /-) ";K$ &
	\ GOTO 4070 IF K$="+" &
	\ GOTO 4080 IF K$="-" &
	\ IF LEFT(K$,1%)="Y" THEN  &
		  V%=FND%(7%,"") &
		\ PRINT "DELETED !!!"
4060	  GOTO 4000
4070	  IF FNN%(7%)=0% THEN &
		  GOTO 4050 &
	  ELSE &
		  PRINT "< End-of-file encountered >" &
		\ GOTO 4000
4080	  IF FNN%(-7%)=0% THEN &
		  GOTO 4050 &
	  ELSE &
		  PRINT "< Beginning-of-file encountered >" &
		\ GOTO 4000
5000	  PRINT  &
	\ INPUT "CHANGE VENDOR  # ";L8$ &
	\ GOTO 1020 IF L8$="" &
	\ L8$=L8$+SPACE$(6%-LEN(L8$)) &
	\ INPUT "INVOICE # ";A2$ &
	\ IF FNG%(7%,L8$+A2$) THEN  &
		  PRINT "TRANSACTION DOES NOT EXIST." &
		\ GOTO 5000
5020	  GOSUB 2100 &
	\ GOSUB 2200
5030	  INPUT "ITEM TO CHANGE";A$ &
	\ GOTO 5000 IF A$="" &
	\ IF A$="+" THEN  &
		  IF FNN%(7%)=0% THEN &
			  GOTO 5020 &
		  ELSE &
			  PRINT "< End-of-file encountered >" &
			\ GOTO 5000
5040	  IF A$="-" THEN  &
		  IF FNN%(-7%)=0% THEN &
			  GOTO 5020 &
		  ELSE &
			  PRINT "< Beginning-of-file encountered >" &
			\ GOTO 5000
5050	  V$="VE IN CK GL TX JB PO AM DI DA DW AP" &
	\ I%=INSTR(1%,V$,A$) &
	\ I%=(I%+2%)/3% &
	\ IF I%=0% THEN  &
		  PRINT "TYPE: VE FOR VENDOR #" &
		\ PRINT "      IN FOR INVOICE #" &
		\ PRINT "      CK FOR CHECK NUMBER" &
		\ PRINT "      GL FOR G/L NUMBER" &
		\ PRINT "      TX FOR TAX FLAG" &
		\ PRINT "      JB FOR JOB NUMBER" &
		\ PRINT "      PO FOR PO NUMBER" &
		\ PRINT "      AM FOR AMOUNT" &
		\ PRINT "      DI FOR DISCOUNT" &
		\ PRINT "      DA FOR INVOICE DATE" &
		\ PRINT "      DW FOR DATE CHECK WRITTEN" &
		\ PRINT "      AP FOR PAYABLE ACCOUNT" &
		\ PRINT "       +   FOR NEXT INVOICE" &
		\ PRINT "      -  FOR PREVIOUS INVOICE" &
		\ GOTO 5030
5060	  IF I%<=7% THEN  &
		  PRINT USING "\      \  \         \", S$(I%),L$(I%); &
		\ GOTO 5090
5070	  IF I%<=9% THEN  &
		  PRINT USING "\      \ ###,###.##", S$(I%),L(I%-7%); &
		\ GOTO 5090
5080	  PRINT USING "\          \  \       \ ", S$(I%),L$(I%-2%);
5090	  A$=SYS(CHR$(3%)) IF I%=8% OR I%=9% &
	\ INPUT A$ &
	\ K$=SYS(CHR$(2%)) &
	\ IF A$="" THEN  &
		  PRINT  &
		\ GOTO 5030
5100	  IF I%<=2% THEN  &
		  V%=FND%(7%,"") &
		\ L$(I%)=A$+"" &
		\ GOSUB 2000 &
		\ STOP IF FNA%(7%,T$) &
		\ GOTO 5000
5110	  GOTO 5130 IF I%>7% &
	\ L$(I%)=A$+""
5120	  GOSUB 2000 &
	\ STOP IF FNU%(7%,T$) &
	\ GOTO 5030
5130	  IF I%<=9% THEN  &
		  L(I%-7%)=VAL(A$)/100. &
		\ PRINT USING "###,###,###.##", L(I%-7%) &
		\ GOTO 5120
5140	  L$(I%-2%)=A$ &
	\ GOTO 5120
10000	  V%=FNX%("",0%,"")
12000	  GOTO 1020 IF FNG%(7%,"") &
	\ GOSUB 2100 &
	\ V%=FNO%(5%,"JOB.DAT","/RO","") IF P1$="P" &
	\ T,T1,T2,T3,T4,T5=0. &
	\ INPUT "SET PAGE ";A$ &
	\ X1%=66% &
	\ X%=0% &
	\ GOSUB 13000 &
	\ L8$=L$(1%)+""
12020	  IF FNG%(11%,L$(1%))=0% THEN &
		  GOSUB 2800 &
	  ELSE &
		  W$(I%)="" FOR I%=1% TO 6% &
		\ W$(2%)="UNDEFINED"
12022	  GOSUB 13030
12025	  GOSUB 2200 &
	\ GOSUB 13000 IF X1%>52% &
	\ IF FNN%(7%) THEN &
		  GOTO 13050 &
	  ELSE &
		  GOSUB 2100 &
		\ IF L8$=L$(1%) THEN  &
			  GOTO 12025
12027	  GOSUB 12030 &
	\ L8$=L$(1%)+"" &
	\ GOTO 12020
12030	  IF T0%>1% THEN  &
		  PRINT "       TOTALS----->";TAB(25%); &
		\ PRINT USING "#,###,###.## ####.## ###,###.##", T,T1,T2 &
		\ X1%=X1%+1%
12035	  T0%=2% &
	\ PRINT  &
	\ X1%=X1%+1% &
	\ T3=T3+T &
	\ T4=T4+T1 &
	\ T5=T5+T2 &
	\ T,T1,T2=0. &
	\ RETURN
13000	  X%=X%+1% &
	\ PRINT  FOR I%=X1% TO 71% &
	\ PRINT "REGISTER OF ACCOUNTS PAYABLE  HOLDING FILE "; &
	\ PRINT DATE$(0%),TIME$(0%) &
	\ PRINT  &
	\ PRINT "PAGE"; &
	\ PRINT USING "  ##", X% &
	\ PRINT "VENDOR INVOICE #     PO#       AMOUNT  DISCNT        NET TAX";" GEN CD  JOB   CHECK" &
	\ PRINT  &
	\ X1%=11% &
	\ RETURN
13030	  PRINT USING "\                          \", W$(2%) &
	\ X1%=X1%+1% &
	\ T0%=0% &
	\ RETURN
13050	  GOSUB 12030 &
	\ T=T3 &
	\ T1=T4 &
	\ T2=T5 &
	\ GOSUB 12030 &
	\ V%=FNC%(5%) &
	\ GOTO 1020
14000	  DEF FND9%(K$) &
	\ K$="0"+K$ IF INSTR(1%,K$,".")=2% &
	\ K$=LEFT(K$,3%)+"0"+RIGHT(K$,4%) IF INSTR(4%,K$,".")=5% &
	\ FND9%=VAL(LEFT(K$,2%))+VAL(MID(K$,4%,2%))*16%+FND8%(VAL(RIGHT(K$,7%)))*512% &
	\ FNEND
14001	  DEF FND9$(V%) &
	\ FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(V%) AND 254%)/2%+100%),2%) &
	\ FNEND &
	\ DEF FND8%(T3) &
	\ FND8%=T3 &
	\ FNEND &
	\ DEF FNA8$(K$) &
	\ FNA8$=CVT%$(VAL(LEFT(K$,3%)))+CHR$(VAL(RIGHT(K$,5%))) &
	\ FNEND &
	\ DEF FNA7$(K$) &
	\ FNA7$=RIGHT(NUM1$(1000%+CVT$%(K$)),2%)+"."+RIGHT(NUM1$(100%+ASCII(RIGHT(K$,3%))),2%) &
	\ FNEND &
	\ DEF FND7%(K$) &
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
