10	  ! Program name: EXTRA		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 19-Jan-89 at 10:31 AM by UNBAC Version 1 &
	! &
	! Used to create new version of EXAMHE
20	  !
30	  ON ERROR GOTO 19000
40	  DIM INFO%(50%)
50	  OPEN "KB:" AS FILE 1%
60	  XFORMAT$="\                  \   ##,### ##,### ! ### \        \"
1020 !	  INPUT #1%, "Output to => ";OUT.FILE$ &
	  OUT.FILE$="KB:" IF OUT.FILE$="" &
	\ OPEN OUT.FILE$ FOR OUTPUT AS FILE 10%
2000	  !
2010	  PRINT "File specification => "; &
	\ INPUT LINE #1%, IN.FILE$ &
	\ IN.FILE$=CVT$$(IN.FILE$,-1%) &
	\ GOTO 32767 IF IN.FILE$=""
2030	  GOSUB 6000 &
	\ PPN%=-1% &
	\ WILD.PPN%=INSTR(1%,IN.FILE$,"(*,*)") &
	\ IF WILD.PPN%=0% THEN &
		  X%=INSTR(1%,IN.FILE$,"(") &
		\ COMMA%=INSTR(1%,IN.FILE$,",") &
		\ EN.PAR%=INSTR(1%,IN.FILE$,")") &
		\ PROJ$=CHR$(VAL(MID(IN.FILE$,X%+1%,COMMA%-X%-1%))) &
		\ PROG$=CHR$(VAL(MID(IN.FILE$,COMMA%+1%,EN.PAR%-COMMA%-1%))) &
		\ GOTO 2050
2040	  PPN%=PPN%+1% &
	\ CHANGE SYS(CHR$(6%)+CHR$(25%)+CHR$(PPN%)+CHR$(SWAP%(PPN%))+CHR$( &
		255%)+CHR$(255%)+STRING$(30%,0%)) TO INFO% &
	\ PROJ$=CHR$(INFO%(6%)) &
	\ PROG$=CHR$(INFO%(5%))
2050	  V$=SYS(CHR$(6%)+CHR$(-23%)+IN.FILE$) &
	\ FILE$=MID(V$,7%,6%) &
	\ INDEX%=-1%
2060	  INDEX%=INDEX%+1% &
	\ INFO$=SYS(CHR$(6%)+CHR$(17%)+CHR$(INDEX%)+CHR$(SWAP%(INDEX%))+PROG$ &
		+PROJ$+FILE$+STRING$(19%,0%)) &
	\ CHANGE INFO$ TO INFO%
2065	  L.DATE$=DATE$(SWAP%(CVT$%(RIGHT(INFO$,17%)))) &
	\ ORIGIONAL$=MID(INFO$,17%,6%) &
	\ KEY.FILE$="["+NUM1$(INFO%(6%))+","+NUM1$(INFO%(5%))+"]"+RAD$(INFO%( &
		7%)+SWAP%(INFO%(8%)))+RAD$(INFO%(9%)+SWAP%(INFO%(10%)))+"."+ &
		RAD$(INFO%(11%)+SWAP%(INFO%(12%)))
2070	  DELETED%,TOTAL%=0% &
	\ OPEN KEY.FILE$ FOR INPUT AS FILE 2%, RECORDSIZE 512% &
	\ V$=SYS(CHR$(6%)+CHR$(-11%)+CHR$(2%)+ORIGIONAL$+STRING$(21%,0%)) &
	\ GET #2%, RECORD 1% &
	\ FIELD #2%, 2% AS NO.$,2% AS LE.$,1% AS S.FLAG$, 1% AS A.FLAG$
2075	  TOTAL%=CVT$%(NO.$) &
	\ REC.LEN%=CVT$%(LE.$) &
	\ A.FLAG% = ASCII(A.FLAG$)
3020	  PRINT #10%, USING XFORMAT$, &
		KEY.FILE$, TOTAL%, REC.LEN%, cvt$$(S.FLAG$,4%), A.FLAG%, l.date$ &
	\ CLOSE 2% &
	\ GOTO 2060
5000	  !
5010	  PRINT #10%, "Complete." &
	\ PRINT #10% &
	\ GOTO 2000
5070	  !
6000	  !
6010	  PRINT #10% &
	\ PRINT #10%, "File name             Records Length S Add  Access" &
	\ PRINT #10%, STRING$(79%,45%) &
	\ RETURN
10000	  CLOSE 4% &
	\ CLOSE 5% &
	\ CLOSE 1% &
	\ GOTO 32767
19000	  !
19010	  IF ERR=11% AND ERL=2070% THEN &
		  RESUME 2060
19050	  IF ERR=5% AND ERL=2060% AND WILD.PPN%>0% THEN &
		  RESUME 2040
19055	  IF ERR=5% AND ERL=2060% AND WILD.PPN%=0% THEN &
		  RESUME 5000
19060	  IF ERR=5% AND ERL=2040% THEN &
		  RESUME 5000
19070	  IF ERR=2% THEN &
		  PRINT "Illegal file specification - ";IN.FILE$;"!!"+ &
			STRING$(3%,7%) &
		\ RESUME 2010
19075	  IF ERR=11% AND ERL=5040% THEN &
		  RESUME 5070
19100	  IF ERR=5% AND ERL=3010% THEN &
		  DATA.LEN=0 &
		\ RESUME 3020
19110	  IF ERR=10% AND ERL=2070% THEN &
		  PRINT #10%, KEY.FILE$; " Protection violation" &
		\ RESUME 2060
19980	  ON ERROR GOTO 0
30000	  !
30010	  DEF FNOFFSET%(DES.REC%,CHN%,REC.BLK%) &
	\ BUF.SIZ%=BUFSIZ(CHN%)/512% &
	\ GET.BLK%=DES.REC%/(BUF.SIZ%*REC.BLK%)+1% &
	\ GOTO 30040 IF GOT.BLK%(CHN%)=GET.BLK%
30020	  PUT #CHN%, RECORD BUF.SIZ%*(GOT.BLK%(CHN%)-1%)+1% IF WRITE%(CHN%) &
	\ WRITE%(CHN%)=0%
30030	  GET #CHN%, RECORD GET.BLK%
30040	  GOT.BLK%(CHN%)=GET.BLK% &
	\ FNOFFSET%=(DES.REC%-REC.BLK%*(DES.REC%/REC.BLK%))*(512%/REC.BLK%) &
	\ FNEND
32767	  END
