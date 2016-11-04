1	  ! &
	  ! Program name: [1,8]CONPRC		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 23-Jun-87 at 10:49 AM by UNBAC Version 1
10	  !
20	  ON ERROR GOTO 19000
50	  DIM A%(30%), B%(30%)
100	  F9$="PR????.??T"
1000	  !
1010	  CHANGE SYS(CHR$(6%)+CHR$(-10%)+F9$) TO A%
1020	  A%(0%)=30% &
	\ A%(1%)=6% &
	\ A%(2%)=17% &
	\ A%(3%)=C% AND 255% &
	\ A%(4%)=C% AND SWAP%(255%)
1030	  ON ERROR GOTO 19000 &
	\ CHANGE A% TO A$ &
	\ A1$=SYS(A$) &
	\ CHANGE A1$ TO A% &
	\ O1$=MID(A1$,17%,6%)
1040	  F$=RAD$(A%(7%)+SWAP%(A%(8%)))+RAD$(A%(9%)+SWAP%(A%(10%)))+"."+RAD$( &
		A%(11%)+SWAP%(A%(12%))) &
	\ O2$=FNO0$(LEFT(F$,9%)+"1") &
	\ O3$=FNO0$("TX"+MID(F$,3%,7%)+"T") &
	\ O4$=FNO0$("TX"+MID(F$,3%,7%)+"1")
1050	  GOTO 1140 IF FNO%(1%,F$,"/RO","") &
 !	\ V$=SYS(CHR$(6%)+CHR$(-11%)+CHR$(1%)+O1$) &
 !	\ V$=SYS(CHR$(6%)+CHR$(-11%)+CHR$(2%)+O2$) &
 	\ PRINT "DATE: ";MID(F$,3%,2%);".";MID(F$,5%,5%) &
 !		"		LAST ACCESSED ";DATE$(A%(17%)+SWAP%(A%(18%))) &
	\ IF FNO%(3%,"TX"+RIGHT(F$,3%),"/RO","") THEN &
		  PRINT "      NO TAX FILE EXISTS." &
		\ GOTO 1120
1080	!  V$=SYS(CHR$(6%)+CHR$(-11%)+CHR$(3%)+O3$) &
 !	\ V$=SYS(CHR$(6%)+CHR$(-11%)+CHR$(4%)+O4$) &
 !	\ PRINT "      TAX FILE EXISTS.	LAST ACCESSED ";
1100 !	  PRINT DATE$(SWAP%(CVT$%(O3$)))
1110	  !
1120	  UNLESS FNG%(3%,"") THEN &
		  F%=ASCII(MID(FNL$,126%,1%)) &
		\ PRINT "      UPDATED TO MASTER FILE" IF F% AND 1% &
		\ PRINT "      TRANSFERRED TO CKDATA" IF F% AND 2% &
		\ PRINT "      UPDATED TO HOUR" IF F% AND 4%
1130	  PRINT &
	\ V%=FNC%(1%)+FNC%(3%)
1140	  C%=C%+1% &
	\ GOTO 1010
10000	  !
10010	  PRINT &
	\ PRINT &
	\ V%=FNX%("",0%,"") &
	\ GOTO 32767
14000	  DEF FNO0$(X$) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+X$) TO B% &
	\ B%(0%)=30% &
	\ B%(1%)=6% &
	\ B%(2%)=17% &
	\ B%(3%),B%(4%)=0% &
	\ CHANGE B% TO B$ &
	\ B$=SYS(B$) &
	\ FNO0$=MID(B$,17%,6%)
14010	  FNEND
19000	  !
19010	  IF ERR=5% AND ERL=1030% THEN &
		  RESUME 10000
19020	  IF ERL=14000% THEN &
		  FNO0$=STRING$(6%,0%) &
		\ RESUME 14010
19999	  ON ERROR GOTO 0
32767	  END
