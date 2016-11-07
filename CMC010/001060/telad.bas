1	  EXTEND
10	! &
	!	Tele-Ad response program &
	! &
	!	Written for John Minor by Dale Handy - February, 1984 &
	!	From a program by Rick Owen - January, 1984 &
	! &
	! &
	!	Features that need to be added: &
	! &
	!		Set VT102 (scope) mode &
	!		Reset terminal correctly &
	!		Delete an ad &
	!		Change an ad &
	!		etc., etc.... &
	! &
	  ON ERROR GOTO 19000	!	First, set error trapping &
	! &
	\ GOSUB 24900		!	Initialize screen format variables &
	\ FILES.OPEN% = 0% &
	\ TOP.LINE% = 5% &
	\ LEFT.COL% = 5% &
	\ PRINT.CH% = 1% &
	\ DESC.FORM$ = "###.  \" + SPACE$(18%) + "\" &
	! &
	! Set 'TELE-AD' as the default files &
	! &
	\ GOSUB 9000 &
	! &
	! &
	\ OPEN "NL:" AS FILE 12%, RECORDSIZE 32% + 32% + 128% &
	! &
	!	Field three files &
	! &
	\ FIELD #12%, 32% AS MAJOR$, 32% AS MINOR$, 128% AS ITEM$ &
	! &
	!	First, the Tele-Ad major category file &
	! &
	\ FIELD #12%, 2% AS A$(1%), 26% AS A$(2%), 4% AS DUMMY$, &
	! &
	!	Now the Tele-Ad minor category file &
	! &
		  2% AS B$(1%), 2% AS B$(2%), 26% AS B$(3%), 2% AS DUMMY$, &
	! &
	!	And finally, the item data file itself: &
	! &
		  2% AS C$(1%),  2% AS C$(2%), 4% AS C$(3%), 4% AS C$(4%), &
		  2% AS C$(5%), 80% AS C$(6%), 8% AS C$(7%), 1% AS C$(8%), &
		  2% AS C$(9%), 23% AS DUMMY$ &
	! &
	\ DIM	  A$(2%),		! Fields in TELMAJ.DA1 &
		  B$(3%),		! Fields in TELMIN.DA1 &
		  C$(9%),		! Fields in TELADS.DAT &
		  D$(5%),		! Dummy array for printing &
		  NEW.DESC$(7%),	! Dummy array for data entry &
		  TIME.ALLOTED%(5%),	! Max run time for each ad type &
		  MAX.DAY%(12%)		! Days in each month &
	! &
	\ READ TIME.ALLOTED%(DUMMY%) FOR DUMMY% = 1% TO 5% &
	\ READ MAX.DAY%(I%) FOR I% = 1% TO 12% &
	\ DATA	  30,			! List for 30 days &
		   7,			! List for  7 days &
		  30,			! Request WANT for 30 days &
		1460,			! List ad until sold &
		  90,			! Dummy to use for setup &
		  31, 28, 31, 30,	! Jan, Feb, Mar, Apr, &
		  31, 30, 31, 31,	! May, Jun, Jul, Aug, &
		  30, 31, 30, 31	! Sep, Oct, Nov, Dec. &

20	! &
	!  Open kb: for delimiterless input &
	! &
	  OPEN "KB:" AS FILE 1%,MODE 8% + 16% + 256% &
	\ PRINT #1%,SET.ANSII$;		!  Set the terminal in ANSII mode &
	! &
	!	Prepare strings to declare a private delimiter &
	! &
	\ TTY.OLD$ = CHR$(6%) + CHR$(16%) + CHR$(0%) + CHR$(255%) + &
		     STRING$(22%-4%, 0%) + CHR$(128%) &
	\ TTY.MNU$ = CHR$(6%) + CHR$(16%) + CHR$(0%) + CHR$(255%) + &
		     STRING$(2%,0%) + CHR$(255%) + STRING$(14%,0%) + CHR$(0%) + &
		     CHR$(128%+8%) + STRING$(6%,0%) &

30	! &
	!	Print menu on screen and find out what the user wants to do &
	! &
	  PRINT #1%, CLEAR.SCREEN$; &
	\ SYST$ = SYS(TTY.MNU$) &
	\ PRINT #1%, FNTITLE$(GROUP$, "MAIN MENU"); &

32	  PRINT #1%, CLEAR.TO.USE$; &
	\ PRINT #1%, FNCURSOR$(10%, 20%); "A - Look up "; GROUP$; &
	\ PRINT #1%, FNCURSOR$(11%, 20%); "B - Print all "; GROUP$; &
	\ PRINT #1%, FNCURSOR$(12%, 20%); "C - Change an existing "; GROUP$ &
	\ PRINT #1%, FNCURSOR$(13%, 20%); "D - Delete a "; GROUP$ &
	\ PRINT #1%, FNCURSOR$(14%, 20%); "E - Enter "; GROUP$; " information"; &
	\ PRINT #1%, FNCURSOR$(16%, 20%); "S - "; ALT.DIRECTORY$; &
	\ PRINT #1%, FNCURSOR$(18%, 20%); "Press BACKSPACE to exit"; &
	\ IF IO.MODE$ = "/RW" &
	  THEN	  FILES.OPEN% = 0% AND (FNC%(2%) + FNC%(4%) + FNC%(6%)) &

35	  PRINT #1%, FNCURSOR$(24%, 1%); CLEAR.TO.EOL$; "Please select the "; &
		  "desired option ."; &
	\ PRINT #1%, FNCURSOR$(24%, 34%); &
	\ OPTION$ = CVT$$(FNINPUT$(1%, ".", 1%), 32%) &

40	! &
	!	Process input &
	! &
	  IF BUFFER$ = "END" &
	  THEN	  V$ = FNMOVING$("Exiting . . .") &
		\ SYST$ = SYS(TTY.OLD$) &
		\ V% = FNX%("TELAD", 25000%, "") &

50	  IF INSTR(1, "ABCDES", OPTION$) = 0% OR OPTION$ = "" &
	  THEN	  PRINT #1%, BEEP$; &
		\ GOTO 35 &

52	  IO.MODE$ = "/RO" &
	! &
	! Set up to do all I/O in read only mode &
	! &

55	  IF OPTION$ = "E" OR OPTION$ = "D" &
	  THEN	  IO.MODE$="/RW" &
		\ FILES.OPEN% = 0% AND (FNC%(2%) + FNC%(4%) + FNC%(6%)) &
	! &
	! If they want to update the files, then they must &
	! get write access to the files &
	! &

57	  GOTO 1000 IF FILES.OPEN% &
	\ FILES.OPEN% = -1% &

60	! &
	!	Now that we know we are going to stay, let's try &
	!	opening the order data file &
	! &
	  V% = FNO%(2%, FILE.PREFIX$ + "MAJ.DA1", IO.MODE$+"/SF", "") &
	\ IF V% = 10% &
	  THEN	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		\ PRINT #1%, "You do not have write access to the Tele-ad file."; &
			"  Please try again later." &
		\ PRINT #1%, FNCURSOR$(24%, 1%); "Press RETURN to continue ."; &
			BACK$; &
		\ V$ = FNINPUT$(1%, ".", 1%) &
		\ PRINT #1%, CLEAR.HELP$; &
		\ GOTO 30 &

70	  IF V% = 5% &
	  THEN	  V% = FNO%(2%, FILE.PREFIX$ + "MAJ.DA1", "/SF/CR:32", "") &
			+ FNC%(2%) &
		\ STOP IF V% &
		\ GOTO 60 &

80	! &
	!	Now that we know we are going to stay, let's try &
	!	opening the order data file &
	! &
	  V% = FNO%(4%, FILE.PREFIX$ + "MIN.DA1", IO.MODE$ + "/SF", "") &
	\ IF V% = 10% &
	  THEN	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		\ PRINT #1%, "You do not have write access to the Tele-ad file."; &
			"  Please try again later." &
		\ PRINT #1%, FNCURSOR$(24%, 1%); "Press RETURN to continue ."; &
			BACK$; &
		\ V$ = FNINPUT$(1%, ".", 1%) &
		\ PRINT #1%, CLEAR.HELP$; &
		\ GOTO 30 &

90	  IF V% = 5% &
	  THEN	  V% = FNO%(4%, FILE.PREFIX$ + "MIN.DA1", "/SF/CR:32", "") &
			+ FNC%(4%) &
		\ STOP IF V% &
		\ GOTO 80 &

100	! &
	!	Now that we know we are going to stay, let's try &
	!	opening the order data file &
	! &
	  V% = FNO%(6%, FILE.PREFIX$ + "ADS.DAT", IO.MODE$, "") &
	\ IF V% = 10% &
	  THEN	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		\ PRINT #1%, "You do not have write access to the Tele-ad file."; &
			"  Please try again later." &
		\ PRINT #1%, FNCURSOR$(24%, 1%); "Press RETURN to continue ."; &
			BACK$; &
		\ V$ = FNINPUT$(1%, ".", 1%) &
		\ PRINT #1%, CLEAR.HELP$; &
		\ GOTO 30 &

110	  IF V% = 5% &
	  THEN	  V% = FNO%(6%, FILE.PREFIX$ + "ADS.DAT", "/CR:16,128", "") &
			+ FNC%(6%) &
		\ STOP IF V% &
		\ GOTO 100 &

1000	! &
	!	Begin option processing &
	! &
	  ON INSTR(1%, "ABCDES", OPTION$) GOTO 2000, 3000, 4000, 5000, &
		  6000, 9900 &
2000	! &
	! Look up an ad - the meat of the whole thing &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "LOOK UP - MAIN CATEGORIES"); &
	\ GOSUB 7000		! First, print the major ad categories &
				! And select the proper category &

2010	  GOSUB 7100		! Now get a selection and process it &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 30 &
	  ELSE	  IF FNS%	! Grouse a little if it's not valid &
		  THEN	  PRINT #1%, BEEP$; &
			\ GOTO 2010 &

2020	  PRINT #1%, FNTITLE$(GROUP$, "LOOK UP - MINOR CATEGORIES"); &
	\ GOSUB 7200		! Print the minor ad categories &

2030	  GOSUB 7300		! And get a selection &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 2000 &
	  ELSE	  IF FNS%	! Grouse some more if not valid &
		  THEN	  PRINT #1%, BEEP$ &
			\ GOTO 2030 &

2040	! &
	! Now that we know what they want, list the ads sequentially &
	! until they found it.  (Ads are in descending order by price) &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "LOOK UP ADS"); &
	\ KEYY$ = IN.MAJOR$ + IN.MINOR$ &
	\ DONE% = FNG%(6%, KEYY$) &
	\ LINE.COUNT%, ITEM.COUNT% = 0% &
	\ PRINT #1%, CLEAR.TO.USE$; &
	\ UNTIL DONE% &
		\ LSET ITEM$ = FNL$ &
		\ GOSUB 2200 IF CVT$%(C$(5%)) = 0% AND ITEM.COUNT% &
		\ UNLESS ITEM.COUNT% &
		  THEN	  NEW.DESC$(0%) = "PRICE = $"+NUM1$(CVT$F(C$(7%))) &
				+ "     " + FNUNMASH.DATE$(CVT$%(C$(4%))) &

2050		  ITEM.COUNT% = ITEM.COUNT% + 1% &
		\ NEW.DESC$(ITEM.COUNT%) = CVT$$(C$(6%)+"", 128%) &
		\ DONE% = FNNEXT%(6%, KEYY$) UNLESS DONE% &
	\ NEXT &
	\ GOTO 2020 IF DONE% = -2% &
	\ GOSUB 2200 IF ITEM.COUNT% &
	\ GOSUB 2300 &
	\ GOTO  2020 &

2200	! &
	! Print an ad on the screen.  First check to see if there is &
	! enough room on the screen. &
	! &
	  GOSUB 2300 IF LINE.COUNT% + ITEM.COUNT% > 17% &
	\ PRINT #1%, FNCURSOR$(LINE.COUNT% + 3% + M.COUNT%, 1%); &
			  NEW.DESC$(M.COUNT%); &
		  FOR M.COUNT% = 0% TO ITEM.COUNT% &
	\ LINE.COUNT% = LINE.COUNT% + 2% + ITEM.COUNT% &
	\ ITEM.COUNT% = 0% &
	\ RETURN &

2300	! &
	! We are going to start a new page.  Stop and prompt first. &
	! &
	  PRINT #1%, FNCURSOR$(23%, 1%); "More "; &
	\ DUMMY$ = FNINPUT$(1%, ".", 1%) &
	\ IF BUFFER$ = "END" &
	  THEN	  DONE% = -2% &
		\ ITEM.COUNT% = 0% &

2310	  PRINT #1%, CLEAR.TO.USE$; &
	\ LINE.COUNT% = 0% &
	\ RETURN &
3000	! &
	! Print all ads - next best thing &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "PRINT ADS - SELECT MAJOR"); &
	\ GOSUB 7000 &

3010	! &
	! Do an input to find what to print next &
	! &
	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		  "Enter selection; RETURN for all; or BACKSPACE to exit "; &
	\ IN.MAJOR$ = FNINPUT$(1%, ".", 3%) &
	\ GOTO 30 IF BUFFER$ = "END" &
	\ IN.MAJOR$ = CVT%$(VAL(IN.MAJOR$)) &
	\ UNLESS FNG%(2%, IN.MAJOR$) AND CVT$%(IN.MAJOR$) &
	  THEN	  LSET MAJOR$ = FNL$ &
	  ELSE	  PRINT #1%, BEEP$; &
		\ GOTO 3010 &

3020	  UNLESS CVT$%(IN.MAJOR$) &
	  THEN	  GOTO  3040 &
	  ELSE	  PRINT #1%, FNTITLE$(GROUP$, "PRINT ADS - SELECT MINOR"); &
		\ GOSUB 7200 &

3030	! &
	! Do an input to find what to print next &
	! &
	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		  "Enter selection; RETURN for all; or BACKSPACE to exit "; &
	\ IN.MINOR$ = FNINPUT$(1%, ".", 3%) &
	\ GOTO 3000 IF BUFFER$ = "END" &
	\ IN.MINOR$ = CVT%$(VAL(IN.MINOR$)) &
	\ IF FNG%(4%, IN.MAJOR$ + IN.MINOR$) AND CVT$%(IN.MINOR$) &
	  THEN	  PRINT #1%, BEEP$; &
		\ GOTO 3030 &

3040	  PRINT #1%, FNTITLE$(GROUP$, "PRINT ADS"); &
	\ PRINT #1%, FNCURSOR$(15%, 1%); "Turn your printer on, and press "; &
		  "'Y' when ready (BACKSPACE to exit) "; &
	! &
	! See if they really want to go through with it &
	! &
	\ V$ = FNINPUT$(1%, ".", 1%) &
	\ GOTO 3020 IF BUFFER$ = "END" &
	\ GOTO 3040 UNLESS V$ = "Y" OR V$ = "y" &
	! &
	! Yep, they sure do, now find the first ad &
	! &
	\ DUMMY% = FNG%(6%, IN.MAJOR$ + IN.MINOR$) &
	! &
	! Set media copy of VT if on printer port thereof &
	! and set up some variables to check for new categories &
	! &
	\ PRINT #1%, PASS.THRU.ON$; IF PRINT.CH% = 1% &
	\ D$(I%) = "" FOR I% = 1% TO 5% &
	! &
	! Now print until we reach the end of the ads. &
	! &
	\ DONE% = 0% &
	\ UNTIL DONE% &
		\ LSET ITEM$ = FNL$ &
		! &
		! New page if over 51 lines on this page, or a new major &
		! category. &
		! &
		\ IF (C$(1%) <> IN.MAJOR$ AND CVT$%(IN.MAJOR$)) &
		  OR (C$(2%) <> IN.MINOR$ AND CVT$%(IN.MINOR$)) &
		  THEN	  DONE% = -1% &
			\ GOTO 3100 &

3050		  IF D$(1%) <> C$(1%) OR D$(2%) <> C$(2%) &
			  OR (LINE.COUNT% > 51% AND CVT$%(C$(5%)) = 0%) &
		  THEN	  V% = FNG%(2%, C$(1%)) &
			\ LSET MAJOR$ = FNL$ &
			\ PRINT #PRINT.CH%, TOP.OF.FORM$; SET.5$; &
				  "MAJOR CAT #"; &
			\ PRINT #PRINT.CH%  USING DESC.FORM$, CVT$%(A$(1%)), &
				  A$(2%) &
			\ D$(1%) = A$(1%) + "" &
			\ D$(2%) = "" &
			\ LINE.COUNT% = 4% &

3060		! &
		! Indicate if we are in a new minor category &
		! &
		  IF D$(2%) <> C$(2%) &
		  THEN	  V% = FNG%(4%, C$(1%) + C$(2%)) &
			\ LSET MINOR$ = FNL$ &
			\ PRINT #PRINT.CH%, NEW.LINE$; NEW.LINE$; "MINOR CAT #"; &
			\ PRINT #PRINT.CH%  USING DESC.FORM$, CVT$%(B$(2%)), &
				  B$(3%) &
			\ D$(2%) = B$(2%) + "" &
			\ LINE.COUNT% = LINE.COUNT% + 3% &

3070		! &
		! Must be a new ad if Sequence number is zero, so blank line &
		! and print price and insert date &
		! &
		  IF C$(5%) = CVT%$(0%) &
		  THEN	  PRINT #PRINT.CH%, NEW.LINE$; SET.5$; "PRICE: "; &
			\ PRINT #PRINT.CH%  USING "$$#,###,###.##    \      \", &
				  CVT$F(C$(7%)); FNUNMASH.DATE$(CVT$%(C$(4%))) &
			\ LINE.COUNT% = LINE.COUNT% + 2% &

3080		! &
		! Print ad description &
		! &
		  PRINT #PRINT.CH%, SET.10$; C$(6%) &
		\ LINE.COUNT% = LINE.COUNT% + 1% &
		\ DONE% = FNN%(6%) &

3100	  NEXT &
	\ PRINT #PRINT.CH%, TOP.OF.FORM$; &
	\ PRINT #PRINT.CH%, PASS.THRU.OFF$; IF PRINT.CH% = 1% &
	\ IF CVT$%(IN.MINOR$) &
	  THEN	  GOTO 3020 &
	  ELSE	  IF CVT$%(IN.MAJOR$) &
		  THEN	  GOTO 3000 &
		  ELSE	  GOTO 30 &
4000	! &
	! Change an ad - must have been some error in data entry &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "CHANGE - MAJOR CATEGORIES"); &

4999	  goto 30 &

5000	! &
	! Delete an ad - this will take some thought &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "DELETE - MAJOR CATEGORIES"); &
	\ GOSUB 7000 		! First, print the major categories &

5010	  GOSUB 7100		! Get a selection and process... &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 30 &
	  ELSE	  IF IN.MAJOR$ = CVT%$(0%) &
		  THEN	  GOTO 5010 &
		  ELSE	  IF FNS% &
			  THEN	  PRINT #1%, BEEP$; &
				\ GOTO 5010 &

5020	  PRINT #1%, FNTITLE$(GROUP$, "DELETE - MINOR CATEGORIES"); &
	\ GOSUB 7200		! Print the minor categories &

5030	  GOSUB 7300		! and get a selection &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 5000 &
	  ELSE	  IF IN.MINOR$ = CVT%$(0%) &
		  THEN	  GOTO 5030 &
		  ELSE	  IF FNS% &
			  THEN	  PRINT #1%, BEEP$; &
				\ GOTO 6030 &

5040	! &
	! Now step thru ads, finding one to delete.... &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "DELETE AN AD"); &
	\ KEYY$ = IN.MAJOR$ + IN.MINOR$ &
	\ DONE% = FNG%(6%, KEYY$) &
	\ ITEM.COUNT% = 0% &
	\ UNTIL DONE% &
		\ LSET ITEM$ = FNL$ &
		\ LAST.ONE% = FNR(6%) &
		\ IF CVT$%(C$(5%)) = 0% &
		  THEN	  GOSUB 5600 IF ITEM.COUNT% &
			\ FIRST.ONE% = LAST.ONE% &
			\ PRINT #1%, CLEAR.TO.USE$; FNCURSOR$(3,0%); &
				  "PRICE = $"; NUM1$(CVT$F(C$(7%))); &
				  "     "; FNUNMASH.DATE$(CVT$%(C$(4%))) &

5050		  PRINT #1%, CVT$$(C$(6%), 128%) &
		\ ITEM.COUNT% = ITEM.COUNT% + 1% &
		\ DONE% = FNNEXT%(6%, KEYY$) UNLESS DONE% &
	\ NEXT &
	\ LAST.ONE% = LAST.ONE% + 1% &
	\ GOSUB 5600 IF ITEM.COUNT% &
	\ GOTO 5020 &

5600	! &
	! See if he really wants to delete this one &
	! &
	  PRINT #1%, FNCURSOR$(23%, 10%); "Delete? (Y/N)  "; &
	\ V$ = FNINPUT$(1%, ".", 1%) &
	\ IF V$ = "Y" OR V$ = "y" &
	  THEN	  LAST.ONE% = FNR(6%) &
		\ DUMMY% = FNG%(-6%, NUM1$(FIRST.ONE%)) &
		\ WHILE FNR(6%) < LAST.ONE% &
			\ DUMMY% = FND%(6%, "") &
			\ DUMMY% = FNN%(6%) &
		\ NEXT &
		! &
		! Whew, done with that one &
		! &

5610	! &
	! Let's go home &
	! &
	  RETURN &
6000	! &
	! Enter new ads - better get this right &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "ENTER - MAJOR CATEGORIES"); &
	\ GOSUB 7000		! First, print the major ad categories &
				! And select the proper category &

6010	  GOSUB 7100		! Now get a selection and process it &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 30 &
	  ELSE	  IF IN.MAJOR$ = CVT%$(0%) &
		  THEN	  GOTO 6010 &
		  ELSE	  IF FNS%	! Add a new major if it's not valid &
			  THEN	  GOSUB 6200 &
				\ GOTO  6000 &

6020	  PRINT #1%, FNTITLE$(GROUP$, "ENTER - MINOR CATEGORIES"); &
	\ GOSUB 7200		! Print the minor ad categories &

6030	  GOSUB 7300		! And get a selection &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 6000 &
	  ELSE	  IF IN.MINOR$ = CVT%$(0%) &
		  THEN	  GOTO 6030 &
		  ELSE	  IF FNS%	! Add a new minor if not valid &
			  THEN	  GOSUB 6300 &
				\ GOTO  6020 &

6040	! &
	! He wants to add an ad.  Well, here goes nothing... &
	! &
	  PRINT #1%, FNTITLE$(GROUP$, "ENTER AN AD"); &
	\ PRINT #1%, CLEAR.TO.USE$; FNCURSOR$(3%, 10%); &
	\ PRINT #1%  USING DESC.FORM$ + "    " + DESC.FORM$, &
		  CVT$%(A$(1%)), A$(2%), CVT$%(B$(2%)), B$(3%); &

6042	  PRINT #1%, FNCURSOR$(7%, 5%); "Price  "; &
	\ PRICE$ = FNINPUT$(1%, ".", 9%) &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 6020 &
	  ELSE	  LSET C$(1%) = A$(1%) &
		\ LSET C$(2%) = B$(2%) &
		\ LSET C$(3%) = FNPRICE$(999999999.0 - VAL(PRICE$)) &
		\ LSET C$(7%) = CVTF$(VAL(PRICE$)) &

6044	  PRINT #1%, FNCURSOR$(7%, 40%); "Entry code (A, B, C, D)  "; &
	\ ENTRY.TYPE$ = FNINPUT$(1%, ".", 1%) &
	\ IF BUFFER$ = "END" &
	  THEN	  GOTO 6040 &
	  ELSE	  ENTRY.TYPE% = INSTR(1%, "ABCD", ENTRY.TYPE$) &
		\ UNLESS ENTRY.TYPE% &
		  THEN	  PRINT #1%, BELL$; &
			\ GOTO 6044 &

6046	  SEQ.NO% = 0% &
	\ PRINT #1%, FNCURSOR$(9%, 1%); "Enter description below.  "; &
		  "Leave a blank line or press BACKSPACE to enter." &

6050	  PRINT #1%, FNCURSOR$(11%+SEQ.NO%, 1%); &
	\ NEW.DESC$(SEQ.NO%) = FNINPUT$(1%, ".", 79%) &
	\ IF BUFFER$ <> "END" AND LEN(NEW.DESC$(SEQ.NO%)) &
	  THEN	  SEQ.NO% = SEQ.NO% + 1% &
		\ IF SEQ.NO% < 8% &
		  THEN	  6050 &

6060	  IF SEQ.NO% &
	  THEN	  PRINT #1%, FNCURSOR$(23%, 1%); "Is this correct?  "; &
		\ V$ = FNINPUT$(1%, ".", 1%) &
		\ IF V$ = "N" OR V$="n" &
		  THEN	  GOTO 6020 &
		  ELSE	  IF V$ <> "Y" AND V$ <> "y" &
			  THEN	  GOTO 6060 &
			  ELSE	  PRINT #1%, FNCURSOR$(23%, 55%); "Working..."; &
				\ GOSUB 6400 &

6080	  GOTO 6020 &
6200	! &
	! Add a new major category &
	! &
	  LSET A$(1%) = IN.MAJOR$ &
	\ PRINT #1%, CLEAR.TO.USE$; &
	\ PRINT #1%, FNCURSOR$(7%, 10%); "New number  "; &
	\ PRINT #1%  USING "###", CVT$%(A$(1%)); &

6210	  PRINT #1%, FNCURSOR$(9%, 10%); "Description "; &
	\ NEW.DESC$ = FNINPUT$(1%, ".", 26%) &
	\ IF BUFFER$ <> "END" AND LEN(NEW.DESC$) &
	  THEN	  LSET A$(2%) = NEW.DESC$ &
		\ V% = FNA%(2%, MAJOR$) &

6299	! &
	! Exit point for above routines &
	! &
	  RETURN &

6300	! &
	! Add a new minor category &
	! &
	  LSET B$(1%) = A$(1%) &
	\ LSET B$(2%) = IN.MINOR$ &
	\ PRINT #1%, CLEAR.TO.USE$; &
	\ PRINT #1%, FNCURSOR$(3%, 15%); &
	\ PRINT #1%  USING DESC.FORM$, CVT$%(A$(1%)), A$(2%); &
	\ PRINT #1%, FNCURSOR$(7%, 10%); "New number  "; &
	\ PRINT #1%  USING "###", CVT$%(B$(2%)); &

6310	  PRINT #1%, FNCURSOR$(9%, 10%); "Description "; &
	\ NEW.DESC$ = FNINPUT$(1%, ".", 26%) &
	\ IF BUFFER$ <> "END" AND LEN(NEW.DESC$) &
	  THEN	  LSET B$(3%) = NEW.DESC$ &
		\ V% = FNA%(4%, MINOR$) &

6399	! &
	! Exit point for above routines &
	! &
	  RETURN &

6400	! &
	! Put description into file &
	! &
	  LSET C$(4%) = FNGET.DATE.TIME$ &
	\ LSET C$(8%) = CHR$(ENTRY.TYPE%) &
	\ LSET C$(9%) = CVT%$(FNNEXT.DATE%(CVT$%(C$(4%)), &
		  TIME.ALLOTED%(ENTRY.TYPE%))) &
	\ FOR INC% = 0% TO SEQ.NO% - 1% &
		\ LSET C$(5%) = CVT%$(INC%) &
		\ LSET C$(6%) = NEW.DESC$(INC%) &
		\ DUMMY% = FNA%(6%, ITEM$) &
	\ NEXT INC% &
	\ RETURN &
7000	! &
	! Show list of major ad categories &
	! &
	  V% = FNG%(2%, "") &
	\ PRINT #1%, CLEAR.TO.USE$; &
	\ LINE.NO% = TOP.LINE% &
	\ COL.NO% = LEFT.COL% &
	\ UNTIL FNS% &
		\ LSET MAJOR$ = FNL$ &
		\ COL.NO%  = 40% IF LINE.NO% > 20% &
		\ LINE.NO% = TOP.LINE% IF LINE.NO% > 20% &
		\ PRINT #1%, FNCURSOR$(LINE.NO%, COL.NO%); &
		\ PRINT #1% USING DESC.FORM$, &
			  CVT$%(A$(1%)), A$(2%); &
		\ LINE.NO% = LINE.NO% + 2% &
		\ DUMMY% = FNN%(2%) &
	\ NEXT &
	\ RETURN &

7100	! &
	! Do an input to find what to look up next &
	! &
	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		  "Enter selection or BACKSPACE to exit "; &
	\ IN.MAJOR$ = FNINPUT$(1%, ".", 3%) &
	\ IF BUFFER$ <> "END" &
	  THEN	  IN.MAJOR$ = CVT%$(VAL(IN.MAJOR$)) &
		\ DUMMY% = FNG%(2%, IN.MAJOR$) &
		\ LSET MAJOR$ = FNL$ &

7199	  RETURN &

7200	! &
	! Show list of minor ad categories &
	! &
	  CHECK% = FNG%(4%, A$(1%)) &
	\ PRINT #1%, CLEAR.TO.USE$; &
	\ LINE.NO% = TOP.LINE% &
	\ COL.NO% = LEFT.COL% &
	\ PRINT #1%, FNCURSOR$(3%, 25%); &
	\ PRINT #1%  USING DESC.FORM$, CVT$%(A$(1%)), A$(2%); &
	\ UNTIL CHECK% &
		\ LSET MINOR$ = FNL$ &
		\ COL.NO%  = 40% IF LINE.NO% > 20% &
		\ LINE.NO% = TOP.LINE% IF LINE.NO% > 20% &
		\ PRINT #1%, FNCURSOR$(LINE.NO%, COL.NO%); &
		\ PRINT #1%  USING "###.  \" + SPACE$(18%) + "\", &
			  CVT$%(B$(2%)), B$(3%); &
		\ LINE.NO% = LINE.NO% + 2% &
		\ CHECK% = FNNEXT%(4%, A$(1%)) &
	\ NEXT &
	\ RETURN &

7300	! &
	! Do an input to find what to look up next &
	! &
	  PRINT #1%, FNCURSOR$(23%, 1%); CLEAR.TO.EOS$; &
		  "Enter selection or BACKSPACE to exit "; &
	\ IN.MINOR$ = FNINPUT$(1%, ".", 3%) &
	\ IF BUFFER$ <> "END" &
	  THEN	  IN.MINOR$ = CVT%$(VAL(IN.MINOR$)) &
		\ DUMMY% = FNG%(4%, IN.MAJOR$ + IN.MINOR$) &
		\ LSET MINOR$ = FNL$ &

7399	  RETURN &
9000	! &
	! Set default files &
	! &
	! First, close all ISAM files, just in case &
	! &
	  DUMMY% = FNC%(2%) + FNC%(4%) + FNC%(6%) &
	\ FILES.OPEN% = 0% &
	! &
	! Then change &
	! &
	\ IF GROUP$ <> "TELE-AD" &
	  THEN	  GROUP$ = "TELE-AD" &
		\ ALT.DIRECTORY$ = "Select SERVICE Directory" &
		\ FILE.PREFIX$ = "TEL" &
		\ RETURN &

9010	! &
	! Okay, he wants to switch to specialized directory &
	! &
	  GROUP$ = "SERVICE" &
	\ ALT.DIRECTORY$ = "Select TELE-AD Directory" &
	\ FILE.PREFIX$ = "SPC" &
	\ RETURN &

9900	  GOSUB 9000 &
	\ GOTO 30 &
10000	! &
	!	Return to pcsmen &
	! &
	  V% = FNX%("TELAD", 0%, "") &

19000	!-------------------------------------------------------------- &
	!	e r r o r   p r o c e s s i n g   r o u t i n e       | &
	!-------------------------------------------------------------- &

19010	  IF ERR <> 52% &
	  THEN	  GOTO 19200 &
	  ELSE	  PRINT #1%, BEEP$; &

19020	  IF ERL = 7100% OR ERL = 7300% &
	  THEN	  DUMMY% = FNG%(2%, "") + FNN%(-2%) &
		\ IN.MINOR$ = CVT%$(0%) &
		\ IN.MAJOR$ = IN.MINOR$ IF ERL = 7100% &
		\ RESUME 7199 &

19030	  RESUME 3010 IF ERL = 3010% &
	\ RESUME 3030 IF ERL = 3030% &
	\ RESUME 6042 IF ERL = 6042% &
	\ RESUME 6299 IF ERL = 6200% OR ERL = 6300% &

19200	! &
	! Not an invalid number &
	! &

19999	  PRINT #1%, "Error stop =";ERR &
	\ PRINT #1%, "Error line =";ERL &
	\ PRINT #1%, "DO NOT PROCEED!  FIND HELP NOW!" &
	\ V$ = SYS(CHR$(5%)) &
24000	!************************************************************ &
	! Screen Formatting Function / Variable Definition Section  | &
	!************************************************************ &
	! &
	!-------------------------------------------------------------- &
	!	Function to input data using block mode               | &
	!-------------------------------------------------------------- &
	  DEF* FNINPUT$(CH%, FILL.CHAR$, INPUT.LEN%) &
		\ PRINT #CH%, STRING$(INPUT.LEN%, ASCII(FILL.CHAR$)); &
			  STRING$(INPUT.LEN%, 8%); &
		\ PRINT #CH%, RECORD 256%, CHR$(0%+INPUT.LEN%)+FILL.CHAR$; &
		\ GET #CH% &
		\ FIELD #CH%, RECOUNT AS BUFFER$ &
		\ IF ASCII(BUFFER$)=128% &
		  THEN	  BUFFER$="END"   IF MID(BUFFER$,2%,4%)="[21~" &
			\ BUFFER$="ABORT" IF MID(BUFFER$,2%,4%)="[19~" &
			\ BUFFER$="LAST"  IF MID(BUFFER$,2%,3%)="[5~" &
			\ BUFFER$="NEXT"  IF MID(BUFFER$,2%,3%)="[6~" &
			\ BUFFER$="UP"    IF MID(BUFFER$,2%,2%)="[A" &
			\ BUFFER$="DOWN"  IF MID(BUFFER$,2%,2%)="[B" &
			\ BUFFER$="RIGHT" IF MID(BUFFER$,2%,2%)="[C" &
			\ BUFFER$="LEFT"  IF MID(BUFFER$,2%,2%)="[D" &
			\ BUFFER$="DO"    IF MID(BUFFER$,2%,4%)="[29~" &

24010		  BUFFER$="END"   IF ASCII(BUFFER$)=8% &
		\ BUFFER$="ABORT" IF ASCII(BUFFER$)=23% &
		\ FNINPUT$=CVT$$(BUFFER$,4%) &
	\ FNEND &

24020	!-------------------------------------------------------------- &
	!	Function to do direct cursor positioning              | &
	!-------------------------------------------------------------- &
	  DEF* FNCURSOR$(ROW%,COL%) = CHR$(155%) + "[" + NUM1$(ROW%) + &
		";" + NUM1$(COL%) + "H" &
	!-------------------------------------------------------------- &
	!	Function to clear the screen and print a message      | &
	!-------------------------------------------------------------- &
	\ DEF* FNMOVING$(MSG$) &
		\ PRINT #1%, CLEAR.SCREEN$; FNCURSOR$(24%, 1%); MSG$; &
	\ FNEND &

24030	!---------------------------------------------------------------- &
	!	Function to convert date and time to integer strings	| &
	!---------------------------------------------------------------- &
	  DEF* FNGET.DATE.TIME$ = &
		 CVT%$(VAL(LEFT(DATE$(0%), 2%)) &
			+ (INSTR(1%, "!!JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", &
				  CVT$$(MID(DATE$(0%), 4%, 3%), -1%)) / 3%) * 32% &
			+ FND8%(VAL(RIGHT(DATE$(0%), 8%))) * 512%) &
		+ CVT%$(TIME(0%)/5%) &
	! &
	!	Convert real to integer &
	! &
 	\ DEF* FND8%(QQ9)=QQ9 &
	! &
	!	Convert integer date to string &
	! &
	\ DEF FNUNMASH.DATE$(QQ9%) &
		  = RIGHT(NUM1$((QQ9% AND 15% * 32%) / 32% + 100%), 2%) + "." &
		  + RIGHT(NUM1$((QQ9% AND 31%) + 100%), 2%) + "." &
		  + RIGHT(NUM1$(((SWAP%(QQ9%) AND 254%) / 2%) + 100%), 2%) &

24040	!---------------------------------------------------------------- &
	!	Get next date in interval				| &
	!---------------------------------------------------------------- &
	  DEF* FNNEXT.DATE%(CURRENT%, INC%) &
		\ DUMMY% = (CURRENT% AND 31%) + INC% &
		\ MONTH% = (CURRENT% AND 15% *32%) / 32% &
		\ YEAR%  = (SWAP%(CURRENT%) AND 254%) / 2% &

24042		  IF DUMMY% > MAX.DAY%(MONTH%) &
		  THEN	  DUMMY% = DUMMY% - MAX.DAY%(MONTH%) &
			\ MONTH% = MONTH% + 1% &
			\ IF MONTH% < 13% &
			  THEN	  GOTO 24042 &
			  ELSE	  MONTH% = 1% &
				\ YEAR% = YEAR% + 1% &
				\ GOTO 24042 &

24044		  FNNEXT.DATE% = YEAR% * 512% + MONTH% * 32% + DUMMY% &
	\ FNEND &

24060	!---------------------------------------------------------------- &
	!	Print Title line for screen				| &
	!---------------------------------------------------------------- &
	  DEF* FNTITLE$(GROUP$, TITLE$) &
		\ TITLE$ = GROUP$ + SPACE$(33% - LEN(TITLE$)/2%) &
			+ TITLE$ &
		\ FNTITLE$ = CLEAR.SCREEN$ + FNCURSOR$(1%, 1%) + SET.RV$ &
			+ TITLE$ + SPACE$(71%-LEN(TITLE$)) &
			+ DATE$(0%) + EXIT.RV$ &
	\ FNEND &

24070	!---------------------------------------------------------------- &
	!	Set up price string for sorting				| &
	!---------------------------------------------------------------- &
	  DEF* FNPRICE$(QQN) = CVT%$(QQN/32767%) &
		+ CVT%$(QQN - INT(QQN/32767%)*32767%) &

24080	!-------------------------------------------------------------- &
	!	Function to get the next record on channel C% that    | &
	!	has K$ at the beginning                               | &
	!-------------------------------------------------------------- &
	  DEF* FNNEXT%(C%,K$) &
		\ C% = FNN%(C%) &
		\ IF C% = 0% &
		  THEN	  FNNEXT% = NOT(LEFT(FNL$,LEN(K$)) = K$) &
		  ELSE	  FNNEXT% = C% &

24090	  FNEND &

24900	!-------------------------------------------------------------- &
	!	Screen formatting variables                           | &
	!							      | &
	!	Executed as a subroutine			      | &
	!-------------------------------------------------------------- &
	  CLEAR.SCREEN$ = CHR$(155%) + "[2J" &
	\ CLEAR.TO.EOS$ = CHR$(155%) + "[0J" &
	\ CLEAR.TO.EOL$ = CHR$(155%) + "[0K" &
	\ SET.RV$       = CHR$(155%) + "[7m" &
	\ EXIT.RV$      = CHR$(155%) + "[0m" &
	\ PASS.THRU.ON$ = CHR$(155%) + "[5i" &
	\ PASS.THRU.OFF$= CHR$(155%) + "[4i" &
	\ SET.10$       = CHR$(155%) + "[0w" &
	\ SET.5$        = CHR$(155%) + "[6w" &
	\ TOP.OF.FORM$  = CHR$( 12%) + CHR$(10%) + CHR$(10%) + CHR$(10%) &
	\ NEW.LINE$     = CHR$( 10%) &
	\ BEEP$         = CHR$(7%) &
	\ SET.ANSII$    = CHR$(155%) + "<" &
	\ CLEAR.TO.USE$	= FNCURSOR$(2%,1%) + CLEAR.TO.EOS$ &
	\ CLEAR.HELP$   = FNCURSOR$(23%,1%) + CLEAR.TO.EOS$ &
	\ BACK$         = CHR$(8%) &
	\ RETURN &

25000	! &
	! Logout of system &
	! &

32767	  SYST$ = SYS(TTY.OLD$) &
	\ V$ = SYS(CHR$(5%)) &
	\ END
