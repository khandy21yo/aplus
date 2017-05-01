1	%TITLE "Display Product Balances"
	%SBTTL "IC_DSPL_35BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_DSPL_35BALANCE(XPRODUCT$, XLOCATION$, &
		XBAL, WPOS$, FLAG%)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function displays inventory product balances.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_DSPL_35BALANCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_DSPL_35BALANCE
	!	$ DELETE IC_DSPL_35BALANCE.OBJ;*
	!
	! Author:
	!
	!	02/05/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/11/92 - Frank F. Starman
	!		Check for region.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	06/03/92 - Dan Perkins
	!		Display History if flag is set.
	!
	!	06/15/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/10/92 - Frank F. Starman
	!		Added flag 2048
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/01/92 - Kevin Handy
	!		Modified to display an actual message if the ERA
	!		is not defined, instead of crashing with an
	!		untrapped error that means nothing to users.
	!
	!	01/20/94 - Kevin Handy
	!		Added comments to source.
	!
	!	01/31/95 - Kevin Handy
	!		Incrased LOCARRAY from 100 to 200.
	!
	!	01/31/95 - Kevin Handy
	!		Hacked so that will not crash if the period
	!		is undefined in the era file.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 standard.
	!		Change last parameter to ENTER_3CHOICE from "" to 0%.
	!
	!	02/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	02/14/96 - Kevin Handy
	!		Modify to output lost sales even if there are
	!		no regular sales.
	!
	!	12/13/96 - Kevin Handy
	!		Try to fix the "LOST SALE" balance when FLAG%
	!		has bit 1024 set.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/99 - Kevin Handy
	!		Make the definition of IC_READ_35BALANCE return a
	!		LONG instead of a REAL. (kbj)
	!
	!	10/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	01/02/2001 - Kevin Handy
	!		Move calculation of J% to inside SELECT STATEMENT
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_UTL_LOCATION_READ) UTL_LOCATION.CH%

	COM (CH_UTL_CONTROL) IC_CONTROL.CH%

	COM (DSPL_UTL_LOCATION) &
		IC_35HISTORY.CH%(2%), &
		STARTPERIOD%(2%), &
		ENDPERIOD%(2%), &
		START%

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY
	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(2%)

	DECLARE LONG EXIT_STATUS
	DECLARE STRING LOCARRAY(200%)

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	!
	! Set up error trap
	!
	ON ERROR GOTO 19000

	!
	! Assume failure
	!
	EXIT_STATUS = CMC$_NOOPTION

	!
	! Initilize variables
	!
	XBAL = 0.0
	LOOP% = 0%

1000	!
	! Open location file
	!
	IF UTL_LOCATION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

	!
	! Do not open history if not necessary
	!
	GOTO 1030 IF (FLAG% AND 1024%) = 0%

1010	!
	! Open IC control file and get control record
	!
	IF IC_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			CONTINUE NoPeriod IF ERR = 52%
			EXIT HANDLER
		END WHEN
	ELSE
		GOTO 1030
	END IF

	WHEN ERROR IN
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 5%
		CONTINUE NoPeriod IF ERR = 52%
		EXIT HANDLER
	END WHEN

	CLOSE IC_CONTROL.CH%

	!
	! Calculate reporting period
	!
	START_DATE$ = DATE_TODAY

	IF READ_PERIOD("DATE", &
		IC_CONTROL::ERA, &
		PERIOD$, &
		PERIOD_DESCR$, &
		STAT$, &
		START_DATE$, &
		END_DATE$, &
		0%)
	THEN
		!
		! If we can't find it, assume 1st qtr, current month
		!
		STAT$ = "1"
		PERIOD$ = LEFT(DATE_TODAY, 6%)
	END IF

	START% = 4% + VAL%(STAT$)

	STARTPERIOD%(1%) = 1%
	ENDPERIOD%(1%)   = 3% * VAL%(STAT$)
	STARTPERIOD%(2%) = 3% * VAL%(STAT$) - 2%
	ENDPERIOD%(2%)   = 12%

	YYYY$ = LEFT(PERIOD$, 4%)

 !	LOOP% = LOOP% + 1%
 !	LOCARRAY(LOOP%) = "Period: " + PERIOD$ + ", Stat: " + STAT$

1020	!
	! Open History file(s)
	!
	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE 1025 IF ERR = 5%
			EXIT HANDLER
		END WHEN

1025		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")

	NEXT Y%

1030	!
	! Get first location
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ XLOCATION$, &
			REGARDLESS
	USE
		REGION$ = STRING$(LEN(UTL_LOCATION::REGION) + 1%, A"?"B)

		CONTINUE 2000 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	REGION$ = UTL_LOCATION::REGION

2000	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE Choice IF ERR = 11% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	!
	! Get next location
	!
 GetNextRec:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE Choice IF ERR = 11% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	GOTO GetNextRec IF REGION$ <> UTL_LOCATION::REGION

	!
	! Build Array
	!
	JUNK% = IC_READ_35BALANCE(XPRODUCT$, UTL_LOCATION::LOCATION, &
		XBALANCE(,))

	BAL(J%) = 0.0 FOR J% = 1% TO 3%

	FOR Z% = 1% TO 3%
		BAL(Z%) = BAL(Z%) + XBALANCE(Z%, Y%) &
			FOR Y% = 1% TO 3%
	NEXT Z%

 History:
17100	GOTO DisplayArray IF (FLAG% AND 1024%) = 0%

	!
	! Get History record
	!
	QTYQTR(J%) = 0.0 FOR J% = 1% TO 8%
	LOST_SALES = 0.0

	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #0% EQ XPRODUCT$ + &
				UTL_LOCATION::LOCATION, &
				REGARDLESS
		USE
			CONTINUE 17130 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "IC_35HISTORY"
			EXIT HANDLER
		END WHEN

 GetHistRec:
17120		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), REGARDLESS
		USE
			CONTINUE 17130 IF ERR = 11%
			FILENAME$ = "IC_35HISTORY"
			EXIT HANDLER
		END WHEN

		IC_HISTORY(Y%) = IC_35HISTORY

		GOTO 17130 IF IC_HISTORY(Y%)::PRODUCT <> XPRODUCT$ OR &
			IC_HISTORY(Y%)::LOCATION <> UTL_LOCATION::LOCATION

 !		GOTO GetHistRec &
 !			IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::TRANSTYPE, -1%), &
 !			"SA,LS,RT,IS,SE,SP,WR") = 0%
		GOTO GetHistRec &
			IF INSTR(1%, "SA,LS,RT,IS,SE,SP,WR", &
			EDIT$(IC_HISTORY(Y%)::TRANSTYPE, -1%)) = 0%

		!
		! for all ??? in this period
		!
		FOR I% = STARTPERIOD%(Y%) TO ENDPERIOD%(Y%)

			SELECT IC_HISTORY(Y%)::TRANSTYPE

			CASE "LS"
				LOST_SALES = LOST_SALES + &
					IC_HISTORY(Y%)::PQUANTITY(I%)

			CASE ELSE
 !			CASE "SA", "RT"
				!
				! ???
				!.   |-- 0 or 4 --|   |------- Quarter ------|
				!
				J% = 4% * (2% - Y%) + INT((I% - 1%) / 3%) + 1%

				QTYQTR(J%) = QTYQTR(J%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)

			END SELECT

		NEXT I%

		GOTO GetHistRec

17130	NEXT Y%

 DisplayArray:
	!
	! If not all zero's
	!
	TEST_ZERO = 0.0
	TEST_ZERO = TEST_ZERO + ABS(BAL(J%)) FOR J% = 1% TO 3%
	TEST_ZERO = TEST_ZERO + ABS(QTYQTR(J%)) FOR J% = 1% TO 8%

	IF (TEST_ZERO <> 0.0) OR (LOST_SALES <> 0.0)
 !	IF (1%)
	THEN
		AVAILABLE = BAL(1%) + BAL(2%)
		AVAILABLE = 0.0 IF AVAILABLE < 0.0

		LOOP% = LOOP% + 1%

		!
		! Choose format to use
		!
		IF FLAG% AND 1024%
		THEN
			XBAL = LOST_SALES IF UTL_LOCATION::LOCATION = XLOCATION$

			LOCARRAY(LOOP%) = UTL_LOCATION::LOCATION + &
				FORMAT$(BAL(1%), "#########") + &
				FORMAT$(-BAL(2%), "#########") + &
				FORMAT$(AVAILABLE, "#########") + &
				FORMAT$(BAL(3%), "#########") + &
				FORMAT$(QTYQTR(START%), "#######") + &
				FORMAT$(QTYQTR(START% - 1%), "#######") + &
				FORMAT$(QTYQTR(START% - 2%), "#######") + &
				FORMAT$(QTYQTR(START% - 3%), "#######") + &
				FORMAT$(QTYQTR(START% - 4%), "#######")
		ELSE
			XBAL = AVAILABLE IF UTL_LOCATION::LOCATION = XLOCATION$

			LOCARRAY(LOOP%) = UTL_LOCATION::LOCATION + &
				FORMAT$(BAL(1%), "#########") + &
				FORMAT$(-BAL(2%), "#########") + &
				FORMAT$(AVAILABLE, "#########") + &
				FORMAT$(BAL(3%), "#########")

		END IF
	END IF

	GOTO GetNextRec

 Choice:
	!
	! Check if should display
	!
	IF (FLAG% AND 2048%) = 0%
	THEN
		!
		! Choose title to display
		!
		IF FLAG% AND 1024%
		THEN
			TITLE$ = "  Loc    OnHand    Alloc    Avail  OnOrder" + &
				" QtrCur   Qtr2   Qtr3   Qtr4   Qtr5"
		ELSE
			TITLE$ = "  Loc    OnHand    Alloc    Avail  OnOrder"
		END IF

		CHOICE% = ENTR_3CHOICE(SCOPE, WPOS$, "", LOCARRAY(), "", &
			FLAG%, TITLE$, "", 0%)

		IF CHOICE% > 0%
		THEN
			XBAL = VAL(MID(LOCARRAY(I%), 23%, 8%)) &
				UNLESS FLAG% AND 1024%
			EXIT_STATUS = CMC$_NORMAL
		END IF
	END IF

 ExitFunction:
	IC_DSPL_35BALANCE = EXIT_STATUS

	EXIT FUNCTION

 NoPeriod:
	!*******************************************************************
	! Period has not been defined in ERA file
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "ERA not defined in ERA file", 0%)

	EXIT_STATUS = CMC$_ABORT
	RESUME ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	ON ERROR GO BACK

	END FUNCTION
