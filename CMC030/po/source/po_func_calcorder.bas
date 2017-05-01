1	%TITLE "Calculate Reorder Quanity for a Product"
	%SBTTL "PO_FUNC_CALCORDER"
	%IDENT "V3.6a Calico"

	SUB PO_FUNC_CALCORDER(STRING PRODUCT, &
		STRING LOCATION, &
		STRING VENDOR, &
		STRING FORMULA, &
		PO_CALCORDER_CDD PO_CALCORDER_DATA)

	!
	! COPYRIGHT (C) 1994 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
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
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:IC023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_FUNC_CALCORDER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_FUNC_CALCORDER
	!	$ DELETE PO_FUNC_CALCORDER.OBJ;*
	!
	! Author:
	!
	!	05/18/94 - Kevin Handy
	!		Started from code taken from IC_RPRT_FREESTOCK.
	!		Started with methods "AHOS".
	!
	! Modification History:
	!
	!	06/02/94 - Kevin Handy
	!		Added "1".."5" quarter options by ripping code from
	!		IC_RPRT_PRODORDER.
	!
	!	11/11/94 - Kevin Handy
	!		Modified so that minimum quanity looks at the
	!		factors.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/07/95 - Kevin Handy
	!		Handle case where READ_PERIOD returns a question
	!		mark instead of valid quarter.
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add more error handlers
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	COM (CH_IC_BINMAP) &
		IC_BINMAP.CH%

	COM (CH_IC_CONTROL) &
		IC_CONTROL.CH%

	COM (CH_IC_HISTORY) &
		IC_35HISTORY.CH%(2%), &
		START%, &
		STARTPERIOD%(2%), &
		ENDPERIOD%(2%)

	COM (CH_PO_PARTCORSS) &
		PO_PARTCROSS.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION IC_READ_35BALANCE
	EXTERNAL REAL	FUNCTION PC_READ_COST

	!
	! Dimension arrays
	!
	DECLARE REAL QTYQTR(8%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize default result
	!
	PO_CALCORDER_DATA::LOCATION = LOCATION
	PO_CALCORDER_DATA::VENDOR = VENDOR
	PO_CALCORDER_DATA::PRODUCT = PRODUCT
	PO_CALCORDER_DATA::QUANITY(I%) = 0.0 FOR I% = 0% TO 10%
	PO_CALCORDER_DATA::ORDER = 0.0

	SAFETY = 0.0
	ONHAND = 0.0
	ALLOC = 0.0
	ONORDER = 0.0

	LOADFLAG% = 0%

	!
	! Open up a bunch of files
	!
330	IF (IC_BINMAP.CH% = 0%)
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
		USE
			CONTINUE 340 IF ERR = 5%
			FILENAME$ = "IC_BINMAP"
			CONTINUE HelpError
		END WHEN
	END IF

	%PAGE

340	IF (PO_PARTCROSS.CH% = 0%)
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
		USE
			CONTINUE 350 IF ERR = 5%
			FILENAME$ = "PO_PARTCROSS"
			CONTINUE HelpError
		END WHEN
	END IF

350	!

17100	!
	! Look for primary vendor number
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #0% GE PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 17200
	END WHEN

	IF (PO_PARTCROSS::PRODUCT = PRODUCT) AND &
		(PO_CALCORDER_DATA::VENDOR = "")
	THEN
		PO_CALCORDER_DATA::VENDOR = PO_PARTCROSS::VENDOR
	END IF

	!
	! Look up the safety stock amount
	!
17200	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PO_CALCORDER_DATA::PRODUCT + &
				PO_CALCORDER_DATA::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	SAFETY = IC_BINMAP::SAFETY

17300	!
	! Print out one line
	!
	IF IC_READ_35BALANCE(PO_CALCORDER_DATA::PRODUCT, &
		PO_CALCORDER_DATA::LOCATION, BALANCE(,)) AND 1% <> 0%
	THEN
		ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
		ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
		ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)
	END IF

	PO_CALCORDER_DATA::COST = &
		PC_READ_COST(PO_CALCORDER_DATA::PRODUCT, &
		PO_CALCORDER_DATA::LOCATION, DATE_TODAY, "")

 ExitProgram:

17400	FOR FL% = 1% TO 10%

		SELECT MID(FORMULA, FL%, 1%)

		CASE "A"
			PO_CALCORDER_DATA::QUANITY(FL%) = ALLOC
		CASE "H"
			PO_CALCORDER_DATA::QUANITY(FL%) = ONHAND
		CASE "O"
			PO_CALCORDER_DATA::QUANITY(FL%) = ONORDER
		CASE "S"
			PO_CALCORDER_DATA::QUANITY(FL%) = -SAFETY
		CASE "1"
			GOSUB LoadQuarters
			PO_CALCORDER_DATA::QUANITY(FL%) = -QTYQTR(START%)
		CASE "2"
			GOSUB LoadQuarters
			PO_CALCORDER_DATA::QUANITY(FL%) = -QTYQTR(START% - 1%)
		CASE "3"
			GOSUB LoadQuarters
			PO_CALCORDER_DATA::QUANITY(FL%) = -QTYQTR(START% - 2%)
		CASE "4"
			GOSUB LoadQuarters
			PO_CALCORDER_DATA::QUANITY(FL%) = -QTYQTR(START% - 3%)
		CASE "5"
			GOSUB LoadQuarters
			PO_CALCORDER_DATA::QUANITY(FL%) = -QTYQTR(START% - 4%)
		END SELECT

		PO_CALCORDER_DATA::ORDER = &
			PO_CALCORDER_DATA::ORDER - &
			PO_CALCORDER_DATA::QUANITY(FL%)

	NEXT FL%

	!
	! Handle any minimum order quanities
	!
	IF (PO_PARTCROSS::PRODUCT = PRODUCT)
	THEN
		MINIMUM = PO_PARTCROSS::MINQTY * &
			PO_PARTCROSS::FACTOR / PO_PARTCROSS::VENFAC

		IF PO_CALCORDER_DATA::ORDER < MINIMUM AND &
			PO_CALCORDER_DATA::ORDER > 0.0
		THEN
			PO_CALCORDER_DATA::ORDER = MINIMUM
		END IF
	END IF

	EXIT SUB

	%PAGE

	!*******************************************************************
	! Load quarterly totals
	!*******************************************************************

 LoadQuarters:
	RETURN IF LOADFLAG%

	LOADFLAG% = -1%

	GOSUB LoadControl IF IC_35HISTORY.CH%(1%) = 0%

18100	!
	! Get History record
	!
	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #0% EQ PRODUCT + LOCATION, &
				REGARDLESS
		USE
			CONTINUE 18130 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

 GetHistRec:
18120		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), REGARDLESS
		USE
			CONTINUE 18130 IF ERR = 11% OR ERR = 9%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		GOTO 18130 IF IC_35HISTORY::PRODUCT <> PRODUCT

		GOTO 18130 IF IC_35HISTORY::LOCATION <> LOCATION

		GOTO GetHistRec IF INSTR(1%, &
			"SA,LS,RT,IS,SE,SP,WR", &
			IC_35HISTORY::TRANSTYPE) = 0%

		FOR I% = STARTPERIOD%(Y%) TO ENDPERIOD%(Y%)

			J% = 4% * (2% - Y%) + INT((I% - 1%) / 3%) + 1%

			SELECT IC_35HISTORY::TRANSTYPE

			CASE "LS"
				LOST.SALES = LOST.SALES + &
					IC_35HISTORY::PQUANTITY(I%)
			CASE ELSE
				QTYQTR(J%) = QTYQTR(J%) - &
					IC_35HISTORY::PQUANTITY(I%)

			END SELECT

		NEXT I%

		GOTO GetHistRec

18130	NEXT Y%

	RETURN

	%PAGE

	!*******************************************************************
	! Load info from control file, and open all necessary files
	!*******************************************************************

 LoadControl:
18300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	START_DATE$ = DATE_TODAY

	V% = READ_PERIOD("DATE", IC_CONTROL::ERA, PERIOD$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	IF STAT$ = "?"
	THEN
		STAT$ = "0"
		CALL ENTR_3MESSAGE(SCOPE, &
			"Period Not Set Up in ERA table", 0%)
	END IF

	START% = 4% + VAL%(STAT$)

	STARTPERIOD%(1%) = 1%
	ENDPERIOD%(1%)   = 3% * VAL%(STAT$)
	STARTPERIOD%(2%) = 3% * VAL%(STAT$) - 2%
	ENDPERIOD%(2%)   = 12%

	YYYY$ = LEFT(PERIOD$, 4%)

	!
	! Open History file
	!
18320	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE 18325 IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

18325		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")
	NEXT Y%

18330	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END SUB
