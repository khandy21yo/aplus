1	%TITLE "Set Inventory Adjustment Journal"
	%SBTTL "IC_FUNC_JOURSET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_FUNC_JOURSET
	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Converts the product cycle count entry journal to
	!	the inventory adjustment journal (posting form of journal).
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FUNC_JOURSET/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_FUNC_JOURSET
	!	$ DELETE IC_FUNC_JOURSET.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	10/26/92 - Frank Starman
	!		Added IC_WRIT_35BALANCE function.
	!
	!	11/05/92 - Frank Starman
	!		Added methods.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico Coding standards.
	!		Modifications to trap duplicate adjustment entries,
	!		and display a nasty message.
	!
	!	05/05/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	09/05/95 - Kevin Handy
	!		Format closer to 80 columns.
	!		Modified to use HelpMessage routine.
	!
	!	12/06/95 - Kevin Handy
	!		Change question from "Just in adjustment" to
	!		"Adjust to running balance", and changed the
	!		default to "no"
	!
	!	04/11/96 - Kevin Handy
	!		Initialize TEST_LOCATION$ to blanks.
	!
	!	08/13/96 - Kevin Handy
	!		Change 'CASE = x' to 'CASE x' in two places.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	12/11/97 - KEVIN HANDY
	!		Fixed it so that it can handle having more than
	!		one transaction file open at a time.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Changes for new CONTROL field, and new keys
	!
	!	09/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/09/2001 - Kevin Handy
	!		Increased number of transaction files from 100
	!		to 300.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR
	MAP (IC_CYCLEJOUR_INIT)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR_INIT

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"
	DECLARE			UTL_TRANSACCT_CDD	UTL_TRANSACCT_READ

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP(IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_CYCLEJOUR) &
		IC_CYCLEJOUR.CH%, &
		IC_CYCLEJOUR.READONLY%

	COM (CH_IC_JOURCOUNT) &
		IC_JOURCOUNT.CH%, &
		IC_JOURCOUNT.READONLY%

	COM (CH_IC_JOURADJUST) &
		IC_JOURADJUST.CH%, &
		IC_JOURADJUST.READONLY%

	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	!
	! Dimension statements
	!
	DIM IC_TRANSACTION_FILE$(300%)
	DIM IC_TRANSACTION.CH%(48%)		! More open than this is crazy

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION UTL_READ_TRANSACCT
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION IC_WRIT_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

	IC_FUNC_JOURSET = 0%
	IC_CYCLEJOUR_INIT = IC_CYCLEJOUR

300	!
	! Open main file (existing) for modification
	!
	CLOSE IC_JOURADJUST.CH%
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	SCOPE::PRG_ITEM = "CONFIRM"

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
		"", &
		"Cycle adjustment file exist. " + &
		"Confirm recreation - then press <Do> ", &
		"N", 0%, "", "")

	CLOSE IC_JOURADJUST.CH%

	IF INP$ <> "Y"
	THEN
		GOTO ExitFunction
	END IF

310	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.UPD"
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(IC_CONTROL%)

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

325	!
	! Figure out which transaction files exist
	!
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)
	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), 16%, "", "")

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))
	IC_LIST% = 0%

330	!
	! Pick out the intresting ones (Current period and later)
	!
	FOR LOOP% = 1% TO IC_TRANSACTION_FILE%

		YYYYPP$ = MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%)
		IF YYYYPP$ >= IC_CONTROL::PERIOD
		THEN
			IC_LIST% = IC_LIST% + 1%

			!
			! Open up the offending transaction file
			!
			IC_TRANSACTION.CH% = 0%
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
			IC_TRANSACTION.CH%(IC_LIST%) = IC_TRANSACTION.CH%
		END IF

	NEXT LOOP%

	!
	! Paint the background
	!
500	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		6%, &
		40%, &
		SMG_EXPAND%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_EXPAND%, &
		"Adjustment Batch No. " + BATCH_NO$, &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_EXPAND%, "Product # ", 2%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_EXPAND%, &
		SCOPE::SMG_PBID, &
		12%, &
		5% &
	)

	%PAGE

1000	!******************************************************************
	! Adjustment
	!******************************************************************
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! Display message
	!
	IF ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
		"", "Adjust to running balance (Y/N)", "N", 0%, "", "") = "Y"
	THEN
		METHOD$ = "J"
	ELSE
		METHOD$ = "D"
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Adjustment... ", 1% + 16%)

	CLOSE IC_JOURADJUST.CH%

	WHEN ERROR IN
		KILL IC_JOURADJUST.DEV$ + "IC_JOURADJUST_" + BATCH_NO$ + ".JRL" &
			FOR I% = 1% TO 10%
	USE
		CONTINUE 1010
	END WHEN

1010	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.CRE"
	USE
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

1015	RESET #IC_JOURCOUNT.CH%, KEY #0%
	TEST_LOCATION$ = ""

1020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_JOURCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 1500 IF ERR = 11%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

	IF TEST_LOCATION$ <> IC_JOURCOUNT::LOCATION OR &
		TEST_PRODUCT$ <> IC_JOURCOUNT::PRODUCT
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_EXPAND%, &
			IC_JOURCOUNT::PRODUCT, &
			2%, 16%)

		GOSUB 18000 IF TEST_LOCATION$ <> ""
	END IF

1030	TEST_LOCATION$  = IC_JOURCOUNT::LOCATION
	TEST_PRODUCT$ = IC_JOURCOUNT::PRODUCT

	COUNT_ONHAND = COUNT_ONHAND + IC_JOURCOUNT::QUANTITY

	GOTO 1020

1500	GOSUB 18000 IF TEST_LOCATION$ <> ""

	CLOSE #IC_JOURADJUST.CH%
	CLOSE #IC_JOURCOUNT.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Adjustment completed ", 1%)
	IC_FUNC_JOURSET = 0%

 ExitFunction:
	IC_CYCLEJOUR = IC_CYCLEJOUR_INIT
	CALL ASSG_FREECHANNEL(IC_JOURCOUNT.CH%)
	CALL ASSG_FREECHANNEL(IC_JOURADJUST.CH%)

	FOR IC_LOOP% = 1% TO IC_LIST%
		CLOSE #IC_TRANSACTION.CH%(IC_LOOP%)
		CALL ASSG_FREECHANNEL(IC_TRANSACTION.CH%(IC_LOOP%))
	NEXT IC_LOOP%

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_EXPAND%)

	EXIT FUNCTION

	%Page

18000	!
	! Compare balance file with cycle couting
	!
	V% = IC_READ_35BALANCE(TEST_PRODUCT$, TEST_LOCATION$, BALANCE(,))

	SELECT METHOD$

	CASE "J"
		ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)

	CASE "D"
		ONHAND = BALANCE(1%, 1%)

18010		FOR IC_LOOP% = 1% TO IC_LIST%

			WHEN ERROR IN
				FIND #IC_TRANSACTION.CH%(IC_LOOP%), &
					KEY #0% EQ TEST_PRODUCT$ + TEST_LOCATION$, &
					REGARDLESS
			USE
				CONTINUE 18025 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "IC_TRANSACTION"
				CONTINUE HelpError
			END WHEN

18020			WHEN ERROR IN
				GET #IC_TRANSACTION.CH%(IC_LOOP%), REGARDLESS
			USE
				CONTINUE 18025 IF ERR = 11%
				FILENAME$ = "IC_TRANSACTION"
				CONTINUE HelpError
			END WHEN

			GOTO 18025 IF TEST_PRODUCT$ <> &
				IC_TRANSACTION::PRODUCT OR &
				TEST_LOCATION$ <> IC_TRANSACTION::LOCATION OR &
				IC_TRANSACTION::TRANS_DATE > &
				IC_CYCLEJOUR_INIT::COUNTDATE

			IF IC_TRANSACTION::TYPE_A <> "" AND &
				IC_TRANSACTION::QUANTITY_A <> 0.0
			THEN
				SELECT IC_TRANSACTION::TYPE_A
				CASE "CC", "IS", "MA", "RE", "RT", "SA", &
					"SE", "SP", "TR", "WA", "WR"

					ONHAND = ONHAND + &
						IC_TRANSACTION::QUANTITY_A

				CASE ELSE
					! NO IMPACT ON HAND
				END SELECT
			END IF

			IF IC_TRANSACTION::TYPE_B <> "" AND &
				IC_TRANSACTION::QUANTITY_B <> 0.0
			THEN
				SELECT IC_TRANSACTION::TYPE_B
				CASE "SO", "MO", "RQ", "TO", "LS", "PO", "WO"
					! NO IMPACT ON HAND

				CASE ELSE
					ONHAND = ONHAND + &
						IC_TRANSACTION::QUANTITY_B
				END SELECT
			END IF
			GOTO 18020

18025		NEXT IC_LOOP%

	END SELECT

 CompareQty:
18030	IF FUNC_ROUND(ONHAND - COUNT_ONHAND, 3%) <> 0.0
	THEN
		WHEN ERROR IN
			GET #IC_CYCLEJOUR.CH%, &
				KEY #0% EQ TEST_LOCATION$, &
				REGARDLESS
		USE
			CONTINUE 18910 IF ERR = 155%
			FILENAME$ = "IC_CYCLEJOUR"
			CONTINUE HelpError
		END WHEN

		V% = PD_EXAM_PRODUCT(TEST_PRODUCT$, PD_PRODUCT_EXAM)
		V% = UTL_READ_TRANSACCT(TEST_LOCATION$, &
			IC_CYCLEJOUR::TRANSTYPE, &
			PD_PRODUCT_EXAM::PROD_TYPE, &
			UTL_TRANSACCT_READ)
		IC_JOURADJUST::LOCATION = TEST_LOCATION$
		IC_JOURADJUST::PRODUCT = TEST_PRODUCT$
		IC_JOURADJUST::QUANTITY = COUNT_ONHAND - ONHAND
		IC_JOURADJUST::ACCOUNT = UTL_TRANSACCT_READ::ACCOUNT

		WHEN ERROR IN
			PUT #IC_JOURADJUST.CH%
		USE
			CONTINUE 18900 IF ERR = 130%
			FILENAME$ = "IC_CYCLEJOUR"
			CONTINUE HelpError
		END WHEN

		V% = IC_WRIT_35BALANCE(IC_JOURADJUST::PRODUCT, &
			IC_JOURADJUST::LOCATION, &
			IC_CYCLEJOUR::TRANSTYPE, &
			IC_JOURADJUST::QUANTITY)

	END IF

	COUNT_ONHAND = 0.0

 Ret18000:

	RETURN

	%Page

18900	!*******************************************************************
	! Duplicate key detected error.
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Product " + TRM$(IC_JOURADJUST::PRODUCT) + &
		" is duplicated!", 0%)

	GOTO Ret18000

18910	!*******************************************************************
	! Missing header
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Lines exist for location " + &
		TRM$(TEST_LOCATION$) + &
		" but header is missing!", 0%)

	GOTO ExitFunction

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	IC_FUNC_JOURSET = 1%
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
