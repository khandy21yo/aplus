1	%TITLE "Product On Order"
	%SBTTL "IC_WRIT_ORDER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_WRIT_ORDER(PD_PRODUCT_CDD PD_PRODUCT, &
		STRING LOCATION, REAL DIFFQTY)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This program prints Inventory order report
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_WRIT_ORDER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_WRIT_ORDER
	!	$ DELETE IC_WRIT_ORDER.OBJ;*
	!
	! Author:
	!
	!	10/14/93 - Frank F. Starman
	!
	! Modification History:
	!
	!	01/12/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/19/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_TEST

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	COM (IC_WRIT_ORDER) PO_REG_LINE.CH%, &
		PO_REG_SUB_LINE.CH%, &
		WP_REGLINE.CH%, &
		SB_SUBACCOUNT.CH%

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "IC_WRIT_ORDER"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	IC_WRIT_ORDER = 0%
	DIFFQTY = 0.0

	!
	! Open needed files
	!
600	IF PO_REG_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
		USE
			CONTINUE 610 IF ERR = 5%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN
	END IF

610	IF PO_REG_SUB_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
		USE
			CONTINUE 620 IF ERR = 5%
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN
	END IF

620	IF WP_REGLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			CONTINUE 630 IF ERR = 5%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

630	IF SB_SUBACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			CONTINUE Init IF ERR = 5%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

 Init:	!
	! Initilize report
	!

 ReportTitle:

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	PO_ORD_QTY, PO_REC_QTY, PO_CAN_QTY = 0.0

	WHEN ERROR IN
		FIND #PO_REG_LINE.CH%, &
			KEY #4% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE FindWPRegline IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE FindWPRegline IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO FindWPRegline IF PO_REG_LINE::PRODUCT + &
		PO_REG_LINE::FROMLOCATION <> &
		PD_PRODUCT::PRODUCT_NUM + LOCATION

17100	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	ORD_QTY, REC_QTY, CAN_QTY = 0.0
	REC.DATE$ = "        "

 GetSRec:
17120	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE PrintPOInfo IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintPOInfo IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"
		ORD_QTY = ORD_QTY + PO_REG_SUB_LINE::QTY
		PRICE = PO_REG_SUB_LINE::PRICE

	CASE "02"
		REC_QTY = REC_QTY + PO_REG_SUB_LINE::QTY

		REC.DATE$ = PO_REG_SUB_LINE::ACTION_DATE &
			IF PO_REG_SUB_LINE::ACTION_DATE > REC.DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + PO_REG_SUB_LINE::QTY

	CASE "09"
		PRICE = PO_REG_SUB_LINE::PRICE

	END SELECT

	GOTO GetSRec

 PrintPOInfo:
	BALANCE = ORD_QTY - REC_QTY - CAN_QTY

	GOTO GetNextRec IF FUNC_ROUND(BALANCE, 3%) <= 0.0

	PO_ORD_QTY = PO_ORD_QTY + ORD_QTY
	PO_REC_QTY = PO_REC_QTY + REC_QTY
	PO_CAN_QTY = PO_CAN_QTY + CAN_QTY

	IF REC_QTY <> 0.0
	THEN
		W_QTY = W_QTY + REC_QTY
		W_PRICE = W_PRICE + REC_QTY * PRICE
	ELSE
		W_QTY = W_QTY + ORD_QTY
		W_PRICE = W_PRICE + ORD_QTY * PRICE
	END IF


	ORD_QTY, REC_QTY, CAN_QTY = 0.0

	GOTO GetNextRec

 FindWPRegline:
	PO_BALANCE = PO_ORD_QTY - PO_REC_QTY - PO_CAN_QTY

	IF W_QTY <> 0.0
	THEN
		PRICE = W_PRICE / W_QTY
	ELSE
		W_PRICE = 0.0
	END IF

17200	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #1% EQ "M" + PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	ORD_QTY, COM_QTY, CAN_QTY = 0.0
	WP_ORD_QTY, WP_COM_QTY, WP_CAN_QTY = 0.0
	COMP_DATE$ = "        "

 GetWPRegline:
17220	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF WP_REGLINE::ITEMCODE <> PD_PRODUCT::PRODUCT_NUM

	GOSUB PrintWPInfo IF WP_REGLINE::JOB + &
		WP_REGLINE::LLINE <> &
		WP_REGLINE_TEST::JOB + &
		WP_REGLINE_TEST::LLINE

	WP_REGLINE_TEST = WP_REGLINE

	SELECT WP_REGLINE::REC_TYPE

	CASE "01"
		ORD_QTY = ORD_QTY + WP_REGLINE::QTY

	CASE "02"
		COM_QTY = COM_QTY + WP_REGLINE::QTY

		COMP_DATE$ = WP_REGLINE::COMP_DATE &
			IF WP_REGLINE::COMP_DATE > COMP_DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + WP_REGLINE::QTY

	END SELECT

	GOTO GetWPRegline

 PrintWPInfo:
	BALANCE = ORD_QTY - COM_QTY - CAN_QTY

	GOTO ExitWP IF FUNC_ROUND(BALANCE, 3%) <= 0.0

17300	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ "J" + WP_REGLINE_TEST::JOB, &
			REGARDLESS
	USE
		CONTINUE ExitWP IF ERR = 155% OR ERR = 9%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitWP IF JC_JOB::LOCATION <> LOCATION

	WP_ORD_QTY = WP_ORD_QTY + ORD_QTY
	WP_COM_QTY = WP_COM_QTY + COM_QTY
	WP_CAN_QTY = WP_CAN_QTY + CAN_QTY

 ExitWP:
	ORD_QTY, COM_QTY, CAN_QTY = 0.0
	COMP_DATE$ = "        "

	RETURN

 ExitTotal:
	GOSUB PrintWPInfo

	WP_BALANCE = WP_ORD_QTY - WP_COM_QTY - WP_CAN_QTY

	TOTAL_ORD_QTY = PO_ORD_QTY + WP_ORD_QTY
	TOTAL_REC_QTY = PO_REC_QTY + WP_COM_QTY
	TOTAL_CAN_QTY = PO_CAN_QTY + WP_CAN_QTY

	TOTAL_BALANCE = TOTAL_ORD_QTY - TOTAL_REC_QTY - TOTAL_CAN_QTY

	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION, BALANCE(,))

	ONORDER = FUNC_ROUND(BALANCE(3%, 1%) + BALANCE(3%, 2%), 3%)

	DIFFQTY = FUNC_ROUND(ONORDER - TOTAL_BALANCE, 3%)

 ExitFunction:

	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
