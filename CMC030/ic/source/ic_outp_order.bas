1	%TITLE "Product On Order Report"
	%SBTTL "IC_OUTP_ORDER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_OUTP_ORDER(PD_PRODUCT_CDD PD_PRODUCT, &
		UTL_LOCATION_CDD UTL_LOCATION, &
		STRING SYSTEM)

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
	!	$ BAS IC_SOURCE:IC_OUTP_ORDER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_OUTP_ORDER
	!	$ DELETE IC_OUTP_ORDER.OBJ;*
	!
	! Author:
	!
	!	11/11/92 - Dan Perkins
	!
	! Modification History:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Dan Perkins
	!		Added code to make sure we are on the same
	!		product.
	!
	!	12/16/92 - Frank F. Starman
	!		Display price for PO.
	!
	!	12/17/92 - Frank F. Starman
	!		Added new argument SYSTEM.
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/16/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	06/19/95 - Kevin Handy
	!		Modified to use OUTP_INITFORM to set up report.
	!
	!	06/30/95 - Kevin Handy
	!		Move zeroing WP totals to before the find instead
	!		of after it.
	!
	!	07/11/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/27/95 - Kevin Handy
	!		Lose excess spaces in source code formatting.
	!
	!	11/27/95 - Kevin Handy
	!		Modified scan of PO Register so that it doesn't
	!		give up when the location changes, only when
	!		the Part Number changes.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

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
	EXTERNAL LONG   FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG   FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "IC_OUTP_ORDER"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	IC_OUTP_ORDER = 0%
	REPORT$ = "IC056"

	!
	! Get report information
	!
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Open needed files
	!
600	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		CONTINUE 610 IF ERR = 5%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

610	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
	USE
		CONTINUE 620 IF ERR = 5%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

620	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		CONTINUE 630 IF ERR = 5%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

630	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		CONTINUE Init IF ERR = 5%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 Init:	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ON ORDER DETAIL"

	TITLE$(2%) = TRM$(PD_PRODUCT::PRODUCT_NUM) + "  " + &
		TRM$(PD_PRODUCT::DESCRIPTION) + "  AT  LOCATION  " + &
		TRM$(UTL_LOCATION::LOCATION)  + "  " + &
		TRM$(UTL_LOCATION::LOCNAME)

	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	PO_ORD_QTY, PO_REC_QTY, PO_CAN_QTY = 0.0

	!
	! Print the PO title
	!
	TEXT$ = "PO#        Line POTyp Vendor     VendorName        " + &
		"             OrdQty RecQty CanQty Balance OrdDate  " + &
		"  RecDate     UnitPrice"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

	GOTO FindWPRegline IF PO_REG_LINE::PRODUCT <> &
		PD_PRODUCT::PRODUCT_NUM

	GOTO 17020 IF PO_REG_LINE::FROMLOCATION <> &
		UTL_LOCATION::LOCATION

17100	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + &
			PO_REG_LINE::PO_LINE, REGARDLESS
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

	GOTO GetNextRec IF FUNC_ROUND(BALANCE, 3%) <= 0.0 AND SYSTEM <> "PO"

	!
	! Get Vendor information
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	!
	! Print the stuff out
	!
	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + " " + &
		PO_REG_LINE::PO_LINE + " " + &
		PO_REG_LINE::PO_TYPE + "    " + &
		PO_REG_LINE::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + " " + &
		FORMAT$(ORD_QTY, "######") + " " + &
		FORMAT$(REC_QTY, "######") + " " + &
		FORMAT$(CAN_QTY, "######") + "  " + &
		FORMAT$(BALANCE, "######") + " " + &
		PRNT_DATE(PO_REG_LINE::ORDDATE, 8%) + " " + &
		PRNT_DATE(REC.DATE$, 8%) + " " + &
		FORMAT$(PRICE, "##,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	PO_ORD_QTY = PO_ORD_QTY + ORD_QTY
	PO_REC_QTY = PO_REC_QTY + REC_QTY
	PO_CAN_QTY = PO_CAN_QTY + CAN_QTY

	IF REC_QTY <> 0.0
	THEN
		W.QTY = W.QTY + REC_QTY
		W.PRICE = W.PRICE + REC_QTY*PRICE
	ELSE
		W.QTY = W.QTY + ORD_QTY
		W.PRICE = W.PRICE + ORD_QTY*PRICE
	END IF


	ORD_QTY, REC_QTY, CAN_QTY = 0.0

	GOTO GetNextRec

 FindWPRegline:
	PO_BALANCE = PO_ORD_QTY - PO_REC_QTY - PO_CAN_QTY

	IF W.QTY <> 0.0
	THEN
		PRICE = W.PRICE / W.QTY
	ELSE
		W.PRICE = 0.0
	END IF

	TEXT$ = "Purchase Order Total " + SPACE$(43%) + &
		FORMAT$(PO_ORD_QTY, "######") + " " + &
		FORMAT$(PO_REC_QTY, "######") + " " + &
		FORMAT$(PO_CAN_QTY, "######") + "  " + &
		FORMAT$(PO_BALANCE, "######") + SPACE$(23%) + &
		FORMAT$(W.PRICE, "##,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitFunction IF SYSTEM = "PO"

	!
	! Print the WP title
	!
	TEXT$ = "Job#       Line JobTp Reference#       JobDescription " + &
		"          OrdQty ComQty CanQty Balance OrdDate  " + &
		"  ComDate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17200	ORD_QTY, COM_QTY, CAN_QTY = 0.0
	WP_ORD_QTY, WP_COM_QTY, WP_CAN_QTY = 0.0

	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #1% EQ "M" + &
			PD_PRODUCT::PRODUCT_NUM, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

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

	GOSUB PrintWPInfo IF WP_REGLINE::JOB <> WP_REGLINE_TEST::JOB OR &
		WP_REGLINE::LLINE <> WP_REGLINE_TEST::LLINE

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

	GOTO ExitWP IF JC_JOB::LOCATION <> UTL_LOCATION::LOCATION

	!
	! Print the stuff out
	!
	TEXT$ = JC_JOB::JOB + " " + &
		WP_REGLINE_TEST::LLINE + " " + &
		JC_JOB::TTYPE + "    " + &
		JC_JOB::REFNO + " " + &
		LEFT(JC_JOB::DESCR, 24%) + " " + &
		FORMAT$(ORD_QTY, "######") + " " + &
		FORMAT$(COM_QTY, "######") + " " + &
		FORMAT$(CAN_QTY, "######") + "  " + &
		FORMAT$(BALANCE, "######") + " " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		PRNT_DATE(COMP_DATE$, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

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

	TEXT$ = "Work in Process Total" + SPACE$(43%) + &
		FORMAT$(WP_ORD_QTY, "######") + " " + &
		FORMAT$(WP_COM_QTY, "######") + " " + &
		FORMAT$(WP_CAN_QTY, "######") + "  " + &
		FORMAT$(WP_BALANCE, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_ORD_QTY = PO_ORD_QTY + WP_ORD_QTY
	TOTAL_REC_QTY = PO_REC_QTY + WP_COM_QTY
	TOTAL_CAN_QTY = PO_CAN_QTY + WP_CAN_QTY

	TOTAL_BALANCE = TOTAL_ORD_QTY - TOTAL_REC_QTY - TOTAL_CAN_QTY

	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ONORDER = FUNC_ROUND(BALANCE(3%, 1%) + BALANCE(3%, 2%), 3%)

	TEXT$ = SPACE$(30%) + &
		"Product Posted Total " + SPACE$(13%) + &
		FORMAT$(TOTAL_ORD_QTY, "######") + " " + &
		FORMAT$(TOTAL_REC_QTY, "######") + " " + &
		FORMAT$(TOTAL_CAN_QTY, "######") + "  " + &
		FORMAT$(TOTAL_BALANCE, "######")

	IF FUNC_ROUND(ONORDER - TOTAL_BALANCE, 3%) <> 0.0
	THEN
		TEXT$ = TEXT$ + " " + &
			" Warning: Inv posted balance is" + " " + &
			NUM1$(ONORDER)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RUN_TOTAL = FUNC_ROUND(BALANCE(3%, 1%) + BALANCE(3%, 2%) + &
		BALANCE(3%, 3%), 3%)

	TEXT$ = SPACE$(30%) + &
		"Product Running Total" + SPACE$(35%) + &
		FORMAT$(RUN_TOTAL, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE PO_REG_LINE.CH%
	CALL ASSG_FREECHANNEL(PO_REG_LINE.CH%)

	CLOSE PO_REG_SUB_LINE.CH%
	CALL ASSG_FREECHANNEL(PO_REG_SUB_LINE.CH%)

	CLOSE WP_REGLINE.CH%
	CALL ASSG_FREECHANNEL(WP_REGLINE.CH%)

	CLOSE SB_SUBACCOUNT.CH%
	CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

 Exit2:

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
