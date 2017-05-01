1	%TITLE "Print Received Order Journal"
	%SBTTL "PO_RPRT_POIC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PO023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Received Order Journal\* option
	!	prints a batch of received orders prior to posting the batch.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_POIC/LINE
	!	$ LINK/EXE:PO_EXE PO_RPRT_POIC, -
	!		FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE PO_RPRT_POIC.OBJ;*
	!
	! Author:
	!
	!	06/16/95 - Kevin Handy
	!		Based upon IC_OUTP_ORDER
	!
	! Modification History:
	!
	!	06/16/95 - Kevin Handy
	!		Modified so that there will be no negative
	!		balances, which throws off the comparison.
	!
	!	06/29/95 - Kevin Handy
	!		Zero variable W_PRICE at the start of each
	!		product.
	!
	!	06/30/95 - Kevin Handy
	!		Modified to zero WP quanities before the find,
	!		so that items without WP will not be given the
	!		previous products value.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check).
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/15/2000 - Kevin Handy
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
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

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
	EXTERNAL LONG    FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^* (01) From Item\*
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort By\*
	!
	! Index:
	!
	!--

	LOCATION$ = LEFT(UTL_REPORTX::OPTDEF(3%), 4%)

	!++
	! Abstract:FLD04
	!	^*(04) Location\*
	!	.b
	!
	! Index:
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  SECONDARY NUMBER"

	END SELECT


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
		CONTINUE 640 IF ERR = 5%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

640	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ON ORDER DETAIL"

	TITLE$(2%) = TRM$(FROM_ITEM$) + " to " + &
		TRM$(TO_ITEM$) + "  At Location " + &
		TRM$(LOCATION$)

	TITLE$(3%) = "Purchase Order System"
	TITLE$(4%) = ""
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17004	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	W_PRICE = 0.0

	SELECT SORT_BY$

	CASE "C"

		GOTO ExitFunction &
			IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND TO_ITEM$ <> ""

	CASE "D"
		GOTO ExitFunction &
			IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "P"
		GOTO ExitFunction &
			IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitFunction &
			IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND TO_ITEM$ <> ""

	END SELECT

	PD_PRINT% = 0%
	PO_PRINT% = 0%
	WP_PRINT% = 0%

17008	PO_ORD_QTY, PO_REC_QTY, PO_CAN_QTY, PO_BAL_QTY = 0.0

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

	GOTO FindWPRegline &
		IF PO_REG_LINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM OR &
		PO_REG_LINE::FROMLOCATION <> LOCATION$

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
	REC_DATE$ = "        "

 GetSRec:
17120	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE PrintPOInfo IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintPOInfo &
		IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"
		ORD_QTY = ORD_QTY + PO_REG_SUB_LINE::QTY
		PRICE = PO_REG_SUB_LINE::PRICE

	CASE "02"
		REC_QTY = REC_QTY + PO_REG_SUB_LINE::QTY

		REC_DATE$ = PO_REG_SUB_LINE::ACTION_DATE &
			IF PO_REG_SUB_LINE::ACTION_DATE > REC_DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + PO_REG_SUB_LINE::QTY

	CASE "09"
		PRICE = PO_REG_SUB_LINE::PRICE

	END SELECT

	GOTO GetSRec

 PrintPOInfo:
	BALANCE = ORD_QTY - REC_QTY - CAN_QTY
	BALANCE = 0.0 &
		IF BALANCE < 0.0

	!
	! Get Vendor information
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	GOSUB PoHeader

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
		PRNT_DATE(REC_DATE$, 8%) + " " + &
		FORMAT$(PRICE, "##,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	PO_ORD_QTY = PO_ORD_QTY + ORD_QTY
	PO_REC_QTY = PO_REC_QTY + REC_QTY
	PO_CAN_QTY = PO_CAN_QTY + CAN_QTY
	PO_BAL_QTY = PO_BAL_QTY + BALANCE

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
	IF PO_PRINT%
	THEN
		IF W_QTY <> 0.0
		THEN
			PRICE = W_PRICE / W_QTY
		ELSE
			W_PRICE = 0.0
		END IF

		TEXT$ = "Purchase Order Total " + SPACE$(43%) + &
			FORMAT$(PO_ORD_QTY, "######") + " " + &
			FORMAT$(PO_REC_QTY, "######") + " " + &
			FORMAT$(PO_CAN_QTY, "######") + "  " + &
			FORMAT$(PO_BAL_QTY, "######") + SPACE$(20%) + &
			FORMAT$(W_PRICE, "#,###,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

	END IF

17200	ORD_QTY, COM_QTY, CAN_QTY = 0.0
	WP_ORD_QTY, WP_COM_QTY, WP_CAN_QTY = 0.0

	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #1% EQ "M" + PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
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

	GOTO ExitTotal &
		IF WP_REGLINE::ITEMCODE <> PD_PRODUCT::PRODUCT_NUM

	GOSUB PrintWPInfo &
		IF WP_REGLINE::JOB <> WP_REGLINE_TEST::JOB OR &
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

	GOTO ExitWP &
		IF FUNC_ROUND(BALANCE, 3%) <= 0.0

17300	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ "J" + WP_REGLINE_TEST::JOB, &
			REGARDLESS
	USE
		CONTINUE ExitWP IF ERR = 155% OR ERR = 9%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitWP &
		IF JC_JOB::LOCATION <> LOCATION$

	GOSUB WpHeader

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

	IF WP_PRINT%
	THEN
		TEXT$ = "Work in Process Total" + SPACE$(43%) + &
			FORMAT$(WP_ORD_QTY, "######") + " " + &
			FORMAT$(WP_COM_QTY, "######") + " " + &
			FORMAT$(WP_CAN_QTY, "######") + "  " + &
			FORMAT$(WP_BALANCE, "######")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TOTAL_ORD_QTY = PO_ORD_QTY + WP_ORD_QTY
	TOTAL_REC_QTY = PO_REC_QTY + WP_COM_QTY
	TOTAL_CAN_QTY = PO_CAN_QTY + WP_CAN_QTY
	TOTAL_BAL_QTY = PO_BAL_QTY + WP_ORD_QTY - WP_COM_QTY - WP_CAN_QTY

	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION$, BALANCE(,))

	ONORDER = FUNC_ROUND(BALANCE(3%, 1%) + BALANCE(3%, 2%), 3%)

	IF (PD_PRINT%) OR (FUNC_ROUND(ONORDER - TOTAL_BAL_QTY, 3%) <> 0.0)
	THEN
		GOSUB PdHeader

		TEXT$ = SPACE$(30%) + &
			"Product Posted Total " + SPACE$(13%) + &
			FORMAT$(TOTAL_ORD_QTY, "######") + " " + &
			FORMAT$(TOTAL_REC_QTY, "######") + " " + &
			FORMAT$(TOTAL_CAN_QTY, "######") + "  " + &
			FORMAT$(TOTAL_BAL_QTY, "######")

		IF FUNC_ROUND(ONORDER - TOTAL_BAL_QTY, 3%) <> 0.0
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

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	GOTO 17004

 PoHeader:
	IF PO_PRINT% = 0%
	THEN
		GOSUB PdHeader

		!
		! Print the PO title
		!
		TEXT$ = "PO#        Line POTyp Vendor     VendorName        " + &
			"             OrdQty RecQty CanQty Balance OrdDate  " + &
			"  RecDate     UnitPrice"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	PO_PRINT% = -1%

	RETURN

 WPHeader:
	IF WP_PRINT% = 0%
	THEN
		GOSUB PdHeader

		!
		! Print the WP title
		!
		TEXT$ = "Job#       Line JobTp Reference#       JobDescription " + &
			"          OrdQty ComQty CanQty Balance OrdDate  "       + &
			"  ComDate"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	WP_PRINT% = -1%

	RETURN

 PdHeader:
	IF PD_PRINT% = 0%
	THEN
		!
		! Print the WP title
		!
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			PD_PRODUCT::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	PD_PRINT% = -1%

	RETURN

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

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

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

32767	END
