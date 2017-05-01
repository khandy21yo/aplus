1	%TITLE "Product Quantity Status Report"
	%SBTTL "OE_RPRT_REGPROD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:OE018
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Quantity Status\* Report contains columns for
	!	the following information:
	!	.table 3,25
	!	.te
	!	Product Number	Product Description
	!	.te
	!	Product Type	Product Category
	!	.te
	!	Customer Number	Customer Name
	!	.te
	!	Customer PO Number	Document Number
	!	.te
	!	Quantity Ordered	Quantity Shipped
	!	.te
	!	Quantity Back Ordered	Balance
	!	.te
	!	Required Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Product Quantity Status
	!	.x Product Quantity Status>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGPROD/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGPROD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGPROD.OBJ;*
	!
	! AUTHOR:
	!
	!	07/14/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	11/09/90 - Kevin Handy
	!		Removed final output from inside of error
	!		trapping, and placed it in ExitTotal.
	!
	!	07/17/91 - Craig Tanner
	!		Added ability to do multiple sorting using
	!		the product file.
	!
	!	10/22/91 - Dan Perkins
	!		Cleaned program code.  Checked error trapping.
	!
	!	11/19/91 - Dan Perkins
	!		Added option to show selected product quantities.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display in integer form.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to manipulate ORDER NUMBER.
	!
	!	04/29/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	05/07/92 - Frank F. Starman
	!		Allow to print report by secondary code.
	!
	!	05/13/92 - Dan Perkins
	!		Show secondary code in report output.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/20/92 - Dan Perkins
	!		Use FUNC_ROUND to round quantities.  This should
	!		solve some zero balance problems.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/30/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Reformatted source code to 80 columns.
	!
	!	05/05/95 - Kevin Handy
	!		Reformat to 80 columns. Make formatting more
	!		standard throughout program.
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	01/28/96 - Kevin Handy
	!		Modified so could see either 'Bin Loc' or
	!		'cust po' on the-report. Not enough room
	!		for both though.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE_OLD

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort by>Backorders by Product Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*S\* - Product Shipped
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Backorders by Product Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins the report
	!	printing with a particular item.
	!	The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field ends printing
	!	with a particular item.
	!	The value entered must be in agreement
	!	with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	DISPLAY_QTY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Display Quantity
	!	^*(05) Display Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Display Quantity\* field selects
	!	designated quantities to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	*O - Ordered Quantity
	!	.te
	!	*S - Shipped Quantity
	!	.te
	!	*C - Canceled Quantity
	!	.te
	!	*B - Backordered Quantity
	!	.te
	!	*Q - Quantity Balance
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	DISPLAY_TEXT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	.x Display Text
	!	^*(05) Display Text (B,P)\*
	!	.b
	!	.lm +5
	!	The ^*Display Text\* field selects
	!	a designated text items to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	*B - Bin Location
	!	.te
	!	*C - Customer PO Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN


315	!
	! Open Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! IC_BINMAP File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		CONTINUE 330
	END WHEN

330	!
 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "PRODUCT STATUS BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "PRODUCT STATUS BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "PRODUCT STATUS BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "PRODUCT STATUS BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "PRODUCT STATUS BY SECONDARY CODE"

	END SELECT

	SELECT DISPLAY_QTY$

	CASE "O"
		TITLE$(2%) = "QUANTITY ORDERED"
		QD$ = " QtyOrd"
		QD% = 1%

	CASE "S"
		TITLE$(2%) = "QUANTITY SHIPPED"
		QD$ = "QtyShip"
		QD% = 2%

	CASE "C"
		TITLE$(2%) = "QUANTITY CANCELLED"
		QD$ = " QtyCan"
		QD% = 3%

	CASE "B"
		TITLE$(2%) = "QUANTITY BACK ORDERED"
		QD$ = "QtyBack"
		QD% = 7%

	CASE "Q"
		TITLE$(2%) = "BALANCE"
		QD$ = "Balance"
		QD% = 0%

	END SELECT

	SELECT DISPLAY_TEXT$
	CASE "C"
		TITLEX$ = "Cust PO"

	CASE ELSE
		TITLEX$ = "Bin Loc"
	END SELECT

	!
	! Heading
	!
	TITLE$(3%) = " Order Entry System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Product        Description          PT PCat SecCode" + &
		"    CusNumber  CusName            " + TITLEX$ + &
		"    Doc#       Line    " + QD$ + " ReqDate"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset REGPROD file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #K_NUM%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitProgram IF (PD_PRODUCT::SECONDARY_CODE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TEST_ORDNUM$ = ""
	TEST_LINE$   = ""

17030	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #1% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegLine:
17050	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintTotal IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF OE_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
	THEN
		GOSUB BackOrderTest
		GOTO PrintTotal
	END IF

	IF TEST_ORDNUM$ = "" AND TEST_LINE$ = ""
	THEN
		TEST_ORDNUM$ = OE_REGLINE::ORDNUM + ""
		TEST_LINE$   = OE_REGLINE::LIN + ""
	END IF

	IF OE_REGLINE::ORDNUM + OE_REGLINE::LIN <> &
		TEST_ORDNUM$ + TEST_LINE$
	THEN
		GOSUB BackOrderTest
	END IF

	GOSUB TestQty
	GOTO GetRegLine

 PrintTotal:
	IF TOTAL <> 0.0
	THEN
		TEXT$ = PRODUCT1$ + &
			SPACE$(90%) + &
			"Total: " + &
			FORMAT$(TOTAL, "###,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL = 0.0
	END IF

	GOTO GetNextRec

 TestQty:
17100	!
	! See if there are quantities to print
	!
	SELECT OE_REGLINE::TRANTYPE

	CASE "01" ! Order
		IND% = 1%
		REQ_DATE$ = OE_REGLINE::TDATE

	CASE "02", "03" ! Ship, Cancel
		IND% = VAL%(OE_REGLINE::TRANTYPE)

	CASE "12"
		IND% = 4%

	CASE "13"
		IND% = 5%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + OE_REGLINE::QTY

	RETURN

 BackOrderTest:
17200	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 2%)
	QTY(0%) = 0.0 IF QTY(0%) < 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	QTY(6%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%) - &
		QTY(4%) - QTY(5%), 2%)
	QTY(6%) = 0.0 IF QTY(6%) < 0.0

	!
	! Back Orders
	!
	IF DATE_TODAY > REQ_DATE$ AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	GOTO EndPrintLine IF QTY(QD%) = 0.0

 PrintLine:
17220	!
	! Get the RegHeader record
	!
	V% = OE_READ_REGHEADER(TEST_ORDNUM$, OE_REGHEADER_READ)

	!
	! Get the Customer record
	!
	IF AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM) <> &
		CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = &
			STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B)
	END IF

	!
	! Display either 'Customer PO' or 'Bin Location'
	!
	SELECT DISPLAY_TEXT$
	CASE "C"
		THIS_BIN$ = LEFT(OE_REGHEADER_READ::CUSTPO, 10%)
	CASE ELSE
		GOSUB GetBinMap
	END SELECT

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 18%) + " " + &
		THIS_BIN$ + " " + &
		CONV_STRING(TEST_ORDNUM$, CMC$_LEFT) + " " + &
		TEST_LINE$ + " " + &
		FORMAT$(QTY(QD%), "##,###,###") + " " + &
		PRNT_DATE(REQ_DATE$, 6%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 EndPrintLine:
17240	PRODUCT1$    = PD_PRODUCT::PRODUCT_NUM + ""
	TEST_ORDNUM$ = OE_REGLINE::ORDNUM + ""
	TEST_LINE$   = OE_REGLINE::LIN + ""
	TOTAL        = TOTAL + QTY(QD%)
	QTY(I%)      = 0.0 FOR I% = 0% TO 10%

	RETURN

	%PAGE

 GetBinMap:
17300	!*******************************************************************
	! Pull out binmap information
	!*******************************************************************

	THIS_BIN$ = "          "

	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			OE_REGHEADER_READ::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17390
	END WHEN

	THIS_BIN$ = IC_BINMAP::BIN(0%) + "    "

17390	RETURN

 ExitProgram:
17900	CALL OUTP_FINISH(UTL_REPORTX)

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
