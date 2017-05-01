1	%TITLE "Shipping Journal Report"
	%SBTTL "OE_RPRT_SHIPJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:OE011
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Shipping Journal\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Document Number	Sale Type
	!	.te
	!	Sale Category	Customer Number
	!	.te
	!	Customer Name	Customer PO Number
	!	.te
	!	Order Date	Location
	!	.te
	!	Ship Via	Line
	!	.te
	!	Product	Description
	!	.te
	!	Quantity Ordered	Quantity Shipped
	!	.te
	!	Prior Quantity Shipped	Quantity Canceled
	!	.te
	!	Prior Quantity Canceled	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Journal
	!	.x Order Journal>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SHIPJOUR/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SHIPJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SHIPJOUR.OBJ;*
	!
	! AUTHOR:
	!
	!	06/14/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	08/09/90 - Craig Tanner
	!		Rewrote error trapping, reformatted printout, printout
	!		routine.
	!
	!	11/13/90 - Val Allen
	!		Modify to prevent printing of non-shipped regular
	!		orders.
	!
	!	08/09/91 - Craig Parham "And I Am Outa Here" Tanner
	!		Updated call to OE_READ_REGLINE. Now third parameter
	!		is NEXT_GET, used to be some meaningless date.
	!
	!	09/23/91 - Dan Perkins
	!		Updated code to include AR_EXAM_CUSTOM function.
	!		Cleaned up program code.  Checked error trapping.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to manipulate ORDER NUMBER.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!		Reformat source closer to 80 columns.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.HB"
	MAP (OE_SHIPJOUR)	OE_SHIPJOUR_CDD		OE_SHIPJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPLINE.HB"
	MAP (OE_SHIPLINE)	OE_SHIPLINE_CDD		OE_SHIPLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
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
	!	.x Sort by
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* - Sale Category
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	item with which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
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
	!	The ^*To Item\* field enters an item with which
	!	the report will end printing.  The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Batch Number
	!	^*(04) Batch Number
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a selected batch number
	!	which is to be printed.  Each journal file is assigned a
	!	user batch number consisting of two (2) alphanumeric characters.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	On entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Ship journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.OPN"
	USE
		FILENAME$ = "OE_SHIPLINE"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_SHIPJOUR"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " SHIPPING JOURNAL SUMMARY BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SHIPPING JOURNAL SUMMARY BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = " SHIPPING JOURNAL SUMMARY BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = " SHIPPING JOURNAL SUMMARY BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - LEN(TO_ITEM$)) + &
			TO_ITEM$

	END SELECT

	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "Batch No. " + BATCH_NO$
	TITLE$(4%) = ""
	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(5%) = "Doc#        SaleType     SalCat CusNumber  " + &
		"CusName                        CustPo#   OrdDate      " + &
		"Location       Shipvia  Release#"
	TITLE$(6%) = "                         Line Product        " + &
		"Description           QtyOrdered    QtyShip PriorQtShp  " + &
		"QtyCancel  PQtyCncel    Balance"
	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_REGHEADER.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_REGHEADER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get OE_REGHEADER record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE Exitprogram IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitProgram IF (OE_REGHEADER::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDNUM, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDTYPE, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17025	!
	! Get OE_SHIPJOUR record
	!
	WHEN ERROR IN
		GET #OE_SHIPJOUR.CH%, &
			KEY #0% GE OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "OE_SHIPJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check and make sure that what you get is what you want
	!
	GOTO GetNextRec IF OE_SHIPJOUR::ORDNUM <> OE_REGHEADER::ORDNUM

	!
	! Read customer name
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

	!
	! Print out blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out customer name line
	!
	TEXT1$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + "           " + &
		OE_REGHEADER::ORDCAT + "   " + &
		OE_REGHEADER::CUSNUM + " " + &
		LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		OE_REGHEADER::CUSTPO + " " + &
		PRNT_DATE(OE_SHIPJOUR::SHIPDATE, 8%) + "  " + &
		OE_REGHEADER::LOCATION + "           " + &
		OE_SHIPJOUR::SHIPVIA + "       " + &
		OE_SHIPJOUR::SHIPNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)

17320	WHEN ERROR IN
		FIND #OE_SHIPLINE.CH%, &
			KEY #0% GE OE_SHIPJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_SHIPJOUR"
		CONTINUE HelpError
	END WHEN

 GetShiplineRec:
17325	!
	! Check if there is a new line number
	!
	WHEN ERROR IN
		GET #OE_SHIPLINE.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "OE_SHIPLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF OE_SHIPLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	!
	! Get product from OE_REGLINE
	!
	V% = OE_READ_REGLINE(OE_REGHEADER::ORDNUM,OE_SHIPLINE::LIN, &
		"EQ", OE_REGLINE_READ, QTY())

	!
	! Get description from PD_PRODUCT
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, &
		PD_PRODUCT_EXAM)

	!
	! Check if there are still any backorders
	!
	BALANCE = QTY(1%) - OE_SHIPLINE::SHPQTY - OE_SHIPLINE::CANCELQTY - &
		QTY(2%) - QTY(3%)

	BALANCE = 0.0 IF BALANCE <= 0.0

	!
	! Print out one line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + &
		"               " + &
		OE_SHIPLINE::LIN + " " + &
		PD_PRODUCT_EXAM::PRODUCT_NUM + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 21%) + " " + &
		FORMAT$(QTY(1%), "#######.##") + " " + &
		FORMAT$(OE_SHIPLINE::SHPQTY, "#######.##") + " " + &
		FORMAT$(QTY(2%), "#######.##") + " " + &
		FORMAT$(OE_SHIPLINE::CANCELQTY, "#######.##") + " " + &
		FORMAT$(QTY(3%), "#######.##") + " " + &
		FORMAT$(BALANCE, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetShiplineRec

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
