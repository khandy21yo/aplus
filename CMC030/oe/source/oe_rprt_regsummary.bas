1	%TITLE "Order Status Summary"
	%SBTTL "OE_RPRT_REGSUMMARY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:OE015
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Status Summary\* Report contains the following information:
	!	.table 3,25
	!	.te
	!	Document Number	Sales Type
	!	.te
	!	Sales Category	Customer Number
	!	.te
	!	Customer Name	Customer PO Number
	!	.te
	!	Order Date	Location
	!	.te
	!	Ship Via	Line _#
	!	.te
	!	Product Number	Product Description
	!	.te
	!	Quantity Ordered	Quantity Shipped
	!	.te
	!	Quantity Backordered	Quantity Canceled
	!	.te
	!	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Status Summary
	!	.x Order Status Summary>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGSUMMARY/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGSUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGSUMMARY.OBJ;*
	!
	! AUTHOR:
	!
	!	06/14/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	07/16/91 - Craig Tanner
	!		Replaced most of code reading OE_REGLINE with call to
	!		OE_READ_REGLINE.
	!
	!	08/27/91 - Deborah K. Fries
	!		Reformatted output for better readability.
	!
	!	09/20/91 - Dan Perkins
	!		Added Back Order Only filed to code to print
	!		only backorders.
	!		Added PD_EXAM_PRODUCT to code.
	!		Streamlined code.
	!		Checked error trapping.
	!		Program now includes OE_RPRT_REGBACK program.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to manipulate ORDER NUMBER.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/11/94 - Kevin Handy
	!		Modified so that an undefined sortby will act as
	!		a 'D' (document) order. Maybe that will stop the
	!		disappearing titles.
	!
	!	10/12/94 - Kevin Handy
	!		Reduced number of places header gets printed.
	!		Modified handling of blank line between documents
	!		so that it doesn't get printed if the document
	!		wasn't printed.
	!
	!	10/14/94 - Kevin Handy
	!		Modified to display "Undefined" instead of giving
	!		a random customer name when the number is undefined.
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
	!		Reformat source code
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
	! Include codes.inc
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION OE_READ_REGLINE
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM

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
	!	.x Sort>Order Status Summary Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the order in which the
	!	report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*C\* - Sale Category
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	A value is required in this field.
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
	!	The ^*From Item\* field enters an item from which the
	!	report will begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Order Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item with which
	!	the report will end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Order Status Summary Report
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	See Appendix B for more information regarding wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Order Status Summary Report
	!
	!--

	YESNO$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Backorders Only>Order Status Summary Report
	!	^*(05) Backorders Only\*
	!	.b
	!	.lm +5
	!	The ^*Backorders Only\* field causes
	!	the report to print information which will include backorders
	!	only or all orders.
	!	.b
	!	Valid setting are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes, Backorders only
	!	.te
	!	^*N\* - No, All orders
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
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

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER SUMMARY BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER SUMMARY BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = " REGISTER SUMMARY BY CUSTOMER NUMBER"

	CASE ELSE
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER SUMMARY BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	END SELECT

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "Doc#        ST SalCat CusNumber    CusName     " + &
		"                     CustPo#        OrdDate     Location" + &
		"       Shipvia"
	TITLE$(5%) = "                         Line   Product        " + &
		"Descr                     QtyOrd      QtyShip    QtyCancel" + &
		"      QtyBack      Balance"
	TITLE$(6%) = "."

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

	PRINT_HEADER% = -1%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitProgram IF (OE_REGHEADER::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			CUSNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			ORDTYPE, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			CUSNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE ELSE
		GOTO ExitProgram IF (OE_REGHEADER::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			ORDNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

	PRINT_HEADER% = 0%

	!
	! Read customer name
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

	IF V% <> CMC$_NORMAL
	THEN
		AR_CUSTOM_EXAM::CUSNAM = "(Undefined)"
	END IF

	HEADER$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + " " + &
		OE_REGHEADER::ORDCAT + "   " + &
		OE_REGHEADER::CUSNUM + "   " + &
		LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + "   " + &
		OE_REGHEADER::CUSTPO + "     " + &
		PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
		OE_REGHEADER::LOCATION + "           " + &
		OE_REGHEADER::SHIPVIA

	NEXT_LINE$ = "   "

 ReadRegline:
	!
	! Try to find any line for the header
	!
	GOTO NewOrder IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, NEXT_LINE$, &
		"GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	GOTO ReadRegline IF QTY(7%) = 0.0 AND YESNO$ = "Y"

	!
	! Read description for the product
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print header if it is the first line for the order
	!
	IF PRINT_HEADER% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), HEADER$, 0%)
		PRINT_HEADER% = -1%
	END IF

	!
	! Print line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "               " + &
		OE_REGLINE_READ::LIN + "   " + &
		OE_REGLINE_READ::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + &
		FORMAT$(QTY(1%), "###,###,###,") + " " + &
		FORMAT$(QTY(2%), "###,###,###,") + " " + &
		FORMAT$(QTY(3%), "###,###,###,") + " " + &
		FORMAT$(QTY(7%), "###,###,###,") + " " + &
		FORMAT$(QTY(0%), "###,###,###,")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadRegline

 NewOrder:
	IF PRINT_HEADER% = -1%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	GOTO GetNextRec

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
