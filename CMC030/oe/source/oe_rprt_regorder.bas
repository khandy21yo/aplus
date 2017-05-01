1	%TITLE " Backorders Report"
	%SBTTL "OE_RPRT_REGORDER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:OE017
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*On Order\* Report contains columns for the following
	!	information:
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
	!	Quantity Onorder	Quantity Canceled
	!	.te
	!	Balance to Ship
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Orders
	!	.x Orders Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGORDER/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGORDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGORDER.OBJ;*
	!
	! AUTHOR:
	!
	!	09/22/97 - Kevin Handy
	!		Taken from OE_RPRT_REGBACK
	!
	! MODIFICATION HISTORY:
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
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
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

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
	!	.x Sort by
	!	^*(01) Sort by\*
	!	.B
	!	.LM +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.B
	!	Valid settings are:
	!	.TABLE 3,25
	!	.TE
	!	^*N\* - Customer Number
	!	.Te
	!	^*D\* - Document Number
	!	.Te
	!	^*C\* - Sale Category
	!	.Te
	!	^*T\* - Sale Type
	!	.eND TABLE
	!	A setting is required in this field.
	!	.LM -5
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
	!	The ^*From Item\* field begins the
	!	printing with a particular item number.
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
	!	The ^*To Item\* field ends printing of the
	!	report with a particular item.  The value entered must be in agreement with
	!	field (01) Sort by.
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
	!	Technique.
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

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " BACKORDERS BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " BACKORDERS BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = " BACKORDERS BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = " BACKORDERS BY DOCUMENT NUMBER"
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
	TITLE$(4%) = "Doc#        ST SalCat CusNumber    CusName                          CustPo#       " + &
		" OrdDate     Location       Shipvia"

	TITLE$(5%) = "                         Line   Product       Descr                      QtyOrd     " + &
		" QtyShip    QtyCancel   QtyOnOrder      ShipBal"

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
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""
	END SELECT

	PRINT_HEADER% = 0%

	!
	! Read customer name
	!
	IF AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_35CUSTOM_EXAM) <> CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = ""
	END IF

 RegHead:
	TEXT1$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + " " + &
		OE_REGHEADER::ORDCAT + "   " + &
		OE_REGHEADER::CUSNUM + "   " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 30%) + "   " + &
		OE_REGHEADER::CUSTPO + "     " + &
		PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
		OE_REGHEADER::LOCATION + "           " + &
		OE_REGHEADER::SHIPVIA

	NEXT_LINE$ = "   "

 ReadRegline:
	!
	! Try to find any line for the header
	!
	IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, NEXT_LINE$, &
		"GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
			IF PRINT_HEADER%

			GOTO GetNextRec
	END IF

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	!
	! Check if there are still any backorders
	!
	GOTO ReadRegline IF QTY(0%) <= 0.0

	!
	! Read description for the product
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print header if it is the first line for the order
	!
	IF PRINT_HEADER% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		PRINT_HEADER% = -1%
	END IF

	!
	! Print line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + &
		"               " + &
		OE_REGLINE_READ::LIN + "   " + &
		OE_REGLINE_READ::PRODUCT + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(QTY(1%), "#,###,###,##") + " " + &
		FORMAT$(QTY(2%), "#,###,###,##") + " " + &
		FORMAT$(QTY(3%), "#,###,###,##") + " " + &
		FORMAT$(QTY(0%), "#,###,###,##") + " " + &
		FORMAT$(QTY(8%), "#,###,###,##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadRegline

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
