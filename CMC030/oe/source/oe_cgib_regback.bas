1	%TITLE " Backorders Report"
	%SBTTL "OE_CGIB_REGBACK"
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
	! ID:OE017
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Backorders\* Report contains columns for the following
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
	!	Quantity Backordered	Quantity Canceled
	!	.te
	!	Balance to Ship
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Backorders
	!	.x Backorders Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_CGIB_REGBACK/LINE
	!	$ LINK/EXE=OE_EXE: OE_CGIB_REGBACK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_CGIB_REGBACK.OBJ;*
	!
	! AUTHOR:
	!
	!	04/01/2004 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	05/19/2004 - Kevin Handy
	!		Add CUSPO to output.
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
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
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	DEF CGI_TEXT$(X$)
		CGI_TEXT$ = '"' + EDIT$(X$, 8% + 128%) + '"'
	FNEND

	DEF CGI_NUMBER$(X, Y%)
		CGI_NUMBER$ = NUM1$(FUNC_ROUND(X, Y%))
	FNEND

	ON ERROR GOTO 19000

 !	OPEN "TT:" FOR OUTPUT AS FILE 1%, &
 !		RECORDSIZE 511%

	MARGIN 511%

	PRINT "Content-Type: Text/Plain"
	PRINT ""

	!
 Init:	! Initilize report
	!
	SYS_STATUS% = LIB$GET_SYMBOL("WWW_QUERY_STRING" BY DESC, &
		UQUERY$ BY DESC,,)

 !	PRINT "TestI: '"; UQUERY$; "'"

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		UQUERY$ = ""
	END IF

	FROM_ITEM% = INSTR(1%, UQUERY$, "=")
	IF (FROM_ITEM%)
	THEN
		FROM_ITEM$ = RIGHT(UQUERY$, FROM_ITEM% + 1%)
		IF LEFT(FROM_ITEM$, 1%) = '"'
		THEN
			FROM_ITEM$ = RIGHT(FROM_ITEM$, 2%)
		END IF
		IF RIGHT(FROM_ITEM$, LEN(FROM_ITEM$)) = '"'
		THEN
			FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(FROM_ITEM$) - 1%)
		END IF
		FROM_ITEM$ = LEFT(FROM_ITEM$ + "          ", 10%)
	END IF

 !	PRINT "Testing: '"; FROM_ITEM$; "'"

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

 !	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	TO_ITEM$ = FROM_ITEM$

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field end printing of the
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

 !	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	WLDCRD$ = ""

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
	K_NUM% = 3%

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "Doc#        ST SalCat CusNumber    " + &
		"CusName                          CustPo#       " + &
		" OrdDate     Location       Shipvia"

	TITLE$(5%) = "                   Line   Product       " + &
		"Descr                     QtyOrd     " + &
		"QtyShip   QtyCancel     QtyBack     ShipBal     On Hand"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	IF FROM_ITEM$ = ""
	THEN
		GOTO ExitProgram
	END IF

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		FIND #OE_REGHEADER.CH%, &
			KEY #K_NUM% EQ FROM_ITEM$, &
			REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
 !	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$)

	PRINT_HEADER% = 0%

	!
	! Read customer name
	!
	IF AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_35CUSTOM_EXAM) <> CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = ""
	END IF

 RegHead:
 !	TEXT1$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
 !		OE_REGHEADER::ORDTYPE + " " + &
 !		OE_REGHEADER::ORDCAT + "   " + &
 !		OE_REGHEADER::CUSNUM + "   " + &
 !		LEFT$(AR_35CUSTOM_EXAM::CUSNAM,30%) + "   " + &
 !		OE_REGHEADER::CUSTPO + "     " + &
 !		PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
 !		OE_REGHEADER::LOCATION + "           " + &
 !		OE_REGHEADER::SHIPVIA

	NEXT_LINE$ = "   "

 ReadRegline:
	!
	! Try to find any line for the header
	!
	IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, NEXT_LINE$, &
		"GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL
	THEN
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
 !			IF PRINT_HEADER%

			GOTO GetNextRec
	END IF

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	!
	! Check if there are still any backorders
	!
	GOTO ReadRegline IF QTY(7%) <= 0.0

	!
	! Read description for the product
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Calculate balances
	!
	IF (IC_READ_35BALANCE(OE_REGLINE_READ::PRODUCT, &
		OE_REGHEADER::LOCATION, BALANCE(,)) AND 1%) = 0%
	THEN
		ONHAND = 0.0
		ALLOC   = 0.0
		ONORDER = 0.0

		CUR_ONHAND = 0.0
		CUR_ALLOC = 0.0
		CUR_ONORDER = 0.0
	ELSE
		ONHAND = BALANCE(1%, 1%)
		ALLOC   = BALANCE(2%, 1%)
		ONORDER = BALANCE(3%, 1%)

		CUR_ONHAND = BALANCE(1%, 2%)
		CUR_ALLOC = BALANCE(2%, 2%)
		CUR_ONORDER = BALANCE(3%, 2%)
	END IF

	!
	! Print header if it is the first line for the order
	!
	IF PRINT_HEADER% = 0%
	THEN
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		PRINT_HEADER% = -1%
	END IF

	!
	! Print line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + &
		"         " + &
		OE_REGLINE_READ::LIN + "   " + &
		OE_REGLINE_READ::PRODUCT + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + &
		FORMAT$(QTY(1%), "#,###,###,##") + &
		FORMAT$(QTY(2%), "#,###,###,##") + &
		FORMAT$(QTY(3%), "#,###,###,##") + &
		FORMAT$(QTY(7%), "#,###,###,##") + &
		FORMAT$(QTY(8%), "#,###,###,##") + &
		FORMAT$(ONHAND + CUR_ONHAND, "#,###,###,##")

 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	PRINT CGI_TEXT$(OE_REGHEADER::ORDNUM); ","; &
		CGI_TEXT$(OE_REGLINE_READ::LIN); ","; &
		CGI_TEXT$(OE_REGLINE_READ::PRODUCT); ","; &
		CGI_TEXT$(PD_PRODUCT_EXAM::DESCRIPTION); ",";  &
		CGI_NUMBER$(QTY(1%), 0%); ","; &
		CGI_NUMBER$(QTY(2%), 0%); ","; &
		CGI_NUMBER$(QTY(3%), 0%); ","; &
		CGI_NUMBER$(QTY(7%), 0%); ","; &
		CGI_NUMBER$(QTY(8%), 0%); ","; &
		CGI_TEXT$(OE_REGHEADER::CUSTPO)

	GOTO ReadRegline

 ExitProgram:

	EXIT PROGRAM

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	PRINT "**UNEXPECTED ERROR: "; &
		NUM1$(ERL) + " " + ERT$(ERR)

 !	UTL_REPORTX::STAT = -1%
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
