1	%TITLE "Order Status Detail"
	%SBTTL "OE_RPRT_REGISTER"
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
	! ID:OE016
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Status Detail\* Report contains the following fields of information:
	!	.table 3,25
	!	.te
	!	Document _#	Sales Type
	!	.te
	!	Sales Category	Customer _#
	!	.te
	!	Customer Name	Customer PO _#
	!	.te
	!	Order Date	Location
	!	.te
	!	Ship Via	Line _#
	!	.te
	!	Transaction Type	Product _#
	!	.te
	!	Product Description	Quantity
	!	.te
	!	Quantity Type	Transaction Date
	!	.te
	!	Posting Date	Posting Time
	!	.te
	!	Batch _# for Process	Shipment Release _#
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Status Detail
	!	.x Order Status Detail>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGISTER/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGISTER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGISTER.OBJ;*
	!
	! AUTHOR:
	!
	!
	! MODIFICATION HISTORY:
	!
	!	08/28/91 - Deborah K. Fries
	!		Reformatted output printout for readability
	!
	!	09/23/91 - Dan Perkins
	!		Added READ and EXAM functions to code to
	!		eliminate opening files.
	!		Streamlined code, checked error trapping.
	!
	!	12/12/91 - Dan Perkins
	!		Changed call to AR_EXAM_CUSTOM so that
	!		question marks are printed in the customer
	!		name field if the customer record is not found.
	!		Compensates for undefined customers.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to lset ORDER NUMBER.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/16/95 - Kevin Handy
	!		Reformatted to 80 columns.
	!		Lost excess externals.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include SCOPE.COM
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	!
	! Declare external functions
	!
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
	!	.x Sort by>Order Status Detail
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
	!	A setting is required in this field.
	!	. lm -5
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
	!	The ^*From Item\* field enters the item from which the
	!	report will begin printing printing.  The value entered must
	!	be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Order Status Detail
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item at which the
	!	report will end printing.  The value entered must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Order Status Detail
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard. The wildcard must be in agreement with the
	!	contents of field (01) Sort by.
	!	.b
	!	See Appendix B for more information regarding wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Order Status Detail
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
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "  REGISTER DETAIL REPORT BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "  REGISTER DETAIL REPORT BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = "  REGISTER DETAIL REPORT BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER DETAIL REPORT BY DOCUMENT NUMBER"
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
	TITLE$(4%) = "Doc#        ST SalCat   CusNumber   CusName  " + &
		"                      CustPo#    OrdDate     " + &
		"Location       Shipvia"
	TITLE$(5%) = "                 Line  TType  Product        " + &
		"Descr                        Qty   Ty   Re#   " + &
		"TranDate    PostDate    PostTime    Batch"
	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset Register file
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
	CASE "N"
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

	CASE "C"
		GOTO ExitProgram IF (OE_REGHEADER::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDCAT, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

	!
	! Check current Custom record
	!
	IF AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM) <> CMC$_NORMAL
	THEN
		AR_CUSTOM_EXAM::CUSNAM = STRING$(LEN(AR_CUSTOM_EXAM::CUSNAM), &
			A"?"B)
	END IF

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + " " + &
		OE_REGHEADER::ORDCAT + "     " + &
		OE_REGHEADER::CUSNUM + "  " + &
		LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		OE_REGHEADER::CUSTPO + " " + &
		PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
		OE_REGHEADER::LOCATION + "           " + &
		OE_REGHEADER::SHIPVIA

	!
	! Assign Variables for later totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17030	!
	! Check current Register Line record
	!
	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% EQ OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetLine:
17040	WHEN ERROR IN
		GET #OE_REGLINE.CH%
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintBlank IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	!
	! Get the Product Description
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT OE_REGLINE::TRANTYPE

	CASE "01"
		TTYPE$ = "O"

	CASE "02"
		TTYPE$ = "S"

	CASE "03"
		TTYPE$ = "C"
	END SELECT

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_REGLINE::ORDNUM, CMC$_LEFT) + "       " + &
		OE_REGLINE::LIN + "  " + &
		OE_REGLINE::TRANTYPE + "     " + &
		OE_REGLINE::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + &
		FORMAT$(OE_REGLINE::QTY, "#,###,###.##")	+ "   " + &
		TTYPE$ + "    " + &
		OE_REGLINE::SHIPNO + "    " + &
		PRNT_DATE(OE_REGLINE::TDATE, 8%) + "  " + &
		PRNT_DATE(OE_REGLINE::POSTDATE, 8%) + "  " + &
		PRNT_TIME(OE_REGLINE::POSTTIME, 0%) + "   " + &
		OE_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetLine

 PrintBlank:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
