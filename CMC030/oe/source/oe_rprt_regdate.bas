1	%TITLE "Most Recent Customer Order Report"
	%SBTTL "OE_RPRT_REGDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:OE031
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Customer Order Status Summary\* Report contains
	!	the following information:
	!	.Table 3,25
	!	.te
	!	Customer Number	Customer Type
	!	.te
	!	Customer Category	Document Number
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
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Most Recent Order By Customer
	!	.x Most Recent Order By Customer>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGDATE/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGDATE.OBJ;*
	!
	! AUTHOR:
	!
	!	07/11/91 - Craig Tanner
	!
	! MODIFICATION HISTORY:
	!
	!	09/03/91 - Deborah K. Fries
	!		Reformatted output for readability.
	!
	!	09/20/91 - Dan Perkins
	!		Use functions to read files.
	!		Streamlined code.
	!		Cleaned up error trapping.
	!
	!	11/06/91 - Dan Perkins
	!		Allow duplicate key in TEMP
	!		file.  Show back orders by
	!		read_regline function.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to lest ORDER NUMBER.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/30/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	MAP (OE_TEMP)	OE_TEMP.ORDNUM$    = 10%, &
			OE_TEMP.ORDTYPE$   =  2%, &
			OE_TEMP.ORDCAT$    =  4%, &
			OE_TEMP.CUSNUM$    = 10%, &
			OE_TEMP.CUSTPO$    = 10%, &
			OE_TEMP.REVS_DATE$ =  8%, &
			OE_TEMP.ORDDATE$   =  8%, &
			OE_TEMP.LOCATION$  =  4%, &
			OE_TEMP.SHIPVIA$   =  2%

	!
	! Declare external functions
	!
	EXTERNAL LONG   FUNCTION OE_READ_REGLINE
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

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
	!	.x Sort by>Name/Address Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order in
	!	which the report is to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Customer Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Name/Address Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	customer number with which the report will begin printing.
	!	The value must be in agreement with field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	customer in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Name/Address Report
	!	.x Name/Address Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To item\* field determines the customer
	!	number with which the report will end printing.
	!	The value entered must be in agreement with field (01)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to end with the last customer
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Most Recent Order By Customer Report
	!	.x Most Recent Order By Customer Report>To Item
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
	!	for wildcarding technique.
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

330	!
	! Open Custom file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

340	!
	! Open temporary file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file. " + &
		"Reading work files.", 1% + 16%)

	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE #OE_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP OE_TEMP, &
			PRIMARY KEY (OE_TEMP.CUSNUM$, &
				OE_TEMP.REVS_DATE$) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Start reading register
	!
500	WHEN ERROR IN
		RESET #OE_REGHEADER.CH%
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	DATE_NUMBER% = VAL%(EDIT$(OE_REGHEADER::ORDDATE, -1%))
	DATE_NUMBER% = 99999999% - DATE_NUMBER%
	OE_TEMP.REVS_DATE$ = NUM1$(DATE_NUMBER%)

	OE_TEMP.ORDNUM$ = OE_REGHEADER::ORDNUM
	OE_TEMP.ORDDATE$ = OE_REGHEADER::ORDDATE
	OE_TEMP.ORDCAT$ = OE_REGHEADER::ORDCAT
	OE_TEMP.ORDTYPE$ = OE_REGHEADER::ORDTYPE
	OE_TEMP.CUSNUM$ = OE_REGHEADER::CUSNUM
	OE_TEMP.CUSTPO$ = OE_REGHEADER::CUSTPO
	OE_TEMP.LOCATION$ = OE_REGHEADER::LOCATION
	OE_TEMP.SHIPVIA$ = OE_REGHEADER::SHIPVIA

	PUT #OE_TEMP.CH%

	GOTO 510

 ReportTitle:
	!
	! Title
	!
	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "CusNumber  CusName                              Doc#  " + &
		"      CusType     CusCat CustPo#     OrdDate" + &
		"     Location       Shipvia"

	TITLE$(5%) = "                         Line   Product        Descr" + &
		"                       QtyOrd      QtyShip"	+ &
		"    QtyCancel      QtyBack"

	TITLE$(6%) = "."

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = " MOST RECENT ORDER BY CUSTOMER NUMBER"
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " MOST RECENT ORDER BY CUSTOMER TYPE"
	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " MOST RECENT ORDER BY CUSTOMER CATEGORY"
	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = " MOST RECENT ORDER ALPHABETICAL BY CUSTOMER"
	END SELECT

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Read customer name
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::CUSNUM, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""
	CASE "T"
		GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::TTYPE, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::CATEGORY, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::ALPSRT, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""
	END SELECT

	!
	! Get temp order register header record
	!
17125	WHEN ERROR IN
		GET #OE_TEMP.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	DATE_NUMBER% = VAL%(EDIT$(OE_TEMP.REVS_DATE$, -1%))
	DATE_NUMBER% = 99999999% - DATE_NUMBER%
	PRINT_DATE$ = FORMAT$(DATE_NUMBER%, "########")

	TEXT$ = OE_TEMP.CUSNUM$ + " " + &
		LEFT$(AR_35CUSTOM::CUSNAM, 30%) + "       " + &
		CONV_STRING(OE_TEMP.ORDNUM$, CMC$_LEFT) + "  " + &
		AR_35CUSTOM::TTYPE + "           " + &
		AR_35CUSTOM::CATEGORY + "   " + &
		OE_TEMP.CUSTPO$ + " " + &
		PRNT_DATE(PRINT_DATE$, 8%) + "  " + &
		OE_TEMP.LOCATION$ + "           " + &
		OE_TEMP.SHIPVIA$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT_LINE$ = "   "

 ReadRegLine:
	GOTO NewOrder IF OE_READ_REGLINE (OE_TEMP.ORDNUM$, NEXT_LINE$, "GT", &
		OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	!
	! Get the product
	!
	IF PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::DESCRIPTION = ""
	END IF

	!
	! Print line with backorders
	!
	TEXT$ = OE_TEMP.CUSNUM$ + "               " + &
		OE_REGLINE_READ::LIN + "   " + &
		OE_REGLINE_READ::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + "  " + &
		FORMAT$(QTY(1%), "#,###,###,##") + " " + &
		FORMAT$(QTY(2%), "#,###,###,##") + " " + &
		FORMAT$(QTY(3%), "#,###,###,##") + " " + &
		FORMAT$(QTY(7%), "#,###,###,##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadRegLine

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

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
