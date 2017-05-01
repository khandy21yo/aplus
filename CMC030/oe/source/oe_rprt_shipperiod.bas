1	%TITLE "Shipped Items by Period Report"
	%SBTTL "OE_RPRT_SHIPPERIOD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:OE032
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Shipped Items by Period\* Report contains columns for the following
	!	information:
	!	.table 3,25
	!	.te
	!	Salesman
	!	.te
	!	Customer
	!	.te
	!	Customer Name
	!	.te
	!	Product Number
	!	.te
	!	Product Description
	!	.te
	!	Quantity Shipped
	!	.te
	!	Price
	!	.te
	!	Cost
	!	.te
	!	Gross Margin
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Shipped Items by Period
	!	.x Shipped Items by Period>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SHIPPERIOD/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SHIPPERIOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SHIPPERIOD.OBJ;*
	!
	! Author:
	!
	!	11/14/91 - Dan Perkins
	!
	! Modification History:
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	06/04/92 - Dan Perkins
	!		Changed Temp file to single key based on Sortby.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
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
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Define record structure(s)
	!
	RECORD OE_TEMP_CDD
		STRING SALNUM = 10%
		STRING CUSNUM = 10%
		STRING PRODNUM = 14%
		STRING TRADAT = 8%
		REAL   QTY
		REAL   PRICE
		REAL   COST
	END RECORD

	MAP (OE_TEMP)		OE_TEMP_CDD		OE_TEMP

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

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
	!	.x Sort by>Shipped Items in Period
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines if the report
	!	is to be printed in customer number order, product
	!	number order or salesman number order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Salesman Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Shipped Items in Period>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins printing the report
	!	with a specified item. The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank in this field will cause the report to begin with the first item in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Order Status Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies an item with which
	!	the report is to end printing.
	!	The value entered must be in agreement with field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Order Status Summary
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard".  The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date
	!	from which the report is to begin printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to begin with the first item
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Shipped Items by Period
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	.x To Date>Shipped Items by Period
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies the date
	!	with which the report is to end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

1000	!*******************************************************************
	! Generate sort file
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	!
	! Create work file
	!
	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	SELECT SORTBY$

	CASE "N"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::CUSNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	CASE "P"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::PRODNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	CASE "S"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::SALNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	END SELECT

1010	!
	! Start at front of OE_REGLINE
	!
	RESET #OE_REGLINE.CH%

 ReadRecord:
1100	!
	! Read in one record
	!
	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check the ranges
	!
	GOTO ReadRecord IF OE_REGLINE::TRANTYPE <> "02"
	GOTO ReadRecord IF OE_REGLINE::TDATE < FROM_DATE$
	GOTO ReadRecord IF OE_REGLINE::TDATE > TO_DATE$ AND TO_DATE$ <> ""

	IF OLD_KEY$ = OE_REGLINE::ORDNUM + OE_REGLINE::LIN
	THEN
		TOT_QTY   = TOT_QTY + OE_REGLINE::QTY
		TOT_PRICE = TOT_PRICE + OE_REGLINE::PRICE
		TOT_COST  = TOT_COST + OE_REGLINE::COST
		GOTO ReadRecord
	ELSE
		OLD_KEY$ = OE_REGLINE::ORDNUM + OE_REGLINE::LIN
	END IF

	!
	! Get info from the Header File
	!
	V% = OE_READ_REGHEADER(OE_REGLINE::ORDNUM, OE_REGHEADER_READ)

	!
	! Create a new record
	!
	OE_TEMP::SALNUM		= OE_REGHEADER_READ::SALESMAN
	OE_TEMP::CUSNUM		= OE_REGHEADER_READ::CUSNUM
	OE_TEMP::PRODNUM	= OE_REGLINE::PRODUCT
	OE_TEMP::TRADAT		= OE_REGLINE::TDATE
	OE_TEMP::QTY		= TOT_QTY
	OE_TEMP::PRICE		= TOT_PRICE
	OE_TEMP::COST		= TOT_COST

1180	!
	! Add record
	!
	WHEN ERROR IN
		PUT #OE_TEMP.CH%
	USE
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO ReadRecord

	%PAGE

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "N"
		TITLE$(1%) = "SHIPPED ITEMS IN PERIOD BY CUSTOMER"

	CASE "P"
		TITLE$(1%) = "SHIPPED ITEMS IN PERIOD BY PRODUCT"

	CASE "S"
		TITLE$(1%) = "SHIPPED ITEMS IN PERIOD BY SALESMAN"

	END SELECT

	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Salesman#  Customer#  CusName              " + &
		"Product#       ProductDescr         Date   " + &
		"   Quantity     ExtPrice      ExtCost"

	TITLE$(6%) = "."

	!
	! Check to see if the Order Date
	! is in desired range
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset OE_TEMP file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_TEMP.CH%
		ELSE
			FIND #OE_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #OE_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record, see if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (OE_TEMP::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (OE_TEMP::PRODNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::PRODNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (OE_TEMP::SALNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::SALNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Get Customer info
	!
	V% = AR_EXAM_CUSTOM(OE_TEMP::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Get Product info
	!
	V% = PD_EXAM_PRODUCT(OE_TEMP::PRODNUM, PD_PRODUCT_EXAM)

	TEXT$ = OE_TEMP::SALNUM + " " + &
		OE_TEMP::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 20%) + " " + &
		OE_TEMP::PRODNUM + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		PRNT_DATE(OE_TEMP::TRADAT, 6%) + " " + &
		FORMAT$(OE_TEMP::QTY, "#,###,###") + " " + &
		FORMAT$(OE_TEMP::PRICE, "#,###,###.##") + " " + &
		FORMAT$(OE_TEMP::COST, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetNextRec

 ExitTotal:

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
