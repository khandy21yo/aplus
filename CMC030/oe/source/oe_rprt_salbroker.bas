1	%TITLE "Item Quantity by Broker Report"
	%SBTTL "OE_RPRT_SALBROKER"
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
	! ID:OE014
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Item Quantity By Broker\* Report contains columns for the following
	!	information:
	!	.table 3,25
	!	.te
	!	Salesman Number	Salesman Name
	!	.te
	!	Customer _################Customer Name
	!	.te
	!	Product Number	Product Description
	!	.te
	!	Document Date	Document Number
	!	.te
	!	Qty. ordered, shipped, or canceled
	!	.te
	!	Price	Cost
	!	.te
	!	Margin	Percent Profit or Loss
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Item Quantity by Broker
	!	.x Item Quantity by Broker>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SALBROKER/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SALBROKER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SALBROKER.OBJ;*
	!
	! Author:
	!
	!	12/09/91 - Dan Perkins
	!
	! Modification History:
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display in integer form.
	!
	!	01/20/92 - Dan Perkins
	!		Add subtotals for customer or product, depending
	!		on sort.  Re worked program code.
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
	!	06/02/92 - Dan Perkins
	!		Changed to accomodate changes in Regheader file
	!		layout.  Changed Temp file to only one key depending
	!		on Sortby value.
	!		This is ONE MESSED UP PROGRAM WHOSE VALUE NEEDS
	!		TO BE DETIRMINED.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
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
	! Include scope.com and CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Define record structure(s)
	!
	RECORD OE_TEMP_CDD
		STRING SALNUM = 10%
		STRING SALESMAN = 40%
		STRING CUSNUM = 10%
		STRING CUSNAM = 50%
		STRING ALPHA = 15%
		STRING PRODNUM = 14%
		STRING PRODDES = 40%
		STRING ORDDATE = 8%
		STRING ORDNUM = 10%
		STRING LIN = 4%
		REAL   QTY
		REAL   PRICE
		REAL   COST
		REAL   DISC
		REAL   SALCOMM
	END RECORD

	MAP (OE_TEMP)		OE_TEMP_CDD		OE_TEMP

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION SA_EXAM_SALESMAN

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
	!	.x Sort by>Item Quantity by Broker
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order in which
	!	the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*A\* - Customer Alpha
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*D\* - Product Description
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Item Quantity by Broker>Sort by
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
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item
	!	with which the report is to end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Sort By Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Sort by Wildcard\* field
	!	selects particular items to be printed by
	!	entering a "wildcard".  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	QTY_EXAM$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	.x Item Select>Item Quantity by Broker
	!	^*(06) Item to Report\*
	!	.b
	!	.lm +5
	!	The ^*Item to Report\* field determines
	!	if the report is to print quantities that are Ordered, Shipped, or Canceled.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*O\* - Ordered Items
	!	.te
	!	^*S\* - Shipped Items
	!	.te
	!	^*C\* - Canceled Items
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Item Quantity by Broker>Quantity Select
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	.x From Date>Item Quantity by Broker Report
	!	^*(07) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date
	!	from which the report is to begin printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to begin with the first date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(7%)), -1%)

	!++
	! Abstract:FLD08
	!	.x To Date>Item Quantity by Broker Report
	!	^*(08) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies the most recent
	!	date with which the report will end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the most recent date
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	SALESMAN_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	^*(09) Salesman Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Salesman Wildcard\* field
	!	selects designated salesmen to be printed by entering a
	!	"wildcard".
	!	.b
	!	For information on "Wildcarding" techniques
	!	refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Salesman Wildcard
	!
	!--

300	!
	! Open REGHEADER file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open REGLINE file
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

	CASE "A"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::ALPHA, &
					OE_TEMP::SALNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	CASE "N"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::CUSNUM, &
					OE_TEMP::SALNUM &
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
					OE_TEMP::PRODNUM, &
					OE_TEMP::SALNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	CASE "D"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP OE_TEMP, &
				PRIMARY KEY &
				( &
					OE_TEMP::PRODDES, &
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
	! Start at front of OE_REGHEADER
	!
	RESET #OE_REGHEADER.CH%

 ReadRecord:
1100	!
	! Read in one record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"

		CONTINUE HelpError
	END WHEN

	GOTO ReadRecord &
		IF COMP_STRING(OE_REGHEADER::SALESMAN, SALESMAN_WLDCRD$) = 0% &
		AND SALESMAN_WLDCRD$ <> ""

1200	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% GE OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE ReadRecord IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegline:
1210	WHEN ERROR IN
		GET #OE_REGLINE.CH%
	USE
		CONTINUE ReadRecord IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ReadRecord IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	GOTO GetRegline IF OE_REGLINE::QTY = 0.0

	SELECT QTY_EXAM$

	CASE "O"
		GOTO GetRegline IF OE_REGLINE::TRANTYPE <> "01"
		GOTO ReadRecord IF OE_REGHEADER::ORDDATE < FROM_DATE$
		GOTO ReadRecord &
			IF OE_REGHEADER::ORDDATE > TO_DATE$ AND TO_DATE$ <> ""
		CHECK_DATE$ = OE_REGHEADER::ORDDATE

	CASE "S"
		GOTO GetRegline IF OE_REGLINE::TRANTYPE <> "02"
		GOTO GetRegline IF OE_REGLINE::TDATE < FROM_DATE$
		GOTO GetRegline &
			IF OE_REGLINE::TDATE > TO_DATE$ AND TO_DATE$ <> ""
		CHECK_DATE$ = OE_REGLINE::TDATE

	CASE "C"
		GOTO GetRegline IF OE_REGLINE::TRANTYPE <> "03"
		GOTO GetRegline IF OE_REGLINE::TDATE < FROM_DATE$
		GOTO GetRegline &
			IF OE_REGLINE::TDATE > TO_DATE$ AND TO_DATE$ <> ""
		CHECK_DATE$ = OE_REGLINE::TDATE

	END SELECT

	!
	! Get the Customer and Product information
	!
	AR_35CUSTOM_EXAM::CUSNAM = &
		STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B) &
		IF AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, &
		AR_35CUSTOM_EXAM) <> CMC$_NORMAL

	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Fnd out the salesman's name
	!
	V% = SA_EXAM_SALESMAN(OE_REGHEADER::SALESMAN, &
		SA_SALESMAN_EXAM, SB_SUBACCOUNT_EXAM)

	!
	! Create a new record
	!
	OE_TEMP::SALNUM		= OE_REGHEADER::SALESMAN
	OE_TEMP::SALESMAN	= SA_SALESMAN_EXAM::DESCR
	OE_TEMP::CUSNUM		= OE_REGHEADER::CUSNUM
	OE_TEMP::CUSNAM		= AR_35CUSTOM_EXAM::CUSNAM
	OE_TEMP::ALPHA		= AR_35CUSTOM_EXAM::ALPSRT
	OE_TEMP::PRODNUM	= OE_REGLINE::PRODUCT
	OE_TEMP::PRODDES	= PD_PRODUCT_EXAM::DESCRIPTION
	OE_TEMP::ORDDATE	= CHECK_DATE$
	OE_TEMP::ORDNUM		= OE_REGHEADER::ORDNUM
	OE_TEMP::LIN		= OE_REGLINE::LIN
	OE_TEMP::QTY		= OE_REGLINE::QTY
	OE_TEMP::PRICE		= OE_REGLINE::PRICE
	OE_TEMP::COST		= OE_REGLINE::COST
	OE_TEMP::DISC		= OE_REGLINE::DISCOUNT
	OE_TEMP::SALCOMM	= OE_REGHEADER::SALCOMM

1300	!
	! Add record
	!
	WHEN ERROR IN
		PUT #OE_TEMP.CH%
	USE
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO GetRegline

	%PAGE

 ReportTitle:
	!
	! Title
	!
	SELECT QTY_EXAM$

	CASE "O"
		ADD$  = " ORDERED ITEMS "
		ADD1$ = "OrdDate"

	CASE "S"
		ADD$ = " SHIPPED ITEMS "
		ADD1$ = "ShpDate"

	CASE "C"
		ADD$ = " CANCELLED ITEMS "
		ADD1$ = "CanDate"

	END SELECT

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "BROKER" + ADD$ + "BY CUSTOMER NUMBER"

	CASE "A"
		K_NUM% = 1%
		TITLE$(1%) = "BROKER" + ADD$ + "BY CUSTOMER ALPHA SORT"

	CASE "P"
		K_NUM% = 2%
		TITLE$(1%) = "BROKER" + ADD$ + "BY PRODUCT NUMBER"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "BROKER" + ADD$ + "BY PRODUCT DESCRIPTION"

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
	TITLE$(5%) = "Salesman#  SalesmanName         Customer#"      + &
		"  CusName                        Prod#"         + &
		"          ProdDescription      " + ADD1$ + "  Doc#"

	TITLE$(6%) = "     DocLin#     Quantity    Disc%           Price" + &
		"        ExtPrice            Cost         "          + &
		"ExtCost          Margin    Gross%"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	TEMP_SALESMAN$ = ""
	TEMP_LINE$ = ""
	SALESMAN_PRICE = 0.0
	GRAND_PRICE = 0.0

	!
	! If from category blank then reset OE_TEMP file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_TEMP.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_TEMP.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
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
		GOTO ExitTotal IF (OE_TEMP::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		TEMP_LINE$ = OE_TEMP::CUSNUM IF TEMP_LINE$ = ""
		C$ = "       Customer Total"
		GOSUB Changed IF OE_TEMP::CUSNUM <> TEMP_LINE$

	CASE "A"
		GOTO ExitTotal IF (OE_TEMP::ALPHA > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::ALPHA, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		TEMP_LINE$ = OE_TEMP::ALPHA IF TEMP_LINE$ = ""
		C$ = "       Customer Total"
		GOSUB Changed IF OE_TEMP::ALPHA <> TEMP_LINE$

	CASE "P"
		GOTO ExitTotal IF (OE_TEMP::PRODNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::PRODNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		TEMP_LINE$ = OE_TEMP::PRODNUM IF TEMP_LINE$ = ""
		C$ = "       Product Total "
		GOSUB Changed IF OE_TEMP::PRODNUM <> TEMP_LINE$

	CASE "D"
		GOTO ExitTotal IF (OE_TEMP::PRODDES > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_TEMP::PRODDES, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		TEMP_LINE$ = OE_TEMP::PRODDES IF TEMP_LINE$ = ""
		C$ = "       Product Total "
		GOSUB Changed IF OE_TEMP::PRODDES <> TEMP_LINE$

	END SELECT

	TEMP_SALESMAN$ = OE_TEMP::SALESMAN IF TEMP_SALESMAN$ = ""
	GOSUB Salesman IF OE_TEMP::SALESMAN <> TEMP_SALESMAN$

	TEXT$ = OE_TEMP::SALNUM + " " + &
		LEFT$(OE_TEMP::SALESMAN, 20%) + " " + &
		OE_TEMP::CUSNUM + " " + &
		LEFT$(OE_TEMP::CUSNAM, 30%) + " " + &
		OE_TEMP::PRODNUM + " " + &
		LEFT(OE_TEMP::PRODDES, 20%) + " " + &
		PRNT_DATE(OE_TEMP::ORDDATE, 6%) + " " + &
		CONV_STRING(OE_TEMP::ORDNUM, CMC$_LEFT)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Figure totals for other fields
	!
	EXT_PRICE = FUNC_ROUND(OE_TEMP::QTY * OE_TEMP::PRICE, 2%)
	EXT_COST = FUNC_ROUND(OE_TEMP::QTY * OE_TEMP::COST, 2%)
	MARG = EXT_PRICE - EXT_COST

	IF EXT_PRICE <> 0.0
	THEN
		PROFIT = (EXT_PRICE - EXT_COST) / EXT_PRICE * 100.0
	ELSE
		PROFIT = 0.0
	END IF

	TEXT$ = "     " + OE_TEMP::LIN + "   " + &
		FORMAT$(OE_TEMP::QTY, "##,###,###,##") + "   " + &
		FORMAT$(OE_TEMP::DISC, "###.##") + "   " + &
		FORMAT$(OE_TEMP::PRICE, "##,###,###.##") + "   " + &
		FORMAT$(EXT_PRICE, "##,###,###.##") + "   " + &
		FORMAT$(OE_TEMP::COST, "##,###,###.##")  + "   " + &
		FORMAT$(EXT_COST, "##,###,###.##") + "   " + &
		FORMAT$(MARG, "##,###,###.##") + "   " + &
		FORMAT$(PROFIT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	CHANGED_PRICE = CHANGED_PRICE + EXT_PRICE
	CHANGED_COST = CHANGED_COST + EXT_COST

	EXT_PRICE = 0.0
	EXT_COST = 0.0

	GOTO GetNextRec

  ExitTotal:
	GOSUB Changed
	GOSUB Salesman

	GRAND_MARG = GRAND_PRICE - GRAND_COST

	IF GRAND_PRICE <> 0.0
	THEN
		GRAND_PROFIT = (GRAND_PRICE - GRAND_COST) / GRAND_PRICE * 100.0
	ELSE
		GRAND_PROFIT = 0.0
	END IF

	TEXT$ = "       T O T A L   F O R   A L L   S A L E S M E N  " + &
		FORMAT$(GRAND_PRICE, "###,###,###.##") + SPACE$(18%) + &
		FORMAT$(GRAND_COST, "###,###,###.##")  + "  " + &
		FORMAT$(GRAND_MARG, "###,###,###.##")  + "   " + &
		FORMAT$(GRAND_PROFIT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 Changed:
	CHANGED_MARG = CHANGED_PRICE - CHANGED_COST

	IF CHANGED_PRICE <> 0.0
	THEN
		CHANGED_PROFIT = &
			(CHANGED_PRICE - CHANGED_COST) / CHANGED_PRICE * 100.0
	ELSE
		CHANGED_PROFIT = 0.0
	END IF

	TEXT$ = C$                                       + SPACE$(31%) + &
		FORMAT$(CHANGED_PRICE, "###,###,###.##") + SPACE$(18%) + &
		FORMAT$(CHANGED_COST, "###,###,###.##")  + "  " + &
		FORMAT$(CHANGED_MARG, "###,###,###.##")  + "   " + &
		FORMAT$(CHANGED_PROFIT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	SELECT SORTBY$
	CASE "N"
		TEMP_LINE$ = OE_TEMP::CUSNUM

	CASE "A"
		TEMP_LINE$ = OE_TEMP::ALPHA

	CASE "P"
		TEMP_LINE$ = OE_TEMP::PRODNUM

	CASE "D"
		TEMP_LINE$ = OE_TEMP::PRODDES

	END SELECT

	SALESMAN_PRICE = SALESMAN_PRICE + CHANGED_PRICE
	SALESMAN_COST = SALESMAN_COST + CHANGED_COST

	CHANGED_PRICE = 0.0
	CHANGED_COST = 0.0

	RETURN

 Salesman:
	SALESMAN_MARG = SALESMAN_PRICE - SALESMAN_COST

	IF SALESMAN_PRICE <> 0.0
	THEN
		SALESMAN_PROFIT = &
			(SALESMAN_PRICE - SALESMAN_COST) / &
			SALESMAN_PRICE * 100.0
	ELSE
		SALESMAN_PROFIT = 0.0
	END IF

	TEXT$ = "       Total for salesman   " + &
		LEFT$(TEMP_SALESMAN$, 20%) + "    " + &
		FORMAT$(SALESMAN_PRICE, "###,###,###.##") + SPACE$(18%) + &
		FORMAT$(SALESMAN_COST, "###,###,###.##")  + "  "   + &
		FORMAT$(SALESMAN_MARG, "###,###,###.##")  + "   "  + &
		FORMAT$(SALESMAN_PROFIT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 999%)

	TEMP_SALESMAN$ = OE_TEMP::SALESMAN
	GRAND_PRICE = GRAND_PRICE + SALESMAN_PRICE
	GRAND_COST = GRAND_COST + SALESMAN_COST

	SALESMAN_PRICE = 0.0
	SALESMAN_COST = 0.0

	RETURN

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
