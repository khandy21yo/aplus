1	%TITLE "Product Sales by Quantity"
	%SBTTL "OE_RPRT_SALQTY"
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
	! ID:OE023
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
	!	Quantity shipped	Price
	!	.te
	!	Cost	Margin
	!	.te
	!	Percent Profit or Loss
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Item Quantity by Broker
	!	.x Item Quantity by Broker>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SALQTY/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SALQTY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SALQTY.OBJ;*
	!
	! Author:
	!
	!	12/16/91 - Dan Perkins
	!
	! Modification History:
	!
	!	01/16/91 - Dan Perkins
	!		Changed quantities to display in integer form.
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
	!		Changes to accomodate changes in Regheader file
	!		layout.  Changed Temp to single key depending on
	!		Sortby.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
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
	!	12/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

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
	RECORD TEMP_QTY_CDD
		STRING SALNUM = 10%
		STRING CUSNUM = 10%
		STRING PRODNUM = 14%
		STRING TDATE = 8%
		STRING ORDNUM = 10%
		STRING LIN = 4%
		STRING SORTQTY = 12%
		REAL   QTY
		REAL   PRICE
		REAL   COST
		REAL   DISC
	END RECORD

	MAP (TEMP_QTY)		TEMP_QTY_CDD		TEMP_QTY

	!
	! Declare external functions
	!
	EXTERNAL STRING	FUNCTION CONV_STRING
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION SA_EXAM_SALESMAN

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
	!	The ^*Sort by\* field determines if the report
	!	is to be printed in Customer Number order, Product
	!	Number order, or Salesman order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Salesman
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Item Quantity by Broker>Sort by
	!
	!--

	FROM.DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(1%)), -1%)

	!++
	! Abstract:FLD02
	!	.x From Date>Item Quantity by Broker Report
	!	^*(02) From Date\*
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

	TO.DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(2%)), -1%)

	!++
	! Abstract:FLD03
	!	.x To Date>Item Quantity by Broker Report
	!	^*(03) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies the date with which
	!	the report will end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the last date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects particular items to be printed by
	!	entering a "wildcard".  The value entered must be in
	!	agreement with the value in field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques
	!	refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
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

	CASE "P"
		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP TEMP_QTY, &
				PRIMARY KEY &
				( &
					TEMP_QTY::SORTQTY, &
					TEMP_QTY::PRODNUM &
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
				MAP TEMP_QTY, &
				PRIMARY KEY &
				( &
					TEMP_QTY::SORTQTY, &
					TEMP_QTY::PRODNUM, &
					TEMP_QTY::CUSNUM &
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
				MAP TEMP_QTY, &
				PRIMARY KEY &
				( &
					TEMP_QTY::SORTQTY, &
					TEMP_QTY::PRODNUM, &
					TEMP_QTY::SALNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	END SELECT

1010	!
	! Start at front of Regline
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

	GOTO ReadRecord IF OE_REGLINE::TRANTYPE <> "02"
	GOTO ReadRecord IF OE_REGLINE::QTY = 0.0
	GOTO ReadRecord IF OE_REGLINE::TDATE < FROM.DATE$
	GOTO ReadRecord IF OE_REGLINE::TDATE > TO.DATE$ AND TO.DATE$ <> ""

	!
	! Get Header, Customer, and Product info
	!
	V% = OE_READ_REGHEADER(OE_REGLINE::ORDNUM, OE_REGHEADER_READ)

	!
	! Figure out the sort qty
	!
	SORTQTY = 100000000.00 - OE_REGLINE::QTY

	!
	! Create a new record
	!
	TEMP_QTY::SALNUM	= OE_REGHEADER_READ::SALESMAN
	TEMP_QTY::CUSNUM	= OE_REGHEADER_READ::CUSNUM
	TEMP_QTY::PRODNUM	= OE_REGLINE::PRODUCT
	TEMP_QTY::TDATE		= OE_REGLINE::TDATE
	TEMP_QTY::ORDNUM	= OE_REGLINE::ORDNUM
	TEMP_QTY::LIN		= OE_REGLINE::LIN
	TEMP_QTY::SORTQTY	= FORMAT$(SORTQTY, "#########.##")
	TEMP_QTY::QTY		= OE_REGLINE::QTY
	TEMP_QTY::PRICE		= OE_REGLINE::PRICE
	TEMP_QTY::COST		= OE_REGLINE::COST
	TEMP_QTY::DISC		= OE_REGLINE::DISCOUNT

1300	!
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

	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SHIPPED PRODUCT SALES BY PRODUCT"

	CASE "N"
		K_NUM% = 1%
		TITLE$(1%) = "SHIPPED PRODUCT SALES BY CUSTOMER"

	CASE "S"
		K_NUM% = 2%
		TITLE$(1%) = "SHIPPED PRODUCT SALES BY SALESMAN"

	END SELECT

	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM.DATE$, 8%) + " To " + PRNT_DATE(TO.DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO.DATE$, 8%) IF FROM.DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM.DATE$, 8%) IF TO.DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM.DATE$ + TO.DATE$ = ""
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Prod#          ProdDescription             ShpQty "  + &
		"Customer#  CusName                        Salesman#" + &
		"  SalesmanName"

	TITLE$(6%) = "     Doc#         Lin#   ShpDate     Disc%         " + &
		"  Price        ExtPrice            Cost         "  + &
		"ExtCost          Margin    Gross%"

	TITLE$(7%) = "."

	%PAGE

	!
	! Initialize variables
	!
	TEMP.PRODNUM$ = ""
	SUBTOT.QTY = 0.0
	SUBTOT.PRICE = 0.0
	SUBTOT.COST = 0.0
	GRAND.QTY = 0.0
	GRAND.PRICE = 0.0
	GRAND.COST = 0.0

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		RESET #OE_TEMP.CH%
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
		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			TEMP_QTY::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			TEMP_QTY::PRODNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			TEMP_QTY::SALNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	TEMP.PRODNUM$ = TEMP_QTY::PRODNUM IF TEMP.PRODNUM$ = ""

	GOSUB SubTotal IF TEMP_QTY::PRODNUM <> TEMP.PRODNUM$

	!
	! Get Product information
	!
	V% = PD_EXAM_PRODUCT(TEMP_QTY::PRODNUM, PD_PRODUCT_EXAM)

	!
	! Get Customer information
	!
	V% =  AR_EXAM_CUSTOM(TEMP_QTY::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Fnd out the salesman's name
	!
	V% = SA_EXAM_SALESMAN(TEMP_QTY::SALNUM, SA_SALESMAN_EXAM, &
		SB_SUBACCOUNT_EXAM)

	TEXT$ = TEMP_QTY::PRODNUM  + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(TEMP_QTY::QTY, "##,###,###,##") + " " + &
		TEMP_QTY::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		TEMP_QTY::SALNUM + " " + &
		SA_SALESMAN_EXAM::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Figure totals for other fields
	!
	EXT.PRICE    = FUNC_ROUND(TEMP_QTY::QTY * TEMP_QTY::PRICE, 2%)
	EXT.COST     = FUNC_ROUND(TEMP_QTY::QTY * TEMP_QTY::COST, 2%)
	SUBTOT.QTY   = SUBTOT.QTY + TEMP_QTY::QTY
	SUBTOT.PRICE = SUBTOT.PRICE + EXT.PRICE
	SUBTOT.COST  = SUBTOT.COST + EXT.COST
	MARG         = EXT.PRICE - EXT.COST
	PROFIT       = (EXT.PRICE - EXT.COST) / EXT.PRICE * 100.0 &
			IF EXT.PRICE <> 0.0

	TEXT$ = "     " + CONV_STRING(TEMP_QTY::ORDNUM, CMC$_LEFT) + "   " + &
		TEMP_QTY::LIN                                      + "   " + &
		PRNT_DATE(TEMP_QTY::TDATE, 6%)                     + "   " + &
		FORMAT$(TEMP_QTY::DISC, "###.##")                  + "   " + &
		FORMAT$(TEMP_QTY::PRICE, "##,###,###.##")          + "   " + &
		FORMAT$(EXT.PRICE, "##,###,###.##")                + "   " + &
		FORMAT$(TEMP_QTY::COST, "##,###,###.##")           + "   " + &
		FORMAT$(EXT.COST, "##,###,###.##")                 + "   " + &
		FORMAT$(MARG, "##,###,###.##")                     + "   " + &
		FORMAT$(PROFIT, "####.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

 SubTotal:
	MARG = SUBTOT.PRICE - SUBTOT.COST
	PROFIT = (SUBTOT.PRICE - SUBTOT.COST) / SUBTOT.PRICE * 100.0 &
		IF SUBTOT.PRICE <> 0.0

	TEXT$ = "  Total for product#  " + &
		TEMP.PRODNUM$ + &
		FORMAT$(SUBTOT.QTY, "##,###,###,##") + SPACE$(11%) + &
		FORMAT$(SUBTOT.PRICE, "###,###,###.##") + SPACE$(18%) + &
		FORMAT$(SUBTOT.COST, "###,###,###.##") + "  " + &
		FORMAT$(MARG, "###,###,###.##") + "   " + &
		FORMAT$(PROFIT, "####.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 999%)

	TEMP.PRODNUM$ = TEMP_QTY::PRODNUM
	GRAND.QTY = GRAND.QTY + SUBTOT.QTY
	GRAND.PRICE = GRAND.PRICE + SUBTOT.PRICE
	GRAND.COST = GRAND.COST + SUBTOT.COST
	SUBTOT.QTY = 0.0
	SUBTOT.PRICE = 0.0
	SUBTOT.COST = 0.0

	RETURN

 ExitTotal:
	GOSUB SubTotal

	MARG = GRAND.PRICE - GRAND.COST
	PROFIT = (GRAND.PRICE - GRAND.COST) / GRAND.PRICE * 100.0 &
		IF GRAND.PRICE <> 0.0

	TEXT$ = "  T O T A L   F O R   A L L "         + "       "     + &
		FORMAT$(GRAND.QTY, "###,###,###,##")   + SPACE$(11%)   + &
		FORMAT$(GRAND.PRICE, "###,###,###.##") + SPACE$(18%)   + &
		FORMAT$(GRAND.COST, "###,###,###.##")  + "  "          + &
		FORMAT$(MARG, "###,###,###.##")        + "   "         + &
		FORMAT$(PROFIT, "####.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "  P R O D U C T S  "

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

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
