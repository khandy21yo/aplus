1	%TITLE "Print Journal"
	%SBTTL "PO_RPRT_ORDERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Journal\* option prints a batch of purchase
	!	orders prior to posting the batch.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Report
	!	.x Report>Purchase Order Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_ORDERJOUR/LINE
	!	$ LINK/EXE:PO_EXE PO_RPRT_ORDERJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE PO_RPRT_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!	06/27/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	10/23/90 - Kevin Handy
	!		Put dash into PO number.
	!
	!	10/23/90 - Kevin Handy
	!		Major Formatting Changes.
	!
	!	01/24/92 - Dan Perkins
	!		More formatting changes.  Added GL_OUTP_ACCTSUM
	!		function to display account information at end
	!		of report.  Cleaned program code.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/11/92 - Dan Perkins
	!		More record layout changes.  Added BATCH field.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/03/92 - Dan Perkins
	!		Added code to pad PO Nmbers with zeros in FROM ITEM
	!		and TO ITEM fields if PO Number is selected in sort
	!		option.
	!
	!	03/17/92 - Dan Perkins
	!		Removed code that changed PO from being RSET to LSET.
	!		Program wouldn't print lines or sub_lines because
	!		PO key was changed.  Program wasn't tested after
	!		whoever made change or he would have seen it didn't
	!		work.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM becasue of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/28/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)	PO_ORDERSLINE_CDD	PO_ORDERSLINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Purchase Order Journal
	!	.x Purchase Order Journal>Batch Number
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort By\*
	!	.b
	!	.lm +5
	!	The ^*Sort By\* field enters a code which will cause
	!	Purchase Order Journal to print in a selected order.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Purchase Order Journal
	!	.x Purchase Order Journal>Sort By
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a value which will cause the
	!	report to begin printing with the value specified.  The value
	!	entered must be in agreement with field (02) Sort by.
	!	.b
	!	If this field is left blank, the report will begin with the first purchase
	!	order in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Purchase Order Journal
	!	.x Purchase Order Journal>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a value with which the
	!	report is to end printing.  The value entered must be in
	!	agreement with field (02) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last purchase order in
	!	the file, assuming the report is selected to print in purchase order number.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Purchase Order Journal
	!	.x Purchase Order Journal>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enters a wildcard value which
	!	causes selected purchase orders to be printed.  If a
	!	value is entered, it must be in agreement with the sort selection made in field
	!	(01), i.e. if the report is selected to print in purchase order number, this
	!	field must be blank or contain a purchase order number.
	!	.b
	!	If this field is blank, all purchase orders in the batch will be printed
	!	in the journal report.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Purchase Order Journal
	!	.x Purchase Order Journal>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.OPN"
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"

		AP_CONTROL::AP_ACCT = ""
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AP_CONTROL.CH%
	USE
		CONTINUE ReportTitle IF ERR = 5% OR ERR = 155%
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	SELECT SORT_BY$
	CASE "N"
		SORT_KEY% = 0%
		TITLE$(1%) = "PURCHASE ORDERS SORTED BY PURCHASE ORDER"

	!
	! Routine to load left justified spaces into FROM_ITEM$
	! and TO_ITEM$ if any order numbers are entered as ranges
	!
	FROM_ITEM$ = SPACE$(LEN(PO_ORDERJOUR::PO) - &
		LEN(FROM_ITEM$)) + &
		FROM_ITEM$ IF FROM_ITEM$ <> ""

	TO_ITEM$ = SPACE$(LEN(PO_ORDERJOUR::PO) - &
		LEN(TO_ITEM$)) + &
		TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "V"
		SORT_KEY% = 1%
		TITLE$(1%) = "PURCHASE ORDERS SORTED BY VENDOR"

	CASE "T"
		SORT_KEY% = 2%
		TITLE$(1%) = "PURCHASE ORDERS SORTED BY TYPE"

	END SELECT

	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Purchase Order System"
	TITLE$(4%) = ""
	TITLE$(5%) = "PO_Number   Typ PO_Date  Vendor     " + &
		"Vendor_Name                    Vendor_Rep " + &
		"Col/Ppd Terms Carrier FOB Ack PrntForm"

	TITLE$(6%) = SPACE$(17%) + &
		"Location Buyer      Operator"

	TITLE$(7%) = SPACE$(22%) + &
		"Line Our_Product#   Description               " + &
		"UOM Ven_Product#    Ven_Price      Tot_Qty" + &
		"    Ext_Price"

	TITLE$(8%) = SPACE$(46%) + &
		"Req_Date            Qty GL_Account         " + &
		"Subacct"

	TITLE$(9%) = "."

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	TOT_QTY = 0.0
	EXT_PRICE = 0.0
	TOTAL_PRICE = 0.0
	SPACE_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_ORDERJOUR.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PO_ORDERJOUR.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PO_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "N"
		GOTO ExitTotal IF (PO_ORDERJOUR::PO > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::PO, -1%), WLDCRD$) = 0%

	CASE "V"
		GOTO ExitTotal IF (PO_ORDERJOUR::VENDOR > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::VENDOR, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (PO_ORDERJOUR::POTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::POTYPE, -1%), WLDCRD$) = 0%

	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF SPACE_FLAG%

	V% = AP_EXAM_VENDOR(PO_ORDERJOUR::VENDOR, AP_VENDOR_EXAM)

	TEXT$ = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT) + "  " + &
		PO_ORDERJOUR::POTYPE + "  " + &
		PRNT_DATE(PO_ORDERJOUR::PODATE, 6%) + " " + &
		PO_ORDERJOUR::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + " " + &
		PO_ORDERJOUR::SALESMAN + " " + &
		PO_ORDERJOUR::COL_PPD + "       " + &
		PO_ORDERJOUR::TERMS + "    " + &
		PO_ORDERJOUR::CARRIER + "      " + &
		PO_ORDERJOUR::FOB + "  " + &
		PO_ORDERJOUR::ACKNOW + "  " + &
		PO_ORDERJOUR::PRINTFORM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT) + &
		SPACE$(7%) + &
		PO_ORDERJOUR::FROMLOCATION + "     " + &
		PO_ORDERJOUR::BUYER + " " + &
		PO_ORDERJOUR::OPERATOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SPACE_FLAG% = -1%

17100	WHEN ERROR IN
		FIND #PO_ORDERLINE.CH%, KEY #0% EQ PO_ORDERJOUR::PO, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 GetLine:
17120	WHEN ERROR IN
		GET #PO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF PO_ORDERLINE::PO <> PO_ORDERJOUR::PO

	GOSUB CalcTotal

	TEXT$ = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT) + &
		SPACE$(12%) + &
		PO_ORDERLINE::PO_LINE + " " + &
		PO_ORDERLINE::OUR_PRODUCT + " " + &
		LEFT(PO_ORDERLINE::DESCRIPTION, 25%) + " " + &
		PO_ORDERLINE::OUR_UOM + "  " + &
		PO_ORDERLINE::VEN_PRODUCT + " " + &
		FORMAT$(PO_ORDERLINE::VEN_PRICE, "###,###.##") + " " + &
		FORMAT$(TOT_QTY, "#,###,###.##") + " " + &
		FORMAT$(EXT_PRICE, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOT_QTY = 0.0
	EXT_PRICE = 0.0

17200	WHEN ERROR IN
		FIND #PO_ORDERSLINE.CH%, &
			KEY #0% EQ PO_ORDERLINE::PO + &
			PO_ORDERLINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE Getline IF ERR = 155%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

 GetSubline:
17220	WHEN ERROR IN
		GET #PO_ORDERSLINE.CH%, REGARDLESS
	USE
		CONTINUE Getline IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

	GOTO Getline IF PO_ORDERSLINE::PO <> PO_ORDERLINE::PO OR &
		PO_ORDERSLINE::PO_LINE <> PO_ORDERLINE::PO_LINE

	TEXT$ = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT) + &
		SPACE$(36%) + &
		PRNT_DATE(PO_ORDERSLINE::RECEIVEDATE, 8%) + " " + &
		FORMAT$(PO_ORDERSLINE::OUR_QTY, "#,###,###.##") + " " + &
		PO_ORDERSLINE::GL_ACCOUNT + " " + &
		PO_ORDERSLINE::SUBACCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetSubline

 ExitTotal:
	V% = GL_OUTP_ACCTSUM(OPT_ADDREC, AP_CONTROL::AP_ACCT, &
		0.0, -TOTAL_PRICE, 0.0, TITLE$(), UTL_REPORTX)

	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

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

 CalcTotal:
17300	WHEN ERROR IN
		FIND #PO_ORDERSLINE.CH%, KEY #0% EQ PO_ORDERLINE::PO + &
			PO_ORDERLINE::PO_LINE, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 155%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

 GetSublineOnce:
17320	WHEN ERROR IN
		GET #PO_ORDERSLINE.CH%, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

	GOTO OutaHere IF PO_ORDERSLINE::PO <> PO_ORDERLINE::PO OR &
		PO_ORDERSLINE::PO_LINE <> PO_ORDERLINE::PO_LINE

	TOT_QTY = TOT_QTY + PO_ORDERSLINE::OUR_QTY

	PRICE = FUNC_ROUND(PO_ORDERSLINE::OUR_QTY * PO_ORDERLINE::VEN_PRICE, 2%)

	EXT_PRICE = EXT_PRICE + PRICE

	TOTAL_PRICE = TOTAL_PRICE + PRICE

	V% = GL_OUTP_ACCTSUM(OPT_ADDREC, PO_ORDERSLINE::GL_ACCOUNT, &
		0.0, PRICE, 0.0, TITLE$(), UTL_REPORTX)

	GOTO GetSublineOnce

 OutaHere:
	RETURN

	%PAGE

  HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
