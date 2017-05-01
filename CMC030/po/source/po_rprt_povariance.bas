1	%TITLE "Purchase Order Price Variance"
	%SBTTL "PO_RPRT_POVARIANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:PO002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Price Variance\* report
	!	prints variances between the standard and actual costs.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_POVARIANCE/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_POVARIANCE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_POVARIANCE.OBJ;*
	!
	! Author:
	!
	!	02/26/93 - Dan Perkins
	!
	! Modification History:
	!
	!	03/01/93 - Dan Perkins
	!		Only print a product line if defined in the product
	!		file.  Added Variance Totals.  Calculate standard
	!		cost on invoice date.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL	FUNCTION PC_READ_COST

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
	!	.TS 55
	!	^*(01) Sort by	N,T,V,P,B\*
	!	.B
	!	.LM +5
	!	The ^*Sort by\* field
	!	determines the sorting
	!	order in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*B\* - Batch Number
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
	!	The ^*From Item\* field enters the item from which the
	!	report will begin. The value entered must be in agreement with the value
	!	in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Register
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item at which the
	!	report will end. The value entered must be in agreement with the contents in
	!	field (01) Sort by.
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
	!	The ^*Wildcard\* field enters a wildcard value which
	!	will cause selected purchase orders to be printed.
	!	.b
	!	If this field is blank, all purchase orders in the batch will be printed
	!	in the journal report.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) From Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date with which
	!	the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	dated item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	.ts 55
	!	^*(07) To Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the date with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the most recent
	!	date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Register Sub Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "N"
		K_NUM%  = 0%
		TITLE$(1%) = "PO VARIANCE REPORT BY PURCHASE ORDER"

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$ &
			IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(TO_ITEM$)) + TO_ITEM$ &
			IF TO_ITEM$ <> ""

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "PO VARIANCE REPORT BY PO TYPE"

	CASE "V"
		K_NUM% = 2%
		TITLE$(1%) = "PO VARIANCE REPORT BY VENDOR NUMBER"

	CASE "B"
		K_NUM% = 3%
		TITLE$(1%) = "PO VARIANCE REPORT BY BATCH NUMBER"

	CASE "P"
		K_NUM% = 4%
		TITLE$(1%) = "PO VARIANCE REPORT BY PRODUCT NUMBER"

	END SELECT

	TITLE$(2%) = "Purchase Order System"

	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)

	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""

	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "PO#        Line Vendor     Name            "      + &
		"Product        Description          A/P Date    " + &
		"InvQty   StdPrice   ActPrice  Variance"

	TITLE$(6%) = "."

	!
	! Set some date ranges if FROM_DATE$
	! and TO_DATE$ are blank
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	TOTVAR = 0.0

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_REG_LINE.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_REG_LINE.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Register record
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (PO_REG_LINE::PO > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PO_REG_LINE::PO_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "V"
		GOTO ExitTotal IF (PO_REG_LINE::VENDOR > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "B"
		GOTO ExitTotal IF (PO_REG_LINE::BATCH > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::BATCH, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (PO_REG_LINE::PRODUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Get the product description
	!
	GOTO GetNextRec IF PD_EXAM_PRODUCT(PO_REG_LINE::PRODUCT, &
		PD_PRODUCT_EXAM) <> CMC$_NORMAL

17100	!
	! Check current Register Sub Line for invoiced records
	!
	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + &
			PO_REG_LINE::PO_LINE + "09", &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	INVQTY, ACTCOST = 0.0

 GetLine:
17120	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLine IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO &
		OR PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE &
		OR PO_REG_SUB_LINE::PO_ACTION <> "09"

	GOTO GetLine IF PO_REG_SUB_LINE::ACTION_DATE < FROM_DATE$ OR &
		PO_REG_SUB_LINE::ACTION_DATE > TO_DATE$

	LASTDATE$ = PO_REG_SUB_LINE::ACTION_DATE

	INVQTY = INVQTY + PO_REG_SUB_LINE::QTY

	ACTCOST = ACTCOST + FUNC_ROUND(PO_REG_SUB_LINE::QTY * &
		PO_REG_SUB_LINE::PRICE, 2%)

	GOTO GetLine

 PrintLine:
	GOTO GetNextRec IF INVQTY = 0.0

	!
	! Get the vendor name
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	!
	! Get the standard cost
	!
	COST = PC_READ_COST(PO_REG_LINE::PRODUCT, &
		PO_REG_LINE::FROMLOCATION, LASTDATE$, "")

	STDCOST = FUNC_ROUND(INVQTY * COST, 2%)

	VAR = ACTCOST - STDCOST

	TOTVAR = TOTVAR + VAR

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + " " + &
		PO_REG_LINE::PO_LINE + " " + &
		PO_REG_LINE::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 15%) + " " + &
		PO_REG_LINE::PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		PRNT_DATE(LASTDATE$, 8%) + " " + &
		FORMAT$(INVQTY, "###,###") + " " + &
		FORMAT$(STDCOST, "###,###.##") + " " + &
		FORMAT$(ACTCOST, "###,###.##") + " " + &
		FORMAT$(VAR, "##,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitTotal:
	TEXT$ = SPACE$(98%) + "VARIANCE TOTALS:      " + &
		FORMAT$(TOTVAR, "##,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
