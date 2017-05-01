1	%TITLE "Register Compare Report"
	%SBTTL "PO_RPRT_REGCOMPARE_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! ID:PO033
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Register Comparison List\* option
	!	provides a report which contains the following:
	!	.table 3,25
	!	.te
	!	Purchase Order Number
	!	.te
	!	Purchase Order Line
	!	.te
	!	Total Quantity placed on Purchase Order
	!	.te
	!	Total Quantity Invoiced
	!	.te
	!	Total Quantity Received
	!	.end table
	!	.NOTE
	!	Currently assumes that all items for a line number have
	!	been assigned the same job number.
	!	.end note
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_REGCOMPARE_02/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_REGCOMPARE_02, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_REGCOMPARE_02.OBJ;*
	!
	! Author:
	!
	!	06/26/2000 - Kevin Handy
	!
	! Modification History:
	!
	!	08/02/2000 - Kevin Handy
	!		Lose extra function definitions
	!
	!	08/20/2001 - Kevin Handy
	!		Several fixes for FROM_ITEM bugs.
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! File Layout for: PO.PO_REG_SUB_LINE
	!
	! Purcahse Order Register Sub-Line
	!
	RECORD TEMP_PO_REG_SUB_LINE_CDD
		STRING SUBACCT = 10
		STRING PO = 10
		STRING PO_LINE = 4
		STRING PRODUCT = 14
		STRING VENDOR = 10
		GFLOAT ORDERED
		GFLOAT RECEIVED
		GFLOAT INVOICED
	END RECORD

	MAP (TEMP_PO_REG_SUB_LINE) TEMP_PO_REG_SUB_LINE_CDD TEMP_PO_REG_SUB_LINE

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION AP_EXAM_VENDOR
	EXTERNAL	REAL	FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Sort by	N,V,P\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	prints the list
	!	in order by a sort key.
	!	.b
	!	The valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*V\* - Vendor Number
	!	.te
	!	^*P\* - Product Number
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
	!	report will begin. The "item" entered must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
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
	!	The ^*To Item\* field specifies the item at which the
	!	report will end. The "item" entered must be in agreement with the value
	!	in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	QTY_EQ$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Only Unequal Quantities\*
	!	.b
	!	.lm +5
	!	The ^*Only Unequal Quantities\* field
	!	selects if the report is to include only the line items
	!	with unequal quantities. Selecting ^*Yes\* in this field will
	!	cause only the line items with unequal quantities to be printed.
	!	Selecting ^*No\* in this field will cause the report to print
	!	all of the line items.  An "_*_" will be printed at the
	!	beginning of the line items with unequal quantities.
	!	.lm -5
	!
	! Index:
	!
	!--

	CUTOFF$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Cutoff Date\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Date\*
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_SUB$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) From Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Date\*
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_SUB$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*(08) From Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Date\*
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Title
	!
	SELECT SORTBY$

	CASE "N"
		KEY_NUM% = 0%
		ADDTITLE$ = " BY PO NUMBER"

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

	CASE "V"
		KEY_NUM% = 2%
		ADDTITLE$ = " BY VENDOR"

	CASE "P"
		KEY_NUM% = 4%
		ADDTITLE$ = " BY PRODUCT NUMBER"

	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		AP_CONTROL::AP_ACCT = ""
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AP_CONTROL.CH%
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

400	!
	! Build sorted work file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Generating Sort File", 1%)
	CNTR% = 0%

	WHEN ERROR IN

		CALL ASSG_CHANNEL(TEMP_PO_REG_SUB_LINE.CH%, STAT%)
		TEMP_PO_REG_SUB_LINE.NAME$ = "TEMP_PO_REG_SUB_LINE.TMP"

		SELECT SORTBY$

		CASE "N"
			OPEN TEMP_PO_REG_SUB_LINE.NAME$ AS FILE TEMP_PO_REG_SUB_LINE.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				MAP TEMP_PO_REG_SUB_LINE, &
				PRIMARY KEY &
				( &
					TEMP_PO_REG_SUB_LINE::SUBACCT, &
					TEMP_PO_REG_SUB_LINE::PO, &
					TEMP_PO_REG_SUB_LINE::PO_LINE &
				) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE


		CASE "P"
			OPEN TEMP_PO_REG_SUB_LINE.NAME$ AS FILE TEMP_PO_REG_SUB_LINE.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				MAP TEMP_PO_REG_SUB_LINE, &
				PRIMARY KEY &
				( &
					TEMP_PO_REG_SUB_LINE::SUBACCT, &
					TEMP_PO_REG_SUB_LINE::PRODUCT, &
					TEMP_PO_REG_SUB_LINE::PO_LINE &
				) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE

		CASE "V"
			OPEN TEMP_PO_REG_SUB_LINE.NAME$ AS FILE TEMP_PO_REG_SUB_LINE.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				MAP TEMP_PO_REG_SUB_LINE, &
				PRIMARY KEY &
				( &
					TEMP_PO_REG_SUB_LINE::SUBACCT, &
					TEMP_PO_REG_SUB_LINE::VENDOR, &
					TEMP_PO_REG_SUB_LINE::PO &
				) DUPLICATES, &
				ACCESS MODIFY, ALLOW NONE

		END SELECT

	USE
		FILENAME$ = "PO_TEMP"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		RESET #PO_REG_SUB_LINE.CH%
		GET #PO_REG_SUB_LINE.CH%
	USE
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

420	FLAG% = 0%	! Anything to write flag

	!
	! New subline. Create a header for it
	!
	TEMP_PO_REG_SUB_LINE::SUBACCT = PO_REG_SUB_LINE::SUBACCT
	TEMP_PO_REG_SUB_LINE::PO = PO_REG_SUB_LINE::PO
	TEMP_PO_REG_SUB_LINE::PO_LINE = PO_REG_SUB_LINE::PO_LINE
	TEMP_PO_REG_SUB_LINE::ORDERED = 0.0
	TEMP_PO_REG_SUB_LINE::RECEIVED = 0.0
	TEMP_PO_REG_SUB_LINE::INVOICED = 0.0

	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, &
			KEY #0% EQ PO_REG_SUB_LINE::PO + &
				PO_REG_SUB_LINE::PO_LINE, &
			REGARDLESS
	USE
		PO_REG_LINE::PRODUCT = "??????????????"
		PO_REG_LINE::VENDOR = "??????????"
	END WHEN

	TEMP_PO_REG_SUB_LINE::PRODUCT = PO_REG_LINE::PRODUCT
	TEMP_PO_REG_SUB_LINE::VENDOR = PO_REG_LINE::VENDOR

430	SELECT SORTBY$

	CASE "N"
		GOTO 450 IF (PO_REG_SUB_LINE::PO < FROM_ITEM$)
		GOTO ReportTitle IF (PO_REG_SUB_LINE::PO > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO 450 IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::PO, -1%), WLDCRD$) = 0%
		END IF

	CASE "V"
		GOTO 450 IF (PO_REG_LINE::VENDOR < FROM_ITEM$)
		GOTO 450 IF (PO_REG_LINE::VENDOR > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO 450 IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0%
		END IF

	CASE "P"
		GOTO 450 IF (PO_REG_LINE::PRODUCT < FROM_ITEM$)
		GOTO 450 IF (PO_REG_LINE::PRODUCT > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO 450 IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0%
		END IF

	END SELECT


	!
	! Place information into correct columns
	!
	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"	! On Order
		TEMP_PO_REG_SUB_LINE::ORDERED = &
			TEMP_PO_REG_SUB_LINE::ORDERED + PO_REG_SUB_LINE::QTY
		FLAG% = -1%

	CASE "02"	! Received (posted)
		TEMP_PO_REG_SUB_LINE::RECEIVED = &
			TEMP_PO_REG_SUB_LINE::RECEIVED + PO_REG_SUB_LINE::QTY
		FLAG% = -1%

	CASE "03"	! Canceled (posted)
		TEMP_PO_REG_SUB_LINE::ORDERED = &
			TEMP_PO_REG_SUB_LINE::ORDERED + PO_REG_SUB_LINE::QTY
		FLAG% = -1%

	CASE "09"	! Invoiced (posted
		TEMP_PO_REG_SUB_LINE::INVOICED = &
			TEMP_PO_REG_SUB_LINE::INVOICED + PO_REG_SUB_LINE::QTY
		FLAG% = -1%

	END SELECT

450	!
	! Grab next record to look at
	!
	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%
	USE
		CONTINUE ReportTitle
	END WHEN

	IF TEMP_PO_REG_SUB_LINE::PO = PO_REG_SUB_LINE::PO AND &
		TEMP_PO_REG_SUB_LINE::PO_LINE = PO_REG_SUB_LINE::PO_LINE
	THEN
		!
		! Still on the same line
		!
		GOTO 430
	ELSE
		!
		! PO/Line changing, so let's start a new record
		!
		IF (TEMP_PO_REG_SUB_LINE::SUBACCT >= FROM_SUB$) AND &
			(TEMP_PO_REG_SUB_LINE::SUBACCT <= TO_SUB$ OR &
			TO_SUB$ = "")
		THEN
			BALANCE = FUNC_ROUND(TEMP_PO_REG_SUB_LINE::RECEIVED - &
				TEMP_PO_REG_SUB_LINE::INVOICED, 2%)

			IF (BALANCE <> 0.0) OR (QTY_EQ$ <> "Y")
			THEN
				PUT #TEMP_PO_REG_SUB_LINE.CH% IF FLAG%
			END IF
		END IF

		IF CNTR% >= 100%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Generating Sort File " + &
				TEMP_PO_REG_SUB_LINE::PO, 1%)
			CNTR% = 0%
		ELSE
			CNTR% = CNTR% + 1%
		END IF

		GOTO 420
	END IF

 ReportTitle:
	!
	! Possibly save last entry
	!
	IF (TEMP_PO_REG_SUB_LINE::SUBACCT >= FROM_SUB$) AND &
		(TEMP_PO_REG_SUB_LINE::SUBACCT <= TO_SUB$ OR &
		TO_SUB$ = "")
	THEN
		IF (BALANCE <> 0.0) OR (QTY_EQ$ <> "Y")
		THEN
			PUT #TEMP_PO_REG_SUB_LINE.CH% IF FLAG%
		END IF
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Sort file created", 1%)

	TITLE$(1%) = "INVENTORY RECEIVER COMPARISON LIST" + ADDTITLE$

	SELECT QTY_EQ$

	CASE "Y"
		TITLE$(2%) = "Unequal Quantities"

	CASE "N"
		TITLE$(2%) = "All Quantities"

	END SELECT

	TITLE$(3%) = "Purchase Order System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "SubAcct    PO_Number  VenNum     VenNam     Line " + &
		"ProductNumber    Po_Qty  Rec_Qty  Inv_Qty" + &
		"      InvBal         Price     ExtPrice"

	TITLE$(6%) = "."

	%PAGE

	TOTAL, EXT = 0.0

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	CNTR% = 0%
	TOTAL_ORDERED = 0.0
	TOTAL_INVOICED = 0.0
	TOTAL_RECEIVED = 0.0
	TOTAL_EXT = 0.0

	SUBTOTAL_ORDERED = 0.0
	SUBTOTAL_INVOICED = 0.0
	SUBTOTAL_RECEIVED = 0.0
	SUBTOTAL_EXT = 0.0

	LAST_SUBACCOUNT$ = ""

	WHEN ERROR IN
		RESET #TEMP_PO_REG_SUB_LINE.CH%, KEY #0%
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PO_TEMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #TEMP_PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	IF LAST_SUBACCOUNT$ <> TEMP_PO_REG_SUB_LINE::SUBACCT
	THEN
		GOSUB PrintSubtotal
		LAST_SUBACCOUNT$ = TEMP_PO_REG_SUB_LINE::SUBACCT
	END IF

	BALANCE = FUNC_ROUND(TEMP_PO_REG_SUB_LINE::RECEIVED - &
		TEMP_PO_REG_SUB_LINE::INVOICED, 2%)

	!
	! Flag unequal quantities when all of the lines
	! are being printed
	!
	IF (QTY_EQ$ = "N") AND (BALANCE <> 0.0)
	THEN
		QTY_EQ_FLAG$ = "??"
	ELSE
		QTY_EQ_FLAG$ = "  "
	END IF

	!
	! Get PO line
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, &
			KEY #0% EQ TEMP_PO_REG_SUB_LINE::PO + &
			TEMP_PO_REG_SUB_LINE::PO_LINE, &
			REGARDLESS
	USE
		!
		! Fake up one if we can't find original
		!
		PO_REG_LINE::PO = TEMP_PO_REG_SUB_LINE::PO
		PO_REG_LINE::PO_LINE = TEMP_PO_REG_SUB_LINE::PO_LINE
		PO_REG_LINE::VENDOR = TEMP_PO_REG_SUB_LINE::VENDOR
		PO_REG_LINE::FROMLOCATION = ""
		PO_REG_LINE::PRODUCT = TEMP_PO_REG_SUB_LINE::PRODUCT
		PO_REG_LINE::UOM = ""
		PO_REG_LINE::DESCRIPTION = ""
		PO_REG_LINE::PO_TYPE = ""
		PO_REG_LINE::OPEN_CLOSE = ""
		PO_REG_LINE::ORDDATE = ""
		PO_REG_LINE::BATCH = ""
		PO_REG_LINE::PERIOD = ""
	END WHEN

	!
	! Get the vendor name
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	RATE = PC_READ_COST(TEMP_PO_REG_SUB_LINE::PRODUCT, &
		PO_REG_LINE::FROMLOCATION, PO_REG_LINE::ORDDATE, "")

	IF RATE = 0.0
	THEN
		RATE = PO_REG_SUB_LINE_READ::PRICE
	END IF

	EXT = FUNC_ROUND(BALANCE * RATE, 2%)

	TEXT$ = TEMP_PO_REG_SUB_LINE::SUBACCT + " " + &
		CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + " " + &
		PO_REG_LINE::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 10%) + " " + &
		PO_REG_LINE::PO_LINE + " " + &
		PO_REG_LINE::PRODUCT + &
		FORMAT$(TEMP_PO_REG_SUB_LINE::ORDERED, "#,###,###") + &
		FORMAT$(TEMP_PO_REG_SUB_LINE::INVOICED, "#,###,###") + &
		FORMAT$(TEMP_PO_REG_SUB_LINE::RECEIVED, "#,###,###") + &
		FORMAT$(BALANCE, "#,###,###.##") + " " + &
		QTY_EQ_FLAG$ + &
		FORMAT$(RATE, "####,###.##") + &
		FORMAT$(EXT, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CNTR% = CNTR% + 1%
	TOTAL_ORDERED = TOTAL_ORDERED + TEMP_PO_REG_SUB_LINE::ORDERED
	TOTAL_INVOICED = TOTAL_INVOICED + TEMP_PO_REG_SUB_LINE::INVOICED
	TOTAL_RECEIVED = TOTAL_RECEIVED + TEMP_PO_REG_SUB_LINE::RECEIVED
	TOTAL_EXT = TOTAL_EXT + EXT

	SUBTOTAL_ORDERED = SUBTOTAL_ORDERED + TEMP_PO_REG_SUB_LINE::ORDERED
	SUBTOTAL_INVOICED = SUBTOTAL_INVOICED + TEMP_PO_REG_SUB_LINE::INVOICED
	SUBTOTAL_RECEIVED = SUBTOTAL_RECEIVED + TEMP_PO_REG_SUB_LINE::RECEIVED
	SUBTOTAL_EXT = SUBTOTAL_EXT + EXT

	EXT = 0.0

 ExitPrint:
	GOTO GetNextRec

 PrintSubtotal:
	IF CNTR% <> 0%
	THEN
		TEXT$ = "          " + " " + &
			"SUB TOTAL  " + &
			"          " + " " + &
			"          " + " " + &
			"    " + " " + &
			"              " + &
			FORMAT$(SUBTOTAL_ORDERED, "#,###,###") + &
			FORMAT$(SUBTOTAL_INVOICED, "#,###,###") + &
			FORMAT$(SUBTOTAL_RECEIVED, "#,###,###") + &
			FORMAT$(-SUBTOTAL_INVOICED + SUBTOTAL_RECEIVED, "#,###,###.##") + " " + &
			"  " + &
			"           " + &
			FORMAT$(SUBTOTAL_EXT, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SUBTOTAL_ORDERED = 0.0
	SUBTOTAL_INVOICED = 0.0
	SUBTOTAL_RECEIVED = 0.0
	SUBTOTAL_EXT = 0.0

	CNTR% = 0%

	RETURN

 ExitTotal:
	GOSUB PrintSubtotal

	TEXT$ = "          " + " " + &
		"GRAND TOTAL" + &
		"          " + " " + &
		"          " + " " + &
		"    " + " " + &
		"              " + &
		FORMAT$(TOTAL_ORDERED, "#,###,###") + &
		FORMAT$(TOTAL_INVOICED, "#,###,###") + &
		FORMAT$(TOTAL_RECEIVED, "#,###,###") + &
		FORMAT$(-TOTAL_INVOICED + TOTAL_RECEIVED, "#,###,###.##") + " " + &
		"  " + &
		"           " + &
		FORMAT$(TOTAL_EXT, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
