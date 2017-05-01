1	%TITLE "Purchase Order Scheduled Receipts List"
	%SBTTL "PO_RPRT_SCHEDRECEIPTS"
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
	! ID:PO030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Scheduled Receipts List\* option will
	!	provide a report which contains the following:
	!	.table 3,25
	!	.te
	!	Purchase Order Number
	!	.te
	!	Purchase Order Line
	!	.te
	!	Purchase Order Type
	!	.te
	!	Vendor Number
	!	.te
	!	Vendor Name
	!	.te
	!	Product Number
	!	.te
	!	Product Description
	!	.te
	!	Unit of Measure
	!	.te
	!	Balance Due
	!	.te
	!	Expected Date
	!	.te
	!	Extended Price
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_SCHEDRECEIPTS/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_SCHEDRECEIPTS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_SCHEDRECEIPTS.OBJ;*
	!
	! Author:
	!
	!	07/17/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	10/09/91 - Deborah K. Fries
	!		Used functions to read files
	!		Cleaned source code
	!		Improved error trapping
	!
	!	01/30/92 - Dan Perkins
	!		Added PO Number to sort option.  Display
	!		PO Number in report.  Changed displayed date
	!		to RECEIVEDATE from CHK_DATE.
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/25/92 - Dan Perkins
	!		Modified program to reflect changes in file
	!		layouts for PO_REG_LINE and PO_REG_SUB_LINE.
	!		Added TOTAL cash requirement.
	!
	!	03/03/92 - Dan Perkins
	!		Added code to pad PO Nmbers with zeros in FROM ITEM
	!		and TO ITEM fields if PO Number is selected in sort
	!		option.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/28/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...)
	!		in several places.
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/04/2002 - Kevin Handy
	!		Add subtotals
	!
	!	01/17/2001 - Kevin Handy
	!		Add LYT_LINE information.
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
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Define record structure(s)
	!
	RECORD TEMP_REC_CDD
		STRING PONUM = 10%
		STRING POLINE =  4%
		STRING POTYPE =  2%
		STRING REQDATE =  8%
		STRING VENDOR = 10%
		STRING PRODNUM = 14%
		STRING UOM =  2%
		REAL   BALANCE
		REAL   PRICE
	END RECORD

	MAP (TEMP_REC)		TEMP_REC_CDD		TEMP_REC

	RECORD SUBTOTAL_DATE_CDD
		STRING PODATE = 8%
		REAL BALANCE
	END RECORD

	DECLARE INTEGER CONSTANT MAX_DATE = 64%
	DIM SUBTOTAL_DATE_CDD SUBTOTAL_DATE(MAX_DATE)

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION	PO_READ_REG_LINE
	EXTERNAL	LONG	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL	LONG	FUNCTION	PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(10) Sort (N,T,V,P)\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field
	!	prints the list in order by a sort key.
	!	.B
	!	Valid settings are:
	!	.TABLE 3,25
	!	.TE
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
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
	!	A ^*From Item\*
	!	field causes the printing
	!	to begin with a selected item.
	!	.b
	!	A blank setting will cause the report to begin with
	!	the first item in the file.
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
	!	A ^*To Item\*
	!	field causes the printing
	!	to end with a selected item.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
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

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	A ^*From Date\*
	!	field causes the printing
	!	to only print items whose Receive
	!	Date is after the From Date entered.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	A ^*To Date\*
	!	field causes the printing
	!	to only print items whose Receive
	!	Date is previous to the To Date entered.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open Purchase Order Register Line file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Set up Sort By Information
	!
	SELECT SORT_BY$
	CASE "N"
		ADD_TITLE$ = "PURCHASE ORDER"
		K_NUM% = 0%

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(TEMP_REC::PONUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$ &
			IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(TEMP_REC::PONUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$ &
			IF TO_ITEM$ <> ""

	CASE "T"
		ADD_TITLE$ = "PO TYPE"
		K_NUM% = 1%

	CASE "V"
		ADD_TITLE$ = "VENDOR NUMBER"
		K_NUM% = 2%

	CASE "P"
		ADD_TITLE$ = "PRODUCT NUMBER"
		K_NUM% = 3%

	END SELECT

	TITLE$(1%) = "PURCHASE ORDER SCHEDULED RECEIPTS"

	TITLE$(2%) = "LIST SORTED BY " + ADD_TITLE$ + " AND DATE"

	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)

	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) &
		IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) &
		IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" &
		IF FROM_DATE$ + TO_DATE$ = ""

	TITLE$(4%) = "Purchase Order System"
	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "PONumber    Line Typ VendorNumb VendorName       " + &
		"    ProductNumber  ProductDescription        " + &
		"UOM Bal_Due ExpcDate     Ext_Price"

	TITLE$(7%) = "."

	!
	! Set dates to Begining and end of time if left blank
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	LYT_LINE$ = "$PONUM:12,$POLINE:17,$POTYPE:21,$VENNUM:32," + &
		"$VENNAM:53,$PRODNUM:68,$PRODDESC:94,$UOM:98," + &
		"VBALANCE:106,$REQDATE:115,VEXT:132"

	LYT_LINE_TOTAL$ = "$TITLE:115,VEXT:132"

	%PAGE

1000	!***************************************************************
	! Create work file
	!***************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	CALL ASSG_CHANNEL(PO_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PO_TEMP.TMP" FOR OUTPUT &
			AS FILE PO_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP TEMP_REC, &
			PRIMARY KEY &
			( &
				TEMP_REC::PONUM, &
				TEMP_REC::REQDATE &
			) DUPLICATES, &
			ALTERNATE KEY &
			( &
				TEMP_REC::POTYPE, &
				TEMP_REC::REQDATE &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				TEMP_REC::VENDOR, &
				TEMP_REC::REQDATE &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				TEMP_REC::PRODNUM, &
				TEMP_REC::REQDATE &
			)	DUPLICATES CHANGES, &
			ALLOW NONE, &
			ACCESS MODIFY
	USE
		FILENAME$ = "PO_TEMP"
		CONTINUE HelpError
	END WHEN

11000	!***************************************************************
	! Set up array and TEMP file
	!***************************************************************

	SUBTOTAL_DATE% = 0%
	RESET #PO_REG_LINE.CH%

 GetNextRec:
11020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get REG_LINE Information
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE PrintReport IF ERR = 11% OR ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	V% = PO_READ_REG_LINE(PO_REG_LINE::PO, PO_REG_LINE::PO_LINE, &
		"EQ", PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
		QTY(), "")

	GOTO GetNextRec IF QTY(0%) = 0.0
	GOTO GetNextRec IF PO_REG_SUB_LINE_READ::ACTION_DATE < FROM_DATE$
	GOTO GetNextRec IF PO_REG_SUB_LINE_READ::ACTION_DATE > TO_DATE$

	TEMP_REC::PONUM   = PO_REG_LINE::PO
	TEMP_REC::POLINE  = PO_REG_LINE::PO_LINE
	TEMP_REC::POTYPE  = PO_REG_LINE::PO_TYPE
	TEMP_REC::VENDOR  = PO_REG_LINE::VENDOR
	TEMP_REC::PRODNUM = PO_REG_LINE::PRODUCT
	TEMP_REC::UOM = PO_REG_LINE::UOM
	TEMP_REC::REQDATE = PO_REG_SUB_LINE_READ::ACTION_DATE
	TEMP_REC::PRICE   = PO_REG_SUB_LINE_READ::PRICE
	TEMP_REC::BALANCE = QTY(0%)

	PUT #PO_TEMP.CH%

	GOTO GetNextRec

	!***************************************************************
	! Print TEMP file
17000	!***************************************************************
 PrintReport:
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_TEMP.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_TEMP.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_TEMP"
		CONTINUE HelpError
	END WHEN

	SUBTOTAL = 0.0
	SUBCOUNT% = 0%
	THIS_VENDOR$ = ""

	TOTAL = 0.0

 GetTempRec:
17020	WHEN ERROR IN
		GET #PO_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "N"
		GOTO ExitTotal IF (TEMP_REC::PONUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetTempRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TEMP_REC::PONUM, -1%), &
			WLDCRD$) = 0%

		GOSUB Subtotal IF TEMP_REC::PONUM <> THIS_VENDOR$

		THIS_VENDOR$ = TEMP_REC::PONUM

	CASE "P"
		GOTO ExitTotal IF (TEMP_REC::PRODNUM > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetTempRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TEMP_REC::PRODNUM, -1%), &
			WLDCRD$) = 0%

		GOSUB Subtotal IF TEMP_REC::PRODNUM <> THIS_VENDOR$

		THIS_VENDOR$ = TEMP_REC::PRODNUM

	CASE "V"
		GOTO ExitTotal IF (TEMP_REC::VENDOR > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetTempRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TEMP_REC::VENDOR, -1%), &
			WLDCRD$) = 0%

		GOSUB Subtotal IF TEMP_REC::VENDOR <> THIS_VENDOR$

		THIS_VENDOR$ = TEMP_REC::VENDOR

	CASE "T"
		GOTO ExitTotal IF (TEMP_REC::POTYPE > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetTempRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TEMP_REC::POTYPE, -1%), &
			WLDCRD$) = 0%

		GOSUB Subtotal IF TEMP_REC::POTYPE <> THIS_VENDOR$

		THIS_VENDOR$ = TEMP_REC::POTYPE

	END SELECT

	V% = AP_EXAM_VENDOR(TEMP_REC::VENDOR, AP_VENDOR_EXAM)

	V% = PD_EXAM_PRODUCT(TEMP_REC::PRODNUM, PD_PRODUCT_EXAM)

	EXT = FUNC_ROUND(TEMP_REC::BALANCE * TEMP_REC::PRICE, 2%)

	SUBTOTAL = SUBTOTAL + EXT
	SUBCOUNT% = SUBCOUNT% + 1%

	TOTAL = TOTAL + EXT

	TEXT$ = CONV_STRING(TEMP_REC::PONUM, CMC$_LEFT) + "  " + &
		TEMP_REC::POLINE + " " + &
		TEMP_REC::POTYPE + "  " + &
		TEMP_REC::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 20%) + " " + &
		TEMP_REC::PRODNUM + " "  + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%)	+ " " + &
		TEMP_REC::UOM + "  " + &
		FORMAT$(TEMP_REC::BALANCE, "###,###") + " " + &
		PRNT_DATE(TEMP_REC::REQDATE, 6%) + " " + &
		FORMAT$(EXT, "##,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Save totals by date
	!
	FOR LOOP% = 1% TO SUBTOTAL_DATE%
		IF SUBTOTAL_DATE(LOOP%)::PODATE = TEMP_REC::REQDATE
		THEN
			SUBTOTAL_DATE(LOOP%)::BALANCE = &
				FUNC_ROUND(SUBTOTAL_DATE(LOOP%)::BALANCE + &
				EXT, 2%)
			GOTO EndSummary
		END IF
	NEXT LOOP%

	IF (SUBTOTAL_DATE% < MAX_DATE)
	THEN
		SUBTOTAL_DATE% = SUBTOTAL_DATE% + 1%
		SUBTOTAL_DATE(SUBTOTAL_DATE%)::PODATE = TEMP_REC::REQDATE
		SUBTOTAL_DATE(SUBTOTAL_DATE%)::BALANCE = EXT
	ELSE
		SUBTOTAL_DATE(SUBTOTAL_DATE%)::PODATE = "XXXXXXXX"
		SUBTOTAL_DATE(SUBTOTAL_DATE%)::BALANCE = &
			FUNC_ROUND(SUBTOTAL_DATE(SUBTOTAL_DATE%)::BALANCE + &
			EXT, 2%)
	END IF

 EndSummary:
	GOTO GetTempRec

 ExitTotal:
	GOSUB Subtotal

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(62%) + &
		"T O T A L   C A S H   R E Q U I R E M E N T" + &
		SPACE$(10%) + &
		FORMAT$(TOTAL, "##,###,###.##")

	CALL OUTP_LINE(LYT_LINE_TOTAL$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print subtotals by date
	!
	CALL OUTP_LINE(LYT_LINE_TOTAL$, UTL_REPORTX, TITLE$(), "", 6%)

	FOR LOOP% = 1% TO SUBTOTAL_DATE%

		TEXT$ = SPACE$(56%) + &
			SPACE$(34%) + &
			"SUBTOTAL FOR " + &
			PRNT_DATE(SUBTOTAL_DATE(LOOP%)::PODATE, 8%) + &
			FORMAT$(SUBTOTAL_DATE(LOOP%)::BALANCE, &
			"  ##,###,###.##")

		CALL OUTP_LINE(LYT_LINE_TOTAL$, UTL_REPORTX, TITLE$(), &
			TEXT$, 0%)

	NEXT LOOP%

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

	!*******************************************************************
	! Print out subtotals
	!*******************************************************************

 Subtotal:

	RETURN IF SUBCOUNT% = 0%

	SELECT SORT_BY$

	CASE "N"	! No subtotals by po number
		GOTO SubtotalExit

	CASE "P"
		!

	CASE "V"
		!

	CASE "T"
		!

	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(56%) + &
		"S U B T O T A L   C A S H   R E Q U I R E M E N T" + &
		SPACE$(10%) + &
		FORMAT$(SUBTOTAL, "##,###,###.##")

	CALL OUTP_LINE(LYT_LINE_TOTAL$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)

 SubtotalExit:
	SUBCOUNT% = 0%
	SUBTOTAL = 0.0

	RETURN

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
	RESUME HelpError

32767	END
