1	%TITLE "Register Compare Report"
	%SBTTL "PO_RPRT_REGCOMPARE"
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
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_REGCOMPARE/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_REGCOMPARE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_REGCOMPARE.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Kevin Handy
	!
	! Modification History:
	!
	!	10/09/91 - Deborah K. Fries
	!		Cleaned source code
	!
	!	10/11/91 - JEFF BEARD
	!		ADDED THE "Our Product", "Balance", "Acct#", AND
	!		"Ext." COLUMNS
	!
	!	01/28/92 - Dan Perkins
	!		Added RATE, and EXT output to report.  Aligned
	!		columns.  Open AP_CONTROL file to get the credit
	!		account number.  Changed SCOPE.COM TO CODES.INC so
	!		GL_OUTP_ACCTSUM would work.  Set up GL_OUTP_ACCTSUM
	!		function to work properly.  Put REG_LINE file first
	!		so sort could be on various fields.  Added SORTBY
	!		to option list.
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/24/92 - Dan Perkins
	!		Changed to accomodate changes in file layouts.
	!
	!	02/25/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/03/92 - Dan Perkins
	!		Added code to pad PO Nmbers with zeros in FROM ITEM
	!		and TO ITEM fields if PO Number is selected in sort
	!		option.
	!
	!	03/18/92 - Dan Perkins
	!		Fixed problem printing same PO even after PO changed.
	!
	!	09/21/92 - Dan Perkins
	!		Print question mark flag if invoice qunatity is
	!		greater than order quantity.  Add vendor name to report.
	!
	!	09/29/92 - Dan Perkins
	!		Read product cost if price is zero.  Changed output
	!		display to show vendor info, then lines.
	!
	!	10/05/92 - Dan Perkins
	!		Changed title name of report.
	!
	!	10/23/92 - Dan Perkins
	!		Added argument to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	11/02/92 - Dan Perkins
	!		Changed tests for BALANCE variable to allow for balances
	!		less than zero.
	!
	!	02/02/93 - Dan Perkins
	!		Moved print section of program from a subroutine into
	!		the main program.  Use COMP_ARRAY instead of COMP_STRING.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	09/28/93 - Kevin Handy
	!		Modified to get rate first from PC_COST file, then
	!		from the PO subline file.
	!
	!	11/03/94 - Kevin Handy
	!		Modified to add a cutoff date.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/25/96 - Kevin Handy
	!		Attempt to get another digit displayed in printout.
	!
	!	01/28/96 - Kevin Handy
	!		Change STRING$(..., ASCII(" ")) to "" or
	!		SPACE$(...) in several places.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add some REGARDLESS
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION PO_READ_REG_LINE
	EXTERNAL	LONG    FUNCTION GL_OUTP_ACCTSUM
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
	!	^*(01) Sort by	N,T,V,B,P\*
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
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.te
	!	^*B\* - Batch Number
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

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$

	CASE "N"
		KEY_NUM% = 0%
		ADDTITLE$ = " BY PO NUMBER"

		!
		! Routine to load left justified spaces into FROM.ITEM
		! and TO.ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$ &
			IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(TO_ITEM$)) + TO_ITEM$ &
			IF TO_ITEM$ <> ""

	CASE "T"
		KEY_NUM% = 1%
		ADDTITLE$ = " BY PO TYPE"

	CASE "V"
		KEY_NUM% = 2%
		ADDTITLE$ = " BY VENDOR"

	CASE "B"
		KEY_NUM% = 3%
		ADDTITLE$ = " BY BATCH NUMBER"

	CASE "P"
		KEY_NUM% = 4%
		ADDTITLE$ = " BY PRODUCT NUMBER"

	END SELECT

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
	TITLE$(5%) = "PO_Number  VenNum     VenNam     Line " + &
		"ProductNumber    Po_Qty  Rec_Qty  Inv_Qty" + &
		"      InvBal         Price     ExtPrice GLAcct#"

	TITLE$(6%) = "."

	%PAGE

	TOTAL, EXT = 0.0

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	TEST_PO$ = ""

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_REG_LINE.CH%, KEY #KEY_NUM%
		ELSE
			FIND #PO_REG_LINE.CH%, &
				KEY #KEY_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (PO_REG_LINE::PO > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::PO, -1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO ExitTotal IF (PO_REG_LINE::PO_TYPE > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::PO_TYPE, -1%), WLDCRD$) = 0%
		END IF

	CASE "V"
		GOTO ExitTotal IF (PO_REG_LINE::VENDOR > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0%
		END IF

	CASE "B"
		GOTO ExitTotal IF (PO_REG_LINE::BATCH > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::BATCH, -1%), WLDCRD$) = 0%
		END IF

	CASE "P"
		GOTO ExitTotal IF (PO_REG_LINE::PRODUCT > &
			TO_ITEM$) AND (TO_ITEM$ <> "")

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0%
		END IF

	END SELECT

	GOTO ExitPrint &
		IF PO_READ_REG_LINE(PO_REG_LINE::PO, &
		PO_REG_LINE::PO_LINE, "EQ", PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_READ, QTY(), CUTOFF$) <> CMC$_NORMAL

	BALANCE = FUNC_ROUND(QTY(2%) - QTY(9%), 2%)

	!
	! Check to see if we need to print the line totals
	!
	GOTO ExitPrint IF (BALANCE = 0.0) AND (QTY_EQ$ = "Y")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
		IF PO_REG_LINE::PO <> TEST_PO$ AND TEST_PO$ <> ""

	!
	! Flag unequal quantities when all of the lines
	! are being printed
	!
	QTY_EQ_FLAG$ = "  "

	QTY_EQ_FLAG$ = "??" &
		IF (QTY_EQ$ = "N") AND (BALANCE <> 0.0)

	!
	! Get the vendor name
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	IF PO_REG_LINE::PO = TEST_PO$
	THEN
		PO_REG_LINE::VENDOR = ""
		AP_VENDOR_EXAM::VENNAM = ""
	ELSE
		TEST_PO$ = PO_REG_LINE::PO
	END IF

	!
	! If the price is zero, we will read the cost
	!
 !	IF PO_REG_SUB_LINE_READ::PRICE <> 0.0
 !	THEN
 !		RATE = PO_REG_SUB_LINE_READ::PRICE
 !	ELSE
 !		RATE = PC_READ_COST(PO_REG_LINE::PRODUCT, &
 !			PO_REG_LINE::FROMLOCATION, PO_REG_LINE::ORDDATE, "")
 !	END IF

	RATE = PC_READ_COST(PO_REG_LINE::PRODUCT, &
		PO_REG_LINE::FROMLOCATION, PO_REG_LINE::ORDDATE, "")

	IF RATE = 0.0
	THEN
		RATE = PO_REG_SUB_LINE_READ::PRICE
	END IF

	EXT = FUNC_ROUND(BALANCE * RATE, 2%)

	V% = GL_OUTP_ACCTSUM(OPT_ADDREC, PO_REG_SUB_LINE_READ::ACCOUNT, &
		0.0, EXT, 0.0, TITLE$(), UTL_REPORTX)

	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + " " + &
		PO_REG_LINE::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 10%) + " " + &
		PO_REG_LINE::PO_LINE + " " + &
		PO_REG_LINE::PRODUCT + &
		FORMAT$(QTY(1%), "#,###,###") + &
		FORMAT$(QTY(2%), "#,###,###") + &
		FORMAT$(QTY(9%), "#,###,###") + &
		FORMAT$(BALANCE, "#,###,###.##") + " " + &
		QTY_EQ_FLAG$ + &
		FORMAT$(RATE, "####,###.##") + &
		FORMAT$(EXT, "##,###,###.##") + " " + &
		PO_REG_SUB_LINE_READ::ACCOUNT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + EXT
	EXT = 0.0

 ExitPrint:
	GOTO GetNextRec

 ExitTotal:
	V% = GL_OUTP_ACCTSUM(OPT_ADDREC, AP_CONTROL::AP_ACCT, &
		0.0, -TOTAL, 0.0, TITLE$(), UTL_REPORTX)

	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", &
		0.0, 0.0, 0.0, TITLE$(), UTL_REPORTX)

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
