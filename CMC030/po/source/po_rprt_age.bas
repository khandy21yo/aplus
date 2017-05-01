1	%TITLE "Purchase Order Aged Receipts Report"
	%SBTTL "PO_RPRT_AGE"
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
	! ID:PO031
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Receive Journal List\* option
	!	provides a report which contains the following:
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
	!	Product Number
	!	.te
	!	Quantity Past Due
	!	.te
	!	Quantity Currently Due, Base Date to Base Date + Increment
	!	.te
	!	Quantity Due, Base Date + Increment * 2 to
	!	Base Date + Increment * 3
	!	Quantity Due, Base Date + Increment * 3 to
	!	Future
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_AGE/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_AGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_AGE.OBJ;*
	!
	! Author:
	!
	!	07/21/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	07/15/91 - Kevin Handy
	!		Renumbered 17350 to 17380 to put it in proper sequence.
	!
	!	10/14/91 - Dan Perkins
	!		Cleaned program code.  Checked error trapping.
	!
	!	02/04/92 - Dan Perkins
	!		Added PO number to sort by option.  Added array to
	!		figure received quantities.  Re_wrote the program
	!		in general.
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/26/92 - Dan Perkins
	!		Changed to accomodate changes in file layouts in
	!		REG_LINE and REG_SUB_LINE.
	!
	!	03/03/92 - Dan Perkins
	!		Added code to pad PO Nmbers with zeros in FROM ITEM
	!		and TO ITEM fields if PO Number is selected in sort
	!		option.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (check)
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
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Open PO_AGE.TMP on UTL_WORK.DEV$.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	03/07/2003 - Kevin Handy
	!		Make PO_AGE a structure.
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	RECORD PO_AGE_CDD
		STRING  VENDOR = 10%, &
		STRING  PRODUCT = 14%, &
		STRING  PONUM = 10%, &
		STRING	POLINE =  4%, &
		STRING	POTYPE =  2%, &
		REAL	PASTDUE, &
		REAL	CURRENT1, &
		REAL	CURRENT2, &
		REAL	CURRENT3, &
		REAL	CURRENT4
	END RECORD

	MAP (PO_AGE) PO_AGE_CDD PO_AGE

	!
	! External functions
	!
	EXTERNAL	INTEGER	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL	INTEGER FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL	STRING  FUNCTION	CONV_STRING
	EXTERNAL	LONG	FUNCTION	PO_READ_REG_LINE

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
	!	.ts 55
	!	^*(01) Sort	N,T,V,P\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field
	!	prints the list in order by a sort key.
	!	.b
	!	The valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - PO Number
	!	.te
	!	^*T\* - PO Type
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
	!	A ^*From Item\* value
	!	causes the printing of the Purchase Order
	!	Aged Receipts Report to begin with a selected item.
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
	!	field causes the printing of the Purchase Order
	!	Aged Receipts Report to end with a selected item.
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

	ONLY_OPEN$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Only Open Balances\*
	!	.b
	!	.lm +5
	!	The ^*Only Open Balances\* field enters ^*YES\*
	!	if only open purchase order should be printed. The flag should
	!	be NO to print all orders.
	!	.lm -5
	!
	! Index:
	!	.x Only Open Balances>Register
	!
	!--

	INCR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) Increment\*
	!	.b
	!	.lm +5
	!	A ^*Increment\*
	!	field calculates in which age period
	!	to place the Product.
	!	.table 3,25
	!	.te
	!	^*1\* - Daily
	!	.te
	!	^*7\* - Weekly
	!	.te
	!	^*30\* - Monthly
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	BASE_DATE% = DATE_DAYCODE(DATE_STOREDATE(EDIT$( &
		UTL_REPORTX::OPTDEF(6%), 132%)))

	!++
	! Abstract:FLD07
	!	^*(07) Base Date\*
	!	.b
	!	.lm +5
	!	A ^*Base Date\* field
	!	causes the account aging beginning with a particular date.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	An entry is required in this field.
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
	SELECT SORT_BY$
	CASE "N"
		ADD_TITLE$ = "PO NUMBER"
		K_NUM% = 0%

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(PO_AGE::PONUM) - &
			LEN(FROM_ITEM$)) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_AGE::PONUM) - &
			LEN(TO_ITEM$)) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "P"
		ADD_TITLE$ = "PRODUCT"
		K_NUM% = 1%

	CASE "V"
		ADD_TITLE$ = "VENDOR"
		K_NUM% = 2%

	CASE "T"
		ADD_TITLE$ = "PO TYPE"
		K_NUM% = 3%
	END SELECT

	TITLE$(1%) = "PURCHASE ORDER AGED RECEIPTS AS OF EXPECTED RECEIVE DATE"

	TITLE$(2%) = "LIST SORTED BY " + ADD_TITLE$

	TITLE$(3%) = "Base Date: " + EDIT$(UTL_REPORTX::OPTDEF(6%), -1%) + &
			"  Day Incremnt: " + FORMAT$(INCR%, "###")

	TITLE$(4%) = "Purchase Order System"

	TITLE$(5%) = ""

	!
	! Heading
	!

	TITLE$(6%) = "PONumber    Line Type VendorNumb VendorName" + &
		"           ProductNumber  ProdDescription  " + &
		"    PastDue Current   " + &
		FORMAT$(INCR% + 1%, "###") + "-" + &
		FORMAT$(NUM1$(INCR% * 2%), "'LL") + " " + &
		FORMAT$(INCR% * 2% + 1%, "###") + "-" + &
		FORMAT$(NUM1$(INCR% * 3%), "'LL") + "" + &
		FORMAT$(INCR% * 3% + 1%, "###") + "-Over"

	TITLE$(7%) = "."

	%PAGE

10000	!***************************************************************
	! CREATE TEMPORARY FILE
	!***************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	CALL ASSG_CHANNEL(PO_AGE.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PO_AGE.TMP" &
			FOR OUTPUT AS FILE PO_AGE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP PO_AGE, &
			PRIMARY KEY &
			( &
				PO_AGE::PONUM &
			)	DUPLICATES, &
			ALTERNATE KEY &
			( &
				PO_AGE::PRODUCT, &
				PO_AGE::PONUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				PO_AGE::VENDOR, &
				PO_AGE::PONUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				PO_AGE::POTYPE, &
				PO_AGE::PONUM &
			)	DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW NONE
		USE
			FILENAME$ = "PO_AGE.TMP"
			CONTINUE HelpError
		END WHEN

11000	!***************************************************************
	! FILL UP TEMPORARY FILE
	!***************************************************************

	!
	! Reset the file we gather the information from
	!
	RESET #PO_REG_LINE.CH%

 ReadRegLine:
11020	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE ProgOutput IF ERR = 11% OR ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	IF ONLY_OPEN$ = "Y" AND PO_REG_LINE::OPEN_CLOSE = "C"
	THEN
		GOTO 11020
	END IF

	V% = PO_READ_REG_LINE(PO_REG_LINE::PO, PO_REG_LINE::PO_LINE, &
		"EQ", PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
		QTY(), "")

	!
	! See if there is a balance
	!
	GOTO ReadRegLine IF QTY(0%) = 0.0

	!
	! Find the date range it fits in
	!
	RANGE% = (DATE_DAYCODE(PO_REG_SUB_LINE_READ::ACTION_DATE) - &
		BASE_DATE%) / INCR%

	SELECT RANGE%

	CASE 0%
		PO_AGE::CURRENT1 = PO_AGE::CURRENT1 + QTY(0%)

	CASE 1%
		PO_AGE::CURRENT2 = PO_AGE::CURRENT2 + QTY(0%)

	CASE 2%
		PO_AGE::CURRENT3 = PO_AGE::CURRENT3 + QTY(0%)

	CASE <0%
		PO_AGE::PASTDUE = PO_AGE::PASTDUE + QTY(0%)

	CASE ELSE
		PO_AGE::CURRENT4 = PO_AGE::CURRENT4 + QTY(0%)

	END SELECT

	PO_AGE::VENDOR = PO_REG_LINE::VENDOR
	PO_AGE::PRODUCT = PO_REG_LINE::PRODUCT
	PO_AGE::PONUM = PO_REG_LINE::PO
	PO_AGE::POLINE = PO_REG_LINE::PO_LINE
	PO_AGE::POTYPE = PO_REG_LINE::PO_TYPE

	PUT #PO_AGE.CH%

	PO_AGE::CURRENT1 = 0.0
	PO_AGE::CURRENT2 = 0.0
	PO_AGE::CURRENT3 = 0.0
	PO_AGE::CURRENT4 = 0.0
	PO_AGE::PASTDUE  = 0.0

	GOTO ReadRegLine

 ProgOutput:
17000	!***************************************************************
	! OUTPUT FILE
	!***************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_AGE.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_AGE.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_AGE.TMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PO_AGE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_AGE.TMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "N"
		GOTO ExitProgram IF (PO_AGE::PONUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE::PONUM, -1%), WLDCRD$) = 0%

	CASE "P"
		GOTO ExitProgram IF (PO_AGE::PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE::PRODUCT, -1%), WLDCRD$) = 0%

	CASE "V"
		GOTO ExitProgram IF (PO_AGE::VENDOR > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE::VENDOR, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PO_AGE::POTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE::POTYPE, -1%), WLDCRD$) = 0%

	END SELECT

	V% = AP_EXAM_VENDOR(PO_AGE::VENDOR, AP_VENDOR_EXAM)

	V% = PD_EXAM_PRODUCT(PO_AGE::PRODUCT, PD_PRODUCT_EXAM)

17370	TEXT$ = CONV_STRING(PO_AGE::PONUM, CMC$_LEFT) + "  " + &
		PO_AGE::POLINE + " " + &
		PO_AGE::POTYPE + "   " + &
		PO_AGE::VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 20%) + " " + &
		PO_AGE::PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(PO_AGE::PASTDUE,  "###,###") + " " + &
		FORMAT$(PO_AGE::CURRENT1, "###,###") + " " + &
		FORMAT$(PO_AGE::CURRENT2, "###,###") + " " + &
		FORMAT$(PO_AGE::CURRENT3, "###,###") + " " + &
		FORMAT$(PO_AGE::CURRENT4, "###,###")

	CALL OUTP_LINE(, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
