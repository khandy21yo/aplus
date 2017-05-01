1	%TITLE "Purchase Order Aged Cash Requirements"
	%SBTTL "PO_RPRT_AGECASHREQ"
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
	! ID:PO034
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Aged Cash Rquirements\* option will
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
	!	Product Number
	!	.te
	!	Quantity Past Due
	!	.te
	!	Amount Currently Due, Base Date to Base Date + Increment
	!	.te
	!	Amount Due, Base Date + Increment * 2 to
	!	Base Date + Increment * 3
	!	Amount Due, Base Date + Increment * 3 to
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
	!	$ BAS PO_SOURCE:PO_RPRT_AGECASHREQ/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_AGECASHREQ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_AGECASHREQ.OBJ;*
	!
	! Author:
	!
	!	07/21/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	02/26/92 - Dan Perkins
	!		Report copied and modified from PO_RPRT_AGE.
	!
	!	03/03/92 - Dan Perkins
	!		Added code to pad PO Nmbers with zeros in FROM ITEM
	!		and TO ITEM fields if PO Number is selected in sort
	!		option.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/13/93 - Kevin Handy
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
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Open PO_AGE.TMP on UTL_WORK.DEV$
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	MAP (PO_AGE)	STRING  PO_AGE.VENDOR = 10%, &
			STRING  PO_AGE.PRODUCT = 14%, &
			STRING  PO_AGE.PONUM = 10%, &
			STRING	PO_AGE.POLINE =  4%, &
			STRING	PO_AGE.POTYPE =  2%, &
			REAL	PO_AGE.PASTDUE, &
			REAL	PO_AGE.CURRENT1, &
			REAL	PO_AGE.CURRENT2, &
			REAL	PO_AGE.CURRENT3, &
			REAL	PO_AGE.CURRENT4

	!
	! External functions
	!
	EXTERNAL	INTEGER	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL	INTEGER FUNCTION	PD_EXAM_PRODUCT
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
	!	Valid settings are:
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
	!	A ^*From Item\*
	!	field causes the printing of the Purchase Order
	!	Aged Cash Requirements Report to begin with a selected item.
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
	!	The value entered must be in agreement with
	!	field (01) Sort by.
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
	!	"wildcard" value.
	!	.lm -5
	!
	! Index:
	!
	!--

	INCR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) Increment\*
	!	.b
	!	.lm +5
	!	The ^*Increment\*
	!	field is used to calculate in which aged period
	!	to place the Product that is due.
	!	.table 3,25
	!	.te
	!	^*1\* - Daily
	!	.te
	!	^*7\* - Weekly
	!	.te
	!	^*30\* - Monthly
	!	.end table
	!	This field must contain a valid value.
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
	!	The ^*Base Date\* field
	!	starts the aging periods, used with increment
	!	and expected receive date.
	!	.b
	!	This field must contain a valid value.
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
		FROM_ITEM$ = SPACE$(LEN(PO_AGE.PONUM) - &
			LEN(FROM_ITEM$)) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_AGE.PONUM) - &
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

	TITLE$(1%) = "PURCHASE ORDER AGED CASH REQUIREMENTS" + &
		" AS OF EXPECTED RECEIVE DATE"

	TITLE$(2%) = "LIST SORTED BY " + ADD_TITLE$

	TITLE$(3%) = "Base Date: " + EDIT$(UTL_REPORTX::OPTDEF(6%), -1%) + &
			"  Day Incremnt: " + FORMAT$(INCR%, "###")

	TITLE$(4%) = "Purchase Order System"

	TITLE$(5%) = ""

	!
	! Heading
	!

	TITLE$(6%) = "PONumber    Line Type VendorNumb VendorName" + &
		"      ProductNumber  ProdDescription " + &
		"  PastDue   Current   " + &
		FORMAT$(INCR% + 1%, "####") + "-" + &
		FORMAT$(NUM1$(INCR% * 2%), "'LLL") + " " + &
		FORMAT$(INCR% * 2% + 1%, "####") + "-" + &
		FORMAT$(NUM1$(INCR% * 3%), "'LLL") + " " + &
		FORMAT$(INCR% * 3% + 1%, "####") + "-Over"

	TITLE$(7%) = "."

	%PAGE

10000	!***************************************************************
	! CREATE TEMPORARY FILE
	!***************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	CALL ASSG_CHANNEL(PO_AGE.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PO_AGE.TMP" FOR OUTPUT AS FILE PO_AGE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP PO_AGE, &
			PRIMARY KEY &
			( &
				PO_AGE.PONUM &
			)	DUPLICATES, &
			ALTERNATE KEY &
			( &
				PO_AGE.PRODUCT, &
				PO_AGE.PONUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				PO_AGE.VENDOR, &
				PO_AGE.PONUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				PO_AGE.POTYPE, &
				PO_AGE.PONUM &
			)	DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PO_AGE.TMP"
		CONTINUE HelpError
	END WHEN

11000	!***************************************************************
	! FILL UP TEMPORARY FILE
	!***************************************************************
	TOTAL1 = 0.0
	TOTAL2 = 0.0
	TOTAL3 = 0.0
	TOTAL4 = 0.0
	TOTALPASTDUE = 0.0

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

	EXT = FUNC_ROUND(QTY(0%) * PO_REG_SUB_LINE_READ::PRICE, 2%)

	SELECT RANGE%

	CASE 0%
		PO_AGE.CURRENT1 = PO_AGE.CURRENT1 + EXT
		TOTAL1 = TOTAL1 + EXT

	CASE 1%
		PO_AGE.CURRENT2 = PO_AGE.CURRENT2 + EXT
		TOTAL2 = TOTAL2 + EXT

	CASE 2%
		PO_AGE.CURRENT3 = PO_AGE.CURRENT3 + EXT
		TOTAL3 = TOTAL3 + EXT

	CASE <0%
		PO_AGE.PASTDUE = PO_AGE.PASTDUE + EXT
		TOTALPASTDUE = TOTALPASTDUE + EXT

	CASE ELSE
		PO_AGE.CURRENT4 = PO_AGE.CURRENT4 + EXT
		TOTAL4 = TOTAL4 + EXT

	END SELECT

	PO_AGE.VENDOR = PO_REG_LINE::VENDOR
	PO_AGE.PRODUCT = PO_REG_LINE::PRODUCT
	PO_AGE.PONUM = PO_REG_LINE::PO
	PO_AGE.POLINE = PO_REG_LINE::PO_LINE
	PO_AGE.POTYPE = PO_REG_LINE::PO_TYPE

	PUT #PO_AGE.CH%

	PO_AGE.CURRENT1 = 0.0
	PO_AGE.CURRENT2 = 0.0
	PO_AGE.CURRENT3 = 0.0
	PO_AGE.CURRENT4 = 0.0
	PO_AGE.PASTDUE  = 0.0

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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_AGE.TMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "N"
		GOTO ExitTotal IF (PO_AGE.PONUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE.PONUM, -1%), WLDCRD$) = 0%

	CASE "P"
		GOTO ExitTotal IF (PO_AGE.PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE.PRODUCT, -1%), WLDCRD$) = 0%

	CASE "V"
		GOTO ExitTotal IF (PO_AGE.VENDOR > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE.VENDOR, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (PO_AGE.POTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_AGE.POTYPE, -1%), WLDCRD$) = 0%

	END SELECT

	V% = AP_EXAM_VENDOR(PO_AGE.VENDOR, AP_VENDOR_EXAM)

	V% = PD_EXAM_PRODUCT(PO_AGE.PRODUCT, PD_PRODUCT_EXAM)

	TEXT$ = CONV_STRING(PO_AGE.PONUM, CMC$_LEFT) + "  " + &
		PO_AGE.POLINE + " " + &
		PO_AGE.POTYPE + "   " + &
		PO_AGE.VENDOR + " " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 15%) + " " + &
		PO_AGE.PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 15%) + " " + &
		FORMAT$(PO_AGE.PASTDUE,  "######.##") + " " + &
		FORMAT$(PO_AGE.CURRENT1, "######.##") + " " + &
		FORMAT$(PO_AGE.CURRENT2, "######.##") + " " + &
		FORMAT$(PO_AGE.CURRENT3, "######.##") + " " + &
		FORMAT$(PO_AGE.CURRENT4, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitTotal:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(61%) + &
		"T O T A L" + &
		SPACE$(10%) + &
		FORMAT$(TOTALPASTDUE, "######.##") + " " + &
		FORMAT$(TOTAL1, "######.##") + " " + &
		FORMAT$(TOTAL2, "######.##") + " " + &
		FORMAT$(TOTAL3, "######.##") + " " + &
		FORMAT$(TOTAL4, "######.##")

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
