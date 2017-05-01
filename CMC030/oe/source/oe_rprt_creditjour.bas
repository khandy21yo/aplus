1	%TITLE "Print Credit Journal"
	%SBTTL "OE_RPRT_CREDITJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:OE025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Credit Journal\* option prints a Credit Memo
	!	Journal which will contain the following information:
	!	.table 3,25
	!	.te
	!	Memo Number	Memo Date
	!	.te
	!	Reason Code	Sales Type
	!	.te
	!	Customer Number	Customer Name
	!	.te
	!	Operator	Discount
	!	.te
	!	Sales Tax	Handling Charges
	!	.te
	!	Freight Charges	Misc. Charges
	!	.te
	!	Misc. Account	Notes
	!	.te
	!	Product Number	Product Description
	!	.te
	!	Cost	Credit Quantity
	!	.te
	!	Unit Price	Promotion
	!	.te
	!	Discount Percentage	Extended Price
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Credit Memo Journal
	!	.x Print>Credit Memo Journal
	!	.x Credit Memo Journal>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_CREDITJOUR/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_CREDITJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_CREDITJOUR.OBJ;*
	!
	! AUTHOR:
	!	08/30/91 - JEFF BEARD
	!
	! MODIFICATION HISTORY:
	!
	!	09/04/91 - Deborah K. Fries
	!		Rewrote program in order for it to
	!		SortBy$ T,C,N,O
	!		Grand Totals for Discount, Salestax, Handling,
	!		Freight, Misc, and ExtPrice
	!
	!	09/25/91 - Deborah K. Fries
	!		Used functions to read files
	!		Cleaned source code
	!		Improved error trapping
	!
	!	10/16/91 - Dan Perkins
	!		Modified program to accomodate new
	!		file layout in Order Journal.
	!		Program now sorts on Memo Number,
	!		Customer Name, or Memo Reason.
	!
	!	11/22/91 - Dan Perkins
	!		Change FUNC_ROUND to two decimal places
	!		for money.
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to lset MEMO NUMBER.
	!
	!	04/16/92 - Frank F. Starman
	!		Replace OE_CREASON file with OE_REASONACCT.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	06/06/96 - Kevin Handy
	!		Lose commented out code.
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter to OE_READ_SALESTAX
	!
	!	10/16/98 - Kevin Handy
	!		Fix sales tax calculation bug (ar instead of oe
	!		flag used)
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.HB"
	MAP (OE_CREDITLINE)	OE_CREDITLINE_CDD	OE_CREDITLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	DECLARE			OE_SALESTAX_CDD		OE_SALESTAX_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	DECLARE			OE_ACCOUNT_CDD		OE_ACCOUNT_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	DECLARE			OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	DECLARE			OE_REASONACCT_CDD	OE_REASONACCT_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION OE_READ_ACCOUNT
	EXTERNAL LONG    FUNCTION OE_READ_SALESTAX
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG    FUNCTION OE_READ_PROMO
	EXTERNAL LONG    FUNCTION OE_READ_REASONACCT
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Batch Number
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number
	!	of a selected batch which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	.x Sort by
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*M\* - Memo Number
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*R\* - Memo Reason
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the item with
	!	which the report will begin printing.  The value entered
	!	must be in agreement with field (02) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with which the
	!	report will end printing.  The value entered must be in
	!	agreement with field (02) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding
	!	Technique.
	!	.b
	!	For information on "Wildcarding" techniques, refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Credit Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.OPN"
	USE
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Credit Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "M"
		K_NUM% = 0%
		TITLE$(1%) = " CREDIT JOURNAL " + BATCH_NO$ + &
			" BY MEMO NUMBER"

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any memo numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(OE_CREDITJOUR::MEMONUM) - &
			LEN(FROM_ITEM$)) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(OE_CREDITJOUR::MEMONUM) - &
			LEN(TO_ITEM$)) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "N"
		K_NUM% = 1%
		TITLE$(1%) = " CREDIT JOURNAL " + BATCH_NO$ + &
			" BY CUSTOMER NUMBER"

	CASE "R"
		K_NUM% = 2%
		TITLE$(1%) = " CREDIT JOURNAL " + BATCH_NO$ + &
			" BY MEMO REASON"

	END SELECT

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!	'              1         2         3         4         5
	!	'     1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
	TITLE$(4%) = "Memo#     MemoDate     OrderDate    RC  SalType  CusNumber   " + &
		"CustomerName                         Operator     " + &
		"Salesman"

	TITLE$(5%) = "           Product#       Description          " + &
		"         Cost     CrdQty      InvQty     UnitPrice" + &
		"        Promo  Disc%       ExtPrice"

	TITLE$(6%) = "                Discount      SalesTax      Handling" + &
		"       Freight      Misc Chg  MiscAcct" + SPACE$(30%) + &
		"Credit Total"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	PRINT_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_CREDITJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_CREDITJOUR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	IF PRINT_FLAG%
	THEN
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + "    " + &
			FORMAT$(OE_CREDITJOUR::DISC, "#,###,###.##") + "  " + &
			FORMAT$(OE_CREDITJOUR::SALESTAX, "#,###,###.##") + "  " + &
			FORMAT$(OE_CREDITJOUR::HANDLING, "#,###,###.##") + "  " + &
			FORMAT$(OE_CREDITJOUR::FREIGHT, "#,###,###.##") + "  " + &
			FORMAT$(OE_CREDITJOUR::MISC, "#,###,###.##") + "  " + &
			OE_CREDITJOUR::MISCACCT + SPACE$(19%) + &
			FORMAT$(ORDER_TOT, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitTotal IF UTL_REPORTX::STAT
		PRINT_FLAG% = 0%
	END IF

17020	WHEN ERROR IN
		GET #OE_CREDITJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "M"
		GOTO ExitTotal IF (OE_CREDITJOUR::MEMONUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_CREDITJOUR::MEMONUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitTotal IF (OE_CREDITJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_CREDITJOUR::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "R"
		GOTO ExitTotal IF (OE_CREDITJOUR::REASON > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			OE_CREDITJOUR::REASON, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Read customer name
	!
	V% = AR_EXAM_CUSTOM(OE_CREDITJOUR::CUSNUM, AR_35CUSTOM_EXAM)

	V% = OE_READ_ACCOUNT(AR_35CUSTOM_EXAM::TTYPE, OE_CREDITJOUR::ORDTYPE, &
		OE_CREDITJOUR::LOCATION, OE_ACCOUNT_READ)

	V% = OE_READ_REASONACCT(OE_CREDITJOUR::REASON, &
		OE_CREDITJOUR::LOCATION, OE_REASONACCT_READ)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + "  " + &
		PRNT_DATE(OE_CREDITJOUR::MEMODATE, 8%) + "   " + &
		PRNT_DATE(OE_CREDITJOUR::ORDDATE, 8%) + "   " + &
		OE_CREDITJOUR::REASON + "  " + &
		OE_CREDITJOUR::ORDTYPE + "       " + &
		OE_CREDITJOUR::CUSNUM + "  " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 35%) + "  "      + &
		OE_CREDITJOUR::OPERATOR + "   " + &
		OE_CREDITJOUR::SALESMAN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT
	PRINT_FLAG% = -1%


 Handling:
	!
	! Check Credit Journal Handling
	!
	IF OE_CREDITJOUR::HANDLING <> 0.0
	THEN
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
			OE_ACCOUNT_READ::HANDLING, &
			0.0, OE_CREDITJOUR::HANDLING, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Check Credit Journal Discount
	!
	IF OE_CREDITJOUR::DISC <> 0.0
	THEN
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
			OE_ACCOUNT_READ::DISACCT, &
			0.0, -OE_CREDITJOUR::DISC, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Check Credit Journal Miscellaneous
	!
	IF OE_CREDITJOUR::MISC <> 0.0
	THEN
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
			OE_CREDITJOUR::MISCACCT, &
			0.0, OE_CREDITJOUR::MISC, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Check Credit Journal Freight
	!
	IF OE_CREDITJOUR::FREIGHT <> 0.0
	THEN
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
			OE_ACCOUNT_READ::FRACCT, &
			0.0, OE_CREDITJOUR::FREIGHT, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Check Credit Journal Sales Tax
	!
	IF OE_CREDITJOUR::SALESTAX <> 0.0
	THEN
		V% = OE_READ_SALESTAX(AR_35CUSTOM_EXAM::TAXCODE, &
			"1", &
			OE_SALESTAX_READ)

		TOTAL = OE_SALESTAX_READ::STATETAX + &
			OE_SALESTAX_READ::CITYTAX + OE_SALESTAX_READ::COUNTYTAX

		IF TOTAL <> 0.0
		THEN
			STATETAX = FUNC_ROUND( &
				OE_CREDITJOUR::SALESTAX * &
				(OE_SALESTAX_READ::STATETAX / TOTAL), 2%)
			CITYTAX = FUNC_ROUND( &
				OE_CREDITJOUR::SALESTAX * &
				(OE_SALESTAX_READ::CITYTAX / TOTAL), 2%)
		ELSE
			STATETAX = 0.0
			CITYTAX = 0.0
		END IF

		IF STATETAX <> 0.0
		THEN
			GOTO ExitProgram &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				OE_SALESTAX_READ::STATEACC, &
				0.0, STATETAX, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		END IF

		IF CITYTAX <> 0.0
		THEN
			IF OE_SALESTAX_READ::COUNTYTAX <> 0.0
			THEN
				ACCT_AMT = -CITYTAX
			ELSE
				ACCT_AMT = -(OE_CREDITJOUR::SALESTAX - STATETAX)
			END IF

			GOTO ExitProgram &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				OE_SALESTAX_READ::CITYACC, &
				0.0, -ACCT_AMT, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		END IF

		IF OE_SALESTAX_READ::COUNTYTAX <> 0.0
		THEN
			ACCT_AMT  = -(OE_CREDITJOUR::SALESTAX - &
				STATETAX - CITYTAX)

			GOTO ExitProgram &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				OE_SALESTAX_READ::COUNTYACC, &
				0.0, -ACCT_AMT, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		END IF
	END IF

 GrandTotals:
	!
	! Finds Grand Totals for end of report
	!
	DISC_GT = DISC_GT + OE_CREDITJOUR::DISC
	SALESTAX_GT = SALESTAX_GT + OE_CREDITJOUR::SALESTAX
	HANDLING_GT = HANDLING_GT + OE_CREDITJOUR::HANDLING
	FREIGHT_GT = FREIGHT_GT + OE_CREDITJOUR::FREIGHT
	MISC_GT = MISC_GT + OE_CREDITJOUR::MISC

	!
	! Find OE_CREDITLINE record
	!
	LTOTAL = 0.0

17300	WHEN ERROR IN
		FIND #OE_CREDITLINE.CH%, &
			KEY #0% EQ OE_CREDITJOUR::MEMONUM, &
			REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

 Creditline:
	!
	! Get OE_CREDITLINE record
	!
	WHEN ERROR IN
		GET #OE_CREDITLINE.CH%, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

	GOTO NewOrder IF OE_CREDITLINE::MEMONUM <> OE_CREDITJOUR::MEMONUM

	EXTPRICE = FUNC_ROUND(OE_CREDITLINE::CREDQTY * OE_CREDITLINE::PRICE, 2%)
	PROMO = FUNC_ROUND(OE_CREDITLINE::CREDQTY * OE_CREDITLINE::PROMO, 2%)
	DISCOUNT = FUNC_ROUND(OE_CREDITLINE::CREDQTY * &
		(OE_CREDITLINE::PRICE - OE_CREDITLINE::PROMO) &
		* OE_CREDITLINE::DISCOUNT / 100, 2%)

	LTOTAL = LTOTAL + EXTPRICE - DISCOUNT - PROMO

	!
	! Read the Product
	!
	V% = PD_EXAM_PRODUCT(OE_CREDITLINE::PRODUCT, PD_PRODUCT_EXAM)

	V% = PD_READ_ACCOUNT(OE_CREDITJOUR::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, &
		PD_ACCOUNT_READ)

	COST = FUNC_ROUND(OE_CREDITLINE::COST * OE_CREDITLINE::INVQTY, 2%)

	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::INVACCT, &
		0.0, COST, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::COSACCT, &
		0.0, -COST, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Check the Discount
	!
	IF DISCOUNT <> 0.0
	THEN
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
			PD_ACCOUNT_READ::DISCACCT, &
			0.0, -DISCOUNT, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Check Product Promo
	!
	IF PROMO <> 0.0
	THEN
		V% = OE_READ_PROMO(OE_CREDITLINE::PRODUCT, &
			OE_CREDITJOUR::MEMODATE, &
			OE_CREDITJOUR::CUSNUM, OE_PROMO_READ, 0.0, 0.0)

		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM(OPT_ADDREC, OE_PROMO_READ::ACCOUNT, &
			0.0, -PROMO, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Print one Journal line
	!
	! FEXTPRICE is the final extprice after all calculations have been
	! completed
	!
	FEXTPRICE = EXTPRICE - DISCOUNT - PROMO
	FEXTPRICE_GT = FEXTPRICE_GT + FEXTPRICE
	FINALCR_TOT = DISC_GT + SALESTAX_GT + HANDLING_GT + &
		FREIGHT_GT + MISC_GT + FEXTPRICE_GT

	TEXT$ = CONV_STRING(OE_CREDITLINE::MEMONUM, CMC$_LEFT) + "   " + &
		OE_CREDITLINE::PRODUCT + " "   + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(OE_CREDITLINE::COST, "#,###,###.###") + "  " + &
		FORMAT$(OE_CREDITLINE::CREDQTY, "#,###,###") + "   " + &
		FORMAT$(OE_CREDITLINE::INVQTY, "#,###,###") + " " + &
		FORMAT$(OE_CREDITLINE::PRICE, "#,###,###.###") + " " + &
		FORMAT$(OE_CREDITLINE::PROMO, "#,###,###.##") + " " + &
		FORMAT$(OE_CREDITLINE::DISCOUNT, "###.##") + "  " + &
		FORMAT$(FEXTPRICE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, OE_REASONACCT_READ::ACCOUNT, &
		0.0, EXTPRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO Creditline

 NewOrder:
	ORDER_TOT = OE_CREDITJOUR::FREIGHT + OE_CREDITJOUR::MISC + &
		OE_CREDITJOUR::SALESTAX +OE_CREDITJOUR::HANDLING + &
		LTOTAL - OE_CREDITJOUR::DISC

	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, OE_ACCOUNT_READ::ACCOUNT, &
		0.0, -ORDER_TOT, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Try for next Order Register record
	!
	GOTO GetNextRec

 ExitTotal:

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Print out the Totals
	!
	TEXT$ = "Final Credit Discount   " + FORMAT$(DISC_GT, "#,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Final Credit Salestax   " + &
		FORMAT$(SALESTAX_GT, "#,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Final Credit Handling   " + &
		FORMAT$(HANDLING_GT, "#,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Final Credit Freight    " + FORMAT$(FREIGHT_GT, "#,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Final Credit Misc       " + FORMAT$(MISC_GT, "#,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Final Credit ExtPrice " + &
		FORMAT$(FEXTPRICE_GT, "###,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Final Credit Total    " + &
		FORMAT$(FINALCR_TOT, "###,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", &
		0.0, 0.0, 0.0, TITLE$(), UTL_REPORTX)

	%PAGE

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
