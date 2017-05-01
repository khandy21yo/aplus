1	%TITLE "Print Pacific Pride Invoice Form"
	%SBTTL "PP_FORM_INVOICE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:PPFROM
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Invoice\* option
	!	prints invoices at the end of a period.
	!	The format is user defined.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_FORM_INVOICE/LINE
	!	$ LINK/EXECUTABLE=PP_EXE:*.EXE PP_FORM_INVOICE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_FORM_INVOICE.OBJ;*
	!
	! Author:
	!
	!	01/13/93 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/04/93 - Kevin Handy
	!		Changed PP_MONTHLY::DISCOUNT to s string.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/17/93 - Kevin Handy
	!		Fix search on 17000 to use CUSNUM+CARD on key 0
	!		instead of CARD+CUSNUM on key 1.
	!
	!	05/20/93 - Kevin Handy
	!		Modified to look at ::FTYPE instead of ::SLTYPE
	!		to determine if the transaction is fuel or not.
	!
	!	06/14/93 - Kevin Handy
	!		Modified calculation of fuel taxes to be based
	!		on quanity and not selling price.
	!
	!	06/18/93 - Kevin Handy
	!		Modified to put address in place of site name
	!		if no site name.
	!
	!	06/28/93 - Kevin Handy
	!		More modifications to try to fix taxes
	!		calculation.
	!
	!	07/08/93 - Kevin Handy
	!		Fixed fleet totals calculation of taxes so it
	!		doesn't divide by 100.
	!
	!	08/11/93 - Kevin Handy
	!		Modified to summarize products by state.
	!
	!	08/19/93 - Kevin Handy
	!		Added several line numbers to aid debugging
	!		an error.
	!
	!	08/19/93 - Kevin Handy
	!		Increased dimension for FORM_GROUP.
	!
	!	08/19/93 - Kevin Handy
	!		Changed DISLOOP to DISLOOP%
	!
	!	08/19/93 - Kevin Handy
	!		Continue to summarize by state (see 08/11/93)
	!
	!	08/20/93 - Kevin Handy
	!		Modified so discount lookup will stop if it sees
	!		an over of zero.
	!
	!	08/23/93 - Kevin Handy
	!		Modified to check exemption status of cards for
	!		card totals and product totals.
	!
	!	08/31/93 - Kevin Handy
	!		Modified to alose rotate PD_TAXABLE array when
	!		sorting PP_MONTHLY_PD array.
	!
	!	08/31/93 - Kevin Handy
	!		Changed PD_TABLE to PD_TAXABLE in total calculation.
	!
	!	08/31/93 - Kevin Handy
	!		Removed junk code. Killtemp, tempfile$, utl_report,
	!		jj$, ...
	!
	!	10/27/93 - Kevin Handy
	!		Zero out totals before starting to print driver
	!		totals.
	!
	!	10/27/93 - Kevin Handy
	!		Modified so that if card # is not found in PP_CARD,
	!		it will set the card number to the one tried for,
	!		instead of blanks.
	!
	!	11/05/93 - Kevin Handy
	!		Modified to check for zero's before division.
	!
	!	12/28/93 - Kevin Handy
	!		Modified to initialize INVOICE$ as soon as possible
	!		to avoid filling PP_CONTROL with a null value.
	!
	!	12/28/93 - Kevin Handy
	!		Check for null INVOICE$ value before updating the
	!		control file.
	!
	!	04/01/94 - Kevin Handy
	!		Don't print an invoice if customer is flagged
	!		for no statements. (FJ request)
	!
	!	10/06/94 - Kevin Handy
	!		Modified MISKEYB value to trim off leading zeroes.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile statement.
	!		Lose unecessary external definitions.
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, which is
	!		never created.
	!
	!	08/06/99 - Kevin Handy
	!		Lose lines 17100, 17500 (never called)
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/09/2005 - Kevin Handy
	!		Fixed re-use of I% problem in NewPage.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP (PP_MONTHLY)	PP_MONTHLY_CDD		PP_MONTHLY
	DIM			PP_MONTHLY_CDD		PP_MONTHLY_PD(100%)
	DIM			PP_MONTHLY_CDD		PP_MONTHLY_DR(100%)
	DIM PD_TAXABLE(100%, 4%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP (PP_CONTROL)	PP_CONTROL_CDD		PP_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.HB"
	MAP (PP_DISCOUNT)	PP_DISCOUNT_CDD		PP_DISCOUNT

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(20%)

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(6%) = 50%, &
		INVDATE$ = 8%, &
		DISDATE$ = 8%, &
		LINE_TOTAL, &
		CARD_TOTAL, &
		CARD_MILES, &
		CARD_MPG, &
		CARD_FUELQTY, &
		CARD_NONFUELQTY, &
		CARD_FUELSALES, &
		CARD_NONFUELSALES, &
		CUST_FUELQTY, &
		CUST_NONFUELQTY, &
		CUST_FUELSALES, &
		CUST_NONFUELSALES, &
		FED_TAX, &
		STA_TAX, &
		COU_TAX, &
		CTY_TAX, &
		SAL_TAX, &
		BASE_PRICE, &
		TOT_FED_TAX, &
		TOT_STA_TAX, &
		TOT_COU_TAX, &
		TOT_CTY_TAX, &
		TOT_SAL_TAX, &
		TOT_BASE_PRICE, &
		DIS_AMT, &
		PAGE_NUMBER%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PPFORM"

	!
	! Look up device
	!
	CALL READ_DEVICE("PP_FORM", PP_FORM.DEV$, STAT%)

	!***************************************************************
	! Open Report files
	!***************************************************************

	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM

	!
	! Get user input
	!
	YYYY_PP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Monthly Transaction Period\*
	!	.b
	!	.lm +5
	!	The ^*Monthly Transaction Period\* field allows for entry of a selected
	!	period for which invoices will be printed.  Each Monthly Transaction
	!	Ledger is assigned a period number consisting of six (6) alpha characters
	!	in YYYYPP format.
	!	.b
	!	Only one period at a time may be printed.
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
	!	.ts 55
	!	^*(02) Sort by	N,T,C,A\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines if the
	!	report is to be printed by customer number, customer
	!	type, category, alpha sort or salesman order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alpha Sort
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field will cause the form to start with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with a particular
	!	item in the file. The value must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field causes the form to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	INVDATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.TS 55
	!	^*(06) Invoice Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Date\* is the desired invoice date which will
	!	appear on each invoice.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* option enters the form number to be used for
	!	printing.  (See Forms Controlling under Utility Section.)
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	CASE "A"
		K_NUM% = 3%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open CUSTOMER file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

610	!
	! Open MONTHLY transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.OPN"
	USE
		FILENAME$ = "PP_MONTHLY_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

620	!
	! Open CARD file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.OPN"
	USE
		CONTINUE 630 IF ERR = 5%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

630	!
	! Open CARDEXEMPT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"
	USE
		CONTINUE 640 IF ERR = 5%
		FILENAME$ = "PP_CARDEXEMPT"
		CONTINUE HelpError
	END WHEN

640	!
	! Open DISCOUNT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.OPN"

		!
		! Right now we are just going to read the record
		! becasue I don't know what to do with it.
		!
		GET #PP_DISCOUNT.CH%, KEY #0% EQ "A", REGARDLESS
	USE
		CONTINUE 650 IF ERR = 5% OR ERR = 9%
		FILENAME$ = "PP_DISCOUNT"
		CONTINUE HelpError
	END WHEN

650	!
	! Open SITE file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.OPN"
	USE
		CONTINUE 660 IF ERR = 5%
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

660	!
	! Open SITE_PRIDUCT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.OPN"
	USE
		CONTINUE 670 IF ERR = 5%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

670	!
	! Open PP transaction type file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.OPN"
	USE
		CONTINUE 680 IF ERR = 5%
		FILENAME$ = "PP_TANTYPE"
		CONTINUE HelpError
	END WHEN

680	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.MOD"

		GET #PP_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "PP_CONTROL"
		CONTINUE HelpError
	END WHEN

	INVOICE$ = PP_CONTROL::LAST_INV

 EndOpen:
	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! Load In The Form
	!
	GOSUB LoadForm

	!
	! Go check out aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Skip if flagged for no invoices
	!
	GOTO GetNextRec IF AR_35CUSTOM::STMTFLG = "N"

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AR_35CUSTOM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_35CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AR_35CUSTOM.ADDLINE$(I%) = EDIT$(EDIT$(AR_35CUSTOM::CITY, 128%) + &
		", " + AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP + " " + &
		AR_35CUSTOM::COUNTRY, 8% + 16% + 32% + 128%)

	!
	! Blank the address lines
	!
	AR_35CUSTOM.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 6%

	!
	! The next thing we need to do is get a transaction sorted in
	! driver card order.
	!
2100	WHEN ERROR IN
		FIND #PP_MONTHLY.CH%, KEY #1% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	!
	! Initialize variables
	!
	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	TEST_CARD$        = ""
	CARD_MILES        = 0.0
	CARD_MPG          = 0.0
	CARD_FUELQTY      = 0.0
	CARD_NONFUELQTY   = 0.0
	CARD_FUELSALES    = 0.0
	CARD_NONFUELSALES = 0.0
	CUST_FUELQTY      = 0.0
	CUST_NONFUELQTY   = 0.0
	CUST_FUELSALES    = 0.0
	CUST_NONFUELSALES = 0.0

	DRLOOP% = 0%
	PDLOOP% = 0%

	!
	! Increment the invoice number
	!
	V% = FUNC_INCREMENT(PP_CONTROL::LAST_INV)

	INVOICE$ = PP_CONTROL::LAST_INV

	!
	! We found a record so we can print the top of the form
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

 GetTranRec:
2110	WHEN ERROR IN
		GET #PP_MONTHLY.CH%, REGARDLESS
	USE
		CONTINUE ExitTran1 IF ERR = 11%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTran1 IF PP_MONTHLY::CUSNUM <> AR_35CUSTOM::CUSNUM

	IF PP_MONTHLY::DRIVER = "" AND PP_MONTHLY::VEHICLE <> ""
	THEN
		PP_MONTHLY::DRIVER = PP_MONTHLY::VEHICLE
	END IF

	!
	! Get Product information
	!
	V% = PD_EXAM_PRODUCT(PP_MONTHLY::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Get Site information
	!
	GOSUB GetSite

	!
	! Hack to make sure that there is a state code on the site record
	!
	IF EDIT$(PP_MONTHLY::STATE, -1%) = ""
	THEN
		PP_MONTHLY::STATE = PP_SITE::STATE
	END IF

	!
	! Since we are on the same Customer, we can create a Product array
	!
	LINE_TOTAL = FUNC_ROUND(PP_MONTHLY::QUANTITY * PP_MONTHLY::SELLPRICE, 3%)

	FOR I% = 1% TO PDLOOP%

		IF (PP_MONTHLY::PRODUCT = PP_MONTHLY_PD(I%)::PRODUCT) AND &
			(PP_MONTHLY::STATE = PP_MONTHLY_PD(I%)::STATE)
		THEN
			PP_MONTHLY_PD(I%)::QUANTITY = &
				PP_MONTHLY_PD(I%)::QUANTITY + &
				PP_MONTHLY::QUANTITY

			!
			! Add price times quantity and store in SELLPRICE
			! since prices can be different on each transaction
			!
			PP_MONTHLY_PD(I%)::SELLPRICE = &
				PP_MONTHLY_PD(I%)::SELLPRICE + &
				LINE_TOTAL

			GOTO ExitPDLoop
		END IF

	NEXT I%

	PDLOOP% = PDLOOP% + 1%
	I% = PDLOOP%
	PP_MONTHLY_PD(PDLOOP%) = PP_MONTHLY
	PP_MONTHLY_PD(PDLOOP%)::SELLPRICE = LINE_TOTAL
	PD_TAXABLE(I%, J%) = 0.0 FOR J% = 0% TO 4%

 ExitPDLoop:
	IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
		PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "F") = 0%
	THEN
		PD_TAXABLE(I%, 0%) = PD_TAXABLE(I%, 0%) + PP_MONTHLY::QUANTITY
	END IF

	IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
		PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "S") = 0%
	THEN
		PD_TAXABLE(I%, 1%) = PD_TAXABLE(I%, 1%) + PP_MONTHLY::QUANTITY
	END IF

	IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
		PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "D") = 0%
	THEN
		PD_TAXABLE(I%, 2%) = PD_TAXABLE(I%, 2%) + PP_MONTHLY::QUANTITY
	END IF

	IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
		PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "C") = 0%
	THEN
		PD_TAXABLE(I%, 3%) = PD_TAXABLE(I%, 3%) + PP_MONTHLY::QUANTITY
	END IF

	IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
		PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "A") = 0%
	THEN
		PD_TAXABLE(I%, 4%) = PD_TAXABLE(I%, 4%) + PP_MONTHLY::QUANTITY
	END IF

	GOSUB CardTotal IF PP_MONTHLY::DRIVER <> TEST_CARD$ AND TEST_CARD$ <> ""

	IF TEST_CARD$ = ""
	THEN
		!
		! Find a card for this transaction
		!
		KEY$ = PP_MONTHLY::DRIVER
		GOSUB GetCard

		!
		! Print the CARD title
		!
		GOSUB NewPage &
			IF BODY_COUNT% >= FORM_GROUP(FRM_CARDBODY%)::NUMBER

		!
		! Print a line
		!
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CARDTITLE%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	END IF

	TEST_CARD$ = PP_MONTHLY::DRIVER

	!
	! Load Driver array for later use
	!
	LINE_TOTAL = FUNC_ROUND(PP_MONTHLY::QUANTITY * &
		PP_MONTHLY::SELLPRICE, 3%)

	FOR I% = 1% TO DRLOOP%

		IF PP_MONTHLY::DRIVER = PP_MONTHLY_DR(I%)::DRIVER AND &
			PP_MONTHLY::PRODUCT = PP_MONTHLY_DR(I%)::PRODUCT AND &
			(PP_MONTHLY::STATE = PP_MONTHLY_DR(I%)::STATE)
		THEN
			PP_MONTHLY_DR(I%)::QUANTITY = &
				PP_MONTHLY_DR(I%)::QUANTITY + &
				PP_MONTHLY::QUANTITY

			!
			! Add price times quantity and store in SELLPRICE
			! since prices can be different on each transaction
			!
			PP_MONTHLY_DR(I%)::SELLPRICE = &
				PP_MONTHLY_DR(I%)::SELLPRICE + &
				LINE_TOTAL

			GOTO ExitDRLoop
		END IF

	NEXT I%

	DRLOOP% = DRLOOP% + 1%
	PP_MONTHLY_DR(DRLOOP%) = PP_MONTHLY
	PP_MONTHLY_DR(DRLOOP%)::SELLPRICE = LINE_TOTAL

 ExitDRLoop:
	IF (PP_CARD::ODOMETER = 0.0)
	THEN
		CARD_MILES = 0.0
	ELSE
		CARD_MILES = PP_MONTHLY::ODOM - PP_CARD::ODOMETER
	END IF

	LINE_TOTAL = FUNC_ROUND(PP_MONTHLY::QUANTITY * &
		PP_MONTHLY::SELLPRICE, 2%)

	SELECT PP_MONTHLY::FTYPE

	CASE "F", '0'C
		CARD_FUELQTY = CARD_FUELQTY + PP_MONTHLY::QUANTITY
		CARD_FUELSALES = CARD_FUELSALES + LINE_TOTAL

		IF (CARD_FUELQTY <> 0.0)
		THEN
			CARD_MPG = CARD_MILES / CARD_FUELQTY
		ELSE
			CARD_MPG = 0.0
		END IF

	CASE ELSE
		CARD_NONFUELQTY = CARD_NONFUELQTY + PP_MONTHLY::QUANTITY
		CARD_NONFUELSALES = CARD_NONFUELSALES + LINE_TOTAL

	END SELECT

	!
	! We can now print a line
	!
	GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_CARDBODY%)::NUMBER

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_CARDBODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO GetTranRec

 ExitTran1:
	GOSUB CardTotal
	GOSUB CustTotal

	!
	! Sort the array
	!
	FOR I% = 1% TO DRLOOP% - 1%

		INDEX% = I%
		PP_MONTHLY_DR(0%) = PP_MONTHLY_DR(I%)

		FOR J% = I% + 1% TO DRLOOP%

			IF PP_MONTHLY_DR(J%)::DRIVER + &
				PP_MONTHLY_DR(J%)::PRODUCT < &
				PP_MONTHLY_DR(0%)::DRIVER + &
				PP_MONTHLY_DR(0%)::PRODUCT
			THEN
				INDEX% = J%
			END IF

		NEXT J%

		PP_MONTHLY_DR(0%)     = PP_MONTHLY_DR(INDEX%)
		PP_MONTHLY_DR(INDEX%) = PP_MONTHLY_DR(I%)
		PP_MONTHLY_DR(I%)     = PP_MONTHLY_DR(0%)

	NEXT I%

	!
	! Get Card information
	!
	!
	KEY$ = PP_MONTHLY_DR(1%)::DRIVER
	PP_MONTHLY = PP_MONTHLY_DR(1%)
	GOSUB GetCard

	!*******************************************************************
	! Print the driver totals
	!*******************************************************************

	GOSUB VehicleSum
	GOSUB DriverTitle

	TEST_CARD$ = ""
	CARD_TOTAL = 0.0
	TOT_FED_TAX = 0.0
	TOT_STA_TAX = 0.0
	TOT_COU_TAX = 0.0
	TOT_CTY_TAX = 0.0
	TOT_SAL_TAX = 0.0
	TOT_BASE_PRICE = 0.0
	CUST_FUELQTY      = 0.0
	CUST_NONFUELQTY   = 0.0
	CUST_FUELSALES    = 0.0
	CUST_NONFUELSALES = 0.0


	FOR I% = 1% TO DRLOOP%

		PP_MONTHLY = PP_MONTHLY_DR(I%)

		IF PP_MONTHLY::DRIVER <> TEST_CARD$ AND TEST_CARD$ <> ""
		THEN
			GOSUB DriverTotal
			GOSUB NewPage &
				IF BODY_COUNT% >= FORM_GROUP(FRM_CARDBODY%)::NUMBER
			KEY$ = PP_MONTHLY::DRIVER
			GOSUB GetCard
			GOSUB DriverTitle
		END IF

		TEST_CARD$ = PP_MONTHLY::DRIVER

		GOSUB GetSiteProduct

		SELECT PP_MONTHLY::FTYPE

		CASE "F", '0'C
			CARD_FUELQTY = PP_MONTHLY::QUANTITY
			CARD_NONFUELQTY = 0.0

		CASE ELSE
			CARD_FUELQTY = 0.0
			CARD_NONFUELQTY = PP_MONTHLY::QUANTITY

		END SELECT

		LINE_TOTAL = PP_MONTHLY::SELLPRICE
		CARD_TOTAL = CARD_TOTAL + LINE_TOTAL

		IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
			PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "F") = 0%
		THEN
			FED_TAX = FUNC_ROUND(CARD_FUELQTY * &
				(PP_SITE_PRODUCT::FED_RATE), 3%)
		ELSE
			FED_TAX = 0.0
		END IF

		IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
			PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "S") = 0%
		THEN
			STA_TAX = FUNC_ROUND(CARD_FUELQTY * &
				(PP_SITE_PRODUCT::STA_RATE), 3%)
		ELSE
			STA_TAX = 0.0
		END IF

		IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
			PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "D") = 0%
		THEN
			COU_TAX = FUNC_ROUND(CARD_FUELQTY * &
				(PP_SITE_PRODUCT::COU_RATE), 3%)
		ELSE
			COU_TAX = 0.0
		END IF

		IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
			PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "C") = 0%
		THEN
			CTY_TAX = FUNC_ROUND(CARD_FUELQTY * &
				(PP_SITE_PRODUCT::CTY_RATE), 3%)
		ELSE
			CTY_TAX = 0.0
		END IF

		IF FNCHECKEXEMPT%(PP_MONTHLY::CUSNUM, PP_MONTHLY::DRIVER, &
			PP_MONTHLY::PRODUCT, PP_MONTHLY::STATE, "A") = 0%
		THEN
			SAL_TAX = FUNC_ROUND(CARD_FUELQTY * &
				(PP_SITE_PRODUCT::STX_RATE), 3%)
		ELSE
			SAL_TAX = 0.0
		END IF

		BASE_PRICE = LINE_TOTAL - FED_TAX - STA_TAX - &
			COU_TAX - CTY_TAX - SALTAX

		TOT_FED_TAX = TOT_FED_TAX + FED_TAX
		TOT_STA_TAX = TOT_STA_TAX + STA_TAX
		TOT_COU_TAX = TOT_COU_TAX + COU_TAX
		TOT_CTY_TAX = TOT_CTY_TAX + CTY_TAX
		TOT_SAL_TAX = TOT_SAL_TAX + SAL_TAX
		TOT_BASE_PRICE = TOT_BASE_PRICE + BASE_PRICE

		CUST_FUELQTY = CUST_FUELQTY + CARD_FUELQTY
		CUST_NONFUELQTY = CUST_NONFUELQTY + CARD_NONFUELQTY

		!
		! Get Product information
		!
		V% = PD_EXAM_PRODUCT(PP_MONTHLY::PRODUCT, PD_PRODUCT_EXAM)

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_DRIVERBODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	NEXT I%

	GOSUB DriverTotal
	GOSUB NewPage

	!
	! Sort the Product array
	!
	FOR I% = 1% TO PDLOOP% - 1%

		INDEX% = I%
		PP_MONTHLY_PD(0%) = PP_MONTHLY_PD(I%)

		FOR J% = I% + 1% TO PDLOOP%

			IF PP_MONTHLY_PD(J%)::PRODUCT < &
				PP_MONTHLY_PD(0%)::PRODUCT
			THEN
				INDEX% = J%
			END IF

		NEXT J%

		PP_MONTHLY_PD(0%)     = PP_MONTHLY_PD(INDEX%)
		PP_MONTHLY_PD(INDEX%) = PP_MONTHLY_PD(I%)
		PP_MONTHLY_PD(I%)     = PP_MONTHLY_PD(0%)

		FOR J% = 0% TO 4%
			TEMP = PD_TAXABLE(INDEX%, J%)
			PD_TAXABLE(INDEX%, J%) = PD_TAXABLE(I%, J%)
			PD_TAXABLE(I%, J%) = TEMP
		NEXT J%

	NEXT I%

	!
	! Print the information
	!
	GOSUB VehicleSum

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_FLEETTITLE%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	FOR I% = 1% TO PDLOOP%

		PP_MONTHLY = PP_MONTHLY_PD(I%)

		GOSUB GetSiteProduct

		SELECT PP_MONTHLY::FTYPE

		CASE "F", '0'C
			CARD_FUELQTY = PP_MONTHLY::QUANTITY
			CARD_NONFUELQTY = 0.0

		CASE ELSE
			CARD_FUELQTY = 0.0
			CARD_NONFUELQTY = PP_MONTHLY::QUANTITY

		END SELECT

		LINE_TOTAL = PP_MONTHLY::SELLPRICE
		CARD_TOTAL = CARD_TOTAL + LINE_TOTAL

		FED_TAX = FUNC_ROUND(PD_TAXABLE(I%, 0%) * &
			(PP_SITE_PRODUCT::FED_RATE), 3%)

		STA_TAX = FUNC_ROUND(PD_TAXABLE(I%, 1%) * &
			(PP_SITE_PRODUCT::STA_RATE), 3%)

		COU_TAX = FUNC_ROUND(PD_TAXABLE(I%, 2%) * &
			(PP_SITE_PRODUCT::COU_RATE), 3%)

		CTY_TAX = FUNC_ROUND(PD_TAXABLE(I%, 3%) * &
			(PP_SITE_PRODUCT::CTY_RATE), 3%)

		SAL_TAX = FUNC_ROUND(PD_TAXABLE(I%, 4%) * &
			(PP_SITE_PRODUCT::STX_RATE), 3%)

		BASE_PRICE = LINE_TOTAL - FED_TAX - STA_TAX - &
			COU_TAX - CTY_TAX - SALTAX

		TOT_FED_TAX = TOT_FED_TAX + FED_TAX
		TOT_STA_TAX = TOT_STA_TAX + STA_TAX
		TOT_COU_TAX = TOT_COU_TAX + COU_TAX
		TOT_CTY_TAX = TOT_CTY_TAX + CTY_TAX
		TOT_SAL_TAX = TOT_SAL_TAX + SAL_TAX
		TOT_BASE_PRICE = TOT_BASE_PRICE + BASE_PRICE

		CUST_FUELQTY = CUST_FUELQTY + CARD_FUELQTY
		CUST_NONFUELQTY = CUST_NONFUELQTY + CARD_NONFUELQTY

		!
		! Get Product information
		!
		V% = PD_EXAM_PRODUCT(PP_MONTHLY::PRODUCT, PD_PRODUCT_EXAM)

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_DRIVERBODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	NEXT I%

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_FLEETTOTAL%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_CARDBODY%)::NUMBER
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)
	NEXT I%

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_CARDBODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	TOT_FED_TAX    = 0.0
	TOT_STA_TAX    = 0.0
	TOT_COU_TAX    = 0.0
	TOT_CTY_TAX    = 0.0
	TOT_SAL_TAX    = 0.0
	TOT_BASE_PRICE = 0.0

	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! PUT LAST INVOICE NUMBER OUT TO THE CONTROL FILE.
	!
3000	IF (INVOICE$ <> "")
	THEN
		WHEN ERROR IN
			GET #PP_CONTROL.CH%, RECORD 1%

			PP_CONTROL::INVDATE  = INVDATE$
			PP_CONTROL::LAST_INV = INVOICE$

			UPDATE #PP_CONTROL.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE ExitControl IF ERR = 9%
			FILENAME$ = "PP_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

 ExitControl:

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

	!***************************************************************
	! File Subroutines
	!***************************************************************

	!
	! Get Card
	!
 GetCard:
17000	IF PP_CARD::CARD + PP_CARD::CUSNUM <> KEY$ + PP_MONTHLY::CUSNUM
	THEN
		PP_CARD::CUSNUM = PP_MONTHLY::CUSNUM
		PP_CARD::CARD = KEY$
		PP_CARD::DESCRIPTION  = ""

		WHEN ERROR IN
			GET #PP_CARD.CH%, KEY #0% EQ &
				PP_MONTHLY::CUSNUM + KEY$, REGARDLESS
		USE
			CONTINUE ExitCard IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PP_CARD"
			CONTINUE HelpError
		END WHEN
	END IF

 ExitCard:
	RETURN

	!
	! Get CardExempt
	!
 ! GetCardExempt:
 !17100	IF PP_CARDEXEMPT::CUSNUM + PP_CARDEXEMPT::CARD <> &
 !		PP_MONTHLY::CUSNUM + PP_CARD::CARD
 !	THEN
 !		PP_CARDEXEMPT::CUSNUM = PP_MONTHLY::CUSNUM
 !		PP_CARDEXEMPT::CARD = PP_CARD::CARD
 !
 !		GET #PP_CARDEXEMPT.CH%, KEY #0% EQ PP_MONTHLY::CUSNUM + &
 !			PP_CARD::CARD, REGARDLESS
 !	END IF
 !
 ! ExitCardExempt:
 !	RETURN

	!
	! Get Discount
	!
 GetDiscount:
17200	!IF PP_DISCOUNT::CODE <> PP_MONTHLY::???
	!THEN
	!	PP_DISCOUNT::CODE = PP_MONTHLY::???
	!	PP_DISCOUNT::DESCRIPTION  = ""

	!	GET #PP_DISCOUNT.CH%, KEY #0% EQ "", REGARDLESS
	!END IF

 ! ExitDiscount:
 !	RETURN

	!
	! Get Site
	!
 GetSite:
17300	IF PP_SITE::SITE <> PP_MONTHLY::SITE OR &
		PP_SITE::HOST <> PP_MONTHLY::HOST OR &
		PP_SITE::STYPE <> PP_MONTHLY::STYPE
	THEN
		PP_SITE::SITE   = PP_MONTHLY::SITE
		PP_SITE::HOST   = PP_MONTHLY::HOST
		PP_SITE::STYPE  = PP_MONTHLY::STYPE
		PP_SITE::SNAME  = "***"

		WHEN ERROR IN
			GET #PP_SITE.CH%, KEY #0% EQ PP_MONTHLY::HOST + &
				PP_MONTHLY::SITE + &
				PP_MONTHLY::STYPE, REGARDLESS
		USE
			CONTINUE ExitSite IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PP_SITE"
			CONTINUE HelpError
		END WHEN

		!
		! Create a description if we don't have one.
		!
		IF PP_SITE::SNAME = ""
		THEN
			PP_SITE::SNAME = TRM$(PP_SITE::ADDRESS) + ", " + &
				TRM$(PP_SITE::CITY) + ", " + &
				TRM$(PP_SITE::STATE)
		END IF
	END IF

 ExitSite:
	RETURN

	!
	! Get Site Product
	!
 GetSiteProduct:
17400	IF PP_SITE_PRODUCT::HOST <> PP_MONTHLY::SITE OR &
		PP_SITE_PRODUCT::HOST <> PP_MONTHLY::HOST OR &
		PP_SITE_PRODUCT::STYPE <> PP_MONTHLY::STYPE OR &
		PP_SITE_PRODUCT::PRODUCT <> PP_MONTHLY::PRODUCT
	THEN
		PP_SITE_PRODUCT::SITE    = PP_MONTHLY::SITE
		PP_SITE_PRODUCT::HOST    = PP_MONTHLY::HOST
		PP_SITE_PRODUCT::STYPE   = PP_MONTHLY::STYPE
		PP_SITE_PRODUCT::PRODUCT = PP_MONTHLY::PRODUCT

		WHEN ERROR IN
			GET #PP_SITE_PRODUCT.CH%, KEY #0% EQ PP_MONTHLY::HOST + &
				PP_MONTHLY::SITE + &
				PP_MONTHLY::STYPE + &
				PP_MONTHLY::PRODUCT, REGARDLESS
		USE
			CONTINUE ExitSiteProduct IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PP_SITE_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

 ExitSiteProduct:
	RETURN

	!
	! Get Transaction Type
	!
 ! GetTranType:
 !17500	IF PP_TRANTYPE::TRANTYPE <> PP_MONTHLY::TRNTYPE
 !	THEN
 !		PP_TRANTYPE::TRANTYPE = PP_MONTHLY::TRNTYPE
 !		PP_TRANTYPE::DESCRIPTION = ""
 !
 !		GET #PP_TRANTYPE.CH%, KEY #0% EQ PP_MONTHLY::TRNTYPE, REGARDLESS
 !	END IF
 !
 ! ExitTranType:
 !	RETURN

	%PAGE

 CardTotal:
17600	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_CARDTOTAL%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CUST_FUELQTY = CUST_FUELQTY + CARD_FUELQTY
	CUST_FUELSALES = CUST_FUELSALES + CARD_FUELSALES

	CUST_NONFUELQTY = CUST_NONFUELQTY + CARD_NONFUELQTY
	CUST_NONFUELSALES = CUST_NONFUELSALES + CARD_NONFUELSALES

	TEST_CARD$        = ""
	CARD_MILES        = 0.0
	CARD_MPG          = 0.0
	CARD_FUELQTY      = 0.0
	CARD_NONFUELQTY   = 0.0
	CARD_FUELSALES    = 0.0
	CARD_NONFUELSALES = 0.0

	RETURN

 CustTotal:
17700	!
	! Figure discount, if any
	!
	DIS_AMT  = 0.0
	DISLOOP% = 0%

 DisLoop:
	IF (CUST_FUELQTY > PP_DISCOUNT::OVER(DISLOOP%)) AND &
		(PP_DISCOUNT::OVER(DISLOOP%) <> 0.0)
	THEN
		DIS_AMT = FUNC_ROUND(PP_DISCOUNT::RATE(DISLOOP%) * &
			CUST_FUELQTY, 2%)

		DISLOOP% = DISLOOP% + 1%
		GOTO DisLoop IF DISLOOP% < 9%
	END IF

	DISDAYS% = VAL%(PP_CONTROL::DIS_DAYS)
	DISDATE$ = DATE_INVDCODE(DATE_DAYCODE(INVDATE$) + DISDAYS%)

	FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_CARDBODY%)::NUMBER

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)

	NEXT I%

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_CARDBODY%)::NUMBER

	!
	! Print the bottom of form
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_CUSTTOTAL%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print lines to botton of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Print the top of form
	!
	PAGE_NUMBER% = PAGE_NUMBER% + 1%

	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	BODY_COUNT% = 0%

	CUST_FUELQTY      = 0.0
	CUST_NONFUELQTY   = 0.0
	CUST_FUELSALES    = 0.0
	CUST_NONFUELSALES = 0.0

	RETURN

 DriverTitle:
17800	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_DRIVERTITLE%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	RETURN

 DriverTotal:
17900	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_DRIVERTOTAL%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CARD_TOTAL = 0.0
	TOT_FED_TAX = 0.0
	TOT_STA_TAX = 0.0
	TOT_COU_TAX = 0.0
	TOT_CTY_TAX = 0.0
	TOT_SAL_TAX = 0.0
	TOT_BASE_PRICE = 0.0

	CUST_FUELQTY      = 0.0
	CUST_NONFUELQTY   = 0.0
	CUST_FUELSALES    = 0.0
	CUST_NONFUELSALES = 0.0

	RETURN

 VehicleSum:
18000	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_VEHICLESUM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	RETURN

	%PAGE

	!*******************************************************************
	! Goto a new page
	!*******************************************************************
 NewPage:
18100	FOR NI% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_CARDBODY%)::NUMBER

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)

	NEXT NI%

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_CARDBODY%)::NUMBER

	!
	! Print the bottom of form
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print lines to botton of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Print the top of form
	!
	PAGE_NUMBER% = PAGE_NUMBER% + 1%

	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	BODY_COUNT% = 0%

	RETURN

	%PAGE

 LoadForm:
18200	!*******************************************************************
	! Initilize form
	!*******************************************************************

	!
	! Get form from the PO form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		PP_FORM.DEV$ + "PP_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "invoice form is missing", "E", &
			SCOPE::PRG_PROGRAM, REPORT$,NUM1$(SMG_STATUS%))

		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_CARDTITLE% = 0%
	FRM_CARDBODY% = 0%
	FRM_CARDTOTAL% = 0%
	FRM_CUSTTOTAL% = 0%
	FRM_DRIVERTITLE% = 0%
	FRM_DRIVERBODY% = 0%
	FRM_DRIVERTOTAL% = 0%
	FRM_VEHICLESUM% = 0%
	FRM_FLEETTITLE% = 0%
	FRM_FLEETTOTAL% = 0%
	FRM_BOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-CARDTITLE"
			FRM_CARDTITLE% = I%

		CASE "FRM-CARDBODY"
			FRM_CARDBODY% = I%

		CASE "FRM-CARDTOTAL"
			FRM_CARDTOTAL% = I%

		CASE "FRM-CUSTTOTAL"
			FRM_CUSTTOTAL% = I%

		CASE "FRM-DRIVERTITLE"
			FRM_DRIVERTITLE% = I%

		CASE "FRM-DRIVERBODY"
			FRM_DRIVERBODY% = I%

		CASE "FRM-DRIVERTOTAL"
			FRM_DRIVERTOTAL% = I%

		CASE "FRM-VEHICLESUM"
			FRM_VEHICLESUM% = I%

		CASE "FRM-FLEETTITLE"
			FRM_FLEETTITLE% = I%

		CASE "FRM-FLEETTOTAL"
			FRM_FLEETTOTAL% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%

		END SELECT

	NEXT I%

	RETURN

	%PAGE

 Alignment:
18300	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	PAGE_NUMBER% = 1%
	LINE_COUNT% = 0%
	BODY_COUNT% = 0%

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"

	!++
	! Abstract:ALIGNMENT
	!
	!
	! Index:
	!
	!--

	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	!
	! Print the top of the form
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	FOR I% = 1% TO 3%

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CARDBODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

	NEXT I%

	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_CARDBODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_CARDBODY%)::NUMBER

	!
	! Display BOTTOM
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print lines to bottom of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

18400	!******************************************************************
	! Function to look up the exempt status of a specified card,
	! for a specific tax type
	!
	! Returns 0 if exempt, else returns -1.
	!******************************************************************

	DEF* FNCHECKEXEMPT%(CUSNUM$, CARD$, PRODUCT$, STATE$, AUTHORITY$)

		FNCHECKEXEMPT% = 0%

		WHEN ERROR IN
			GET #PP_CARDEXEMPT.CH%, &
				KEY #0% EQ CUSNUM$ + CARD$ + PRODUCT$ + STATE$, &
				REGARDLESS
		USE
			CONTINUE 18490
		END WHEN

		WHILE (CUSNUM$ = PP_CARDEXEMPT::CUSNUM) AND &
			(CARD$ = PP_CARDEXEMPT::CARD) AND &
			(PRODUCT$ = PP_CARDEXEMPT::PRODUCT) AND &
			(STATE$ = PP_CARDEXEMPT::STATE)

			FNCHECKEXEMPT% = -1% &
				IF PP_CARDEXEMPT::AUTHORITY = AUTHORITY$

			GET #PP_CARDEXEMPT.CH%, REGARDLESS
		NEXT

18490	FNEND

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	RESUME 19990

 HelpError:
19990	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP (PP_MONTHLY)	PP_MONTHLY_CDD		PP_MONTHLY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP (PP_CONTROL)	PP_CONTROL_CDD		PP_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.HB"
	MAP (PP_DISCOUNT)	PP_DISCOUNT_CDD		PP_DISCOUNT

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	MAP (JOUR_FORM) &
		AR_35CUSTOM.ADDLINE$(6%) = 50%, &
		INVDATE$ = 8%, &
		DISDATE$ = 8%, &
		LINE_TOTAL, &
		CARD_TOTAL, &
		CARD_MILES, &
		CARD_MPG, &
		CARD_FUELQTY, &
		CARD_NONFUELQTY, &
		CARD_FUELSALES, &
		CARD_NONFUELSALES, &
		CUST_FUELQTY, &
		CUST_NONFUELQTY, &
		CUST_FUELSALES, &
		CUST_NONFUELSALES, &
		FED_TAX, &
		STA_TAX, &
		COU_TAX, &
		CTY_TAX, &
		SAL_TAX, &
		BASE_PRICE, &
		TOT_FED_TAX, &
		TOT_STA_TAX, &
		TOT_COU_TAX, &
		TOT_CTY_TAX, &
		TOT_SAL_TAX, &
		TOT_BASE_PRICE, &
		DIS_AMT, &
		PAGE_NUMBER%

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!************************************************************
	! Fields for the PP_MONTHLY transaction file
	!************************************************************

	CASE "PP_MONTHLY::CUSNUM"
		TEXTVALUE$ = PP_MONTHLY::CUSNUM

	CASE "PP_MONTHLY::VEHICLE"
		TEXTVALUE$ = PP_MONTHLY::VEHICLE

	CASE "PP_MONTHLY::DRIVER"
		TEXTVALUE$ = PP_MONTHLY::DRIVER

	CASE "PP_MONTHLY::TRANDATE"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::TRANDATE, 8%)

	CASE "PP_MONTHLY::TRANDATE6"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::TRANDATE, 6%)

	CASE "PP_MONTHLY::TRANTIME"
		TEXTVALUE$ = PRNT_TIME(PP_MONTHLY::TRANTIME, 2048%)

	CASE "PP_MONTHLY::HOST"
		TEXTVALUE$ = PP_MONTHLY::HOST

	CASE "PP_MONTHLY::SITE"
		TEXTVALUE$ = PP_MONTHLY::SITE

	CASE "PP_MONTHLY::STYPE"
		TEXTVALUE$ = PP_MONTHLY::STYPE

	CASE "PP_MONTHLY::PRODUCT"
		TEXTVALUE$ = PP_MONTHLY::PRODUCT

	CASE "PP_MONTHLY::UOM"
		TEXTVALUE$ = PP_MONTHLY::UOM

	CASE "PP_MONTHLY::QUANTITY"
		REALVALUE = PP_MONTHLY::QUANTITY

	CASE "PP_MONTHLY::ODOM"
		REALVALUE = PP_MONTHLY::ODOM

	CASE "PP_MONTHLY::SLTYPE"
		TEXTVALUE$ = PP_MONTHLY::SLTYPE

	CASE "PP_MONTHLY::SELLPRICE"
		REALVALUE = PP_MONTHLY::SELLPRICE

	CASE "PP_MONTHLY::TRANCOST"
		REALVALUE = PP_MONTHLY::TRANCOST

	CASE "PP_MONTHLY::MISCKEYB"
		TEXTVALUE$ = PP_MONTHLY::MISCKEYB

		!
		! Convert leading zeroes to spaces
		!
		I% = 1%
		WHILE MID(TEXTVALUE$, I%, 1%) = "0"
			TEXTVALUE$ = LEFT(TEXTVALUE$, I% - 1%) + &
				" " + &
				RIGHT(TEXTVALUE$, I% + 1%)
			I% = I% + 1%
		NEXT

	CASE "PP_MONTHLY::TRNTYPE"
		TEXTVALUE$ = PP_MONTHLY::TRNTYPE

	CASE "PP_MONTHLY::DISCOUNT"
		TEXTVALUE$ = PP_MONTHLY::DISCOUNT

	CASE "PP_MONTHLY::ICBDATE"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::ICBDATE, 8%)

	CASE "PP_MONTHLY::ICBDATE6"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::ICBDATE, 6%)

	CASE "PP_MONTHLY::TRNNUM"
		TEXTVALUE$ = PP_MONTHLY::TRNNUM

	CASE "PP_MONTHLY::STAXRATE"
		REALVALUE = PP_MONTHLY::STAXRATE

	CASE "PP_MONTHLY::PUMP"
		TEXTVALUE$ = PP_MONTHLY::PUMP

	CASE "PP_MONTHLY::BUYFRAN"
		TEXTVALUE$ = PP_MONTHLY::BUYFRAN

	CASE "PP_MONTHLY::CAPDATE"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::CAPDATE, 8%)

	CASE "PP_MONTHLY::CAPDATE6"
		TEXTVALUE$ = PRNT_DATE(PP_MONTHLY::CAPDATE, 6%)

	CASE "PP_MONTHLY::CAPTIME"
		TEXTVALUE$ = PRNT_TIME(PP_MONTHLY::CAPTIME, 2048%)

	CASE "PP_MONTHLY::POSTBNUM"
		TEXTVALUE$ = PP_MONTHLY::POSTBNUM

	CASE "PP_MONTHLY::TRANSOURCE"
		TEXTVALUE$ = PP_MONTHLY::TRANSOURCE

	CASE "PP_MONTHLY::EDITACT"
		TEXTVALUE$ = PP_MONTHLY::EDITACT

	CASE "PP_MONTHLY::JULIANDAY"
		TEXTVALUE$ = PP_MONTHLY::JULIANDAY

	CASE "PP_MONTHLY::RSTATION"
		TEXTVALUE$ = PP_MONTHLY::RSTATION

	CASE "PP_MONTHLY::STATE"
		TEXTVALUE$ = PP_MONTHLY::STATE

	CASE "PP_MONTHLY::BATCH"
		TEXTVALUE$ = PP_MONTHLY::BATCH

	CASE "PP_MONTHLY::IDENTITY"
		TEXTVALUE$ = PP_MONTHLY::IDENTITY

	!************************************************************
	! Fields for the Accounts Receivable Ver. 3.5 Customer file
	!************************************************************

	CASE "AR_35CUSTOM::CUSNUM"
		TEXTVALUE$ = AR_35CUSTOM::CUSNUM

	CASE "AR_35CUSTOM::CUSNAM"
		TEXTVALUE$ = AR_35CUSTOM::CUSNAM

	CASE "AR_35CUSTOM::TTYPE"
		TEXTVALUE$ = AR_35CUSTOM::TTYPE

	CASE "AR_35CUSTOM::CATEGORY"
		TEXTVALUE$ = AR_35CUSTOM::CATEGORY

	CASE "AR_35CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::BDATE, 8%)

	CASE "AR_35CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::BDATE, 6%)

	CASE "AR_35CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM::SSTATUS

	CASE "AR_35CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::EDATE, 8%)

	CASE "AR_35CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM::EDATE, 6%)

	CASE "AR_35CUSTOM::ADD1"
		TEXTVALUE$ = AR_35CUSTOM::ADD1

	CASE "AR_35CUSTOM::ADD2"
		TEXTVALUE$ = AR_35CUSTOM::ADD2

	CASE "AR_35CUSTOM::ADD3"
		TEXTVALUE$ = AR_35CUSTOM::ADD3

	CASE "AR_35CUSTOM::CITY"
		TEXTVALUE$ = AR_35CUSTOM::CITY

	CASE "AR_35CUSTOM::STATE"
		TEXTVALUE$ = AR_35CUSTOM::STATE

	CASE "AR_35CUSTOM::ZIP"
		TEXTVALUE$ = AR_35CUSTOM::ZIP

	CASE "AR_35CUSTOM::COUNTRY"
		TEXTVALUE$ = AR_35CUSTOM::COUNTRY

	CASE "AR_35CUSTOM::COUNTY"
		TEXTVALUE$ = AR_35CUSTOM::COUNTY

	CASE "AR_35CUSTOM:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(1%)

	CASE "AR_35CUSTOM:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(2%)

	CASE "AR_35CUSTOM:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(3%)

	CASE "AR_35CUSTOM:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_35CUSTOM.ADDLINE$(4%)

	CASE "AR_35CUSTOM::PHONE"
		TEXTVALUE$ = AR_35CUSTOM::PHONE

	CASE "AR_35CUSTOM::METHOD"
		TEXTVALUE$ = AR_35CUSTOM::METHOD

	CASE "AR_35CUSTOM::STMTFLG"
		TEXTVALUE$ = AR_35CUSTOM::STMTFLG

	CASE "AR_35CUSTOM::ALPSRT"
		TEXTVALUE$ = AR_35CUSTOM::ALPSRT

	CASE "AR_35CUSTOM::SERCHRG"
		TEXTVALUE$ = AR_35CUSTOM::SERCHRG

	CASE "AR_35CUSTOM::TAXCODE"
		TEXTVALUE$ = AR_35CUSTOM::TAXCODE

	CASE "AR_35CUSTOM::TAXEXEMP"
		TEXTVALUE$ = AR_35CUSTOM::TAXEXEMP

	CASE "AR_35CUSTOM::LOCATION"
		TEXTVALUE$ = AR_35CUSTOM::LOCATION

	CASE "AR_35CUSTOM::TERMS"
		TEXTVALUE$ = AR_35CUSTOM::TERMS

	CASE "AR_35CUSTOM::CARRIER"
		TEXTVALUE$ = AR_35CUSTOM::CARRIER

	CASE "AR_35CUSTOM::SALESMAN"
		TEXTVALUE$ = AR_35CUSTOM::SALESMAN

	CASE "AR_35CUSTOM::CREDITLIM"
		REALVALUE  = AR_35CUSTOM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT"
		REALVALUE  = AR_35CUSTOM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM::TAXFLAG

	!************************************************************
	! Fields for the Product Description file
	!************************************************************

	CASE "PD_PRODUCT::PRODUCT_NUM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PRODUCT_NUM

	CASE "PD_PRODUCT::DESCRIPTION"
		TEXTVALUE$ = PD_PRODUCT_EXAM::DESCRIPTION

	CASE "PD_PRODUCT::PROD_TYPE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PROD_TYPE

	CASE "PD_PRODUCT::CATEGORY"
		TEXTVALUE$ = PD_PRODUCT_EXAM::CATEGORY

	CASE "PD_PRODUCT::UOM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::UOM

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT_EXAM::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT_EXAM::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 8%)

	CASE "PD_PRODUCT::BDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 6%)

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 8%)

	CASE "PD_PRODUCT::EDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 6%)

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SECONDARY_CODE

	CASE "PD_PRODUCT::WEIGHT"
		REALVALUE = PD_PRODUCT_EXAM::WEIGHT

	CASE "PD_PRODUCT::BOMUOM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::BOMUOM

	CASE "PD_PRODUCT::PRODUCT_FACTOR"
		REALVALUE = PD_PRODUCT_EXAM::PRODUCT_FACTOR

	!************************************************************
	! Fields for the Card file
	!************************************************************

	CASE "PP_CARD::CUSNUM"
		TEXTVALUE$ = PP_CARD::CUSNUM

	CASE "PP_CARD::CARD"
		TEXTVALUE$ = PP_CARD::CARD

	CASE "PP_CARD::CTYPE"
		TEXTVALUE$ = PP_CARD::CTYPE

	CASE "PP_CARD::DESCRIPTION"
		TEXTVALUE$ = PP_CARD::DESCRIPTION

	CASE "PP_CARD::ODOMETER"
		REALVALUE = PP_CARD::ODOMETER

	!************************************************************
	! Fields for the Card Exemption file
	!************************************************************

	CASE "PP_CARDEXEMPT::CUSNUM"
		TEXTVALUE$ = PP_CARDEXEMPT::CUSNUM

	CASE "PP_CARDEXEMPT::CARD"
		TEXTVALUE$ = PP_CARDEXEMPT::CARD

	CASE "PP_CARDEXEMPT::STATE"
		TEXTVALUE$ = PP_CARDEXEMPT::STATE

	CASE "PP_CARDEXEMPT::AUTHORITY"
		TEXTVALUE$ = PP_CARDEXEMPT::AUTHORITY

	CASE "PP_CARDEXEMPT::PRODUCT"
		TEXTVALUE$ = PP_CARDEXEMPT::PRODUCT

	!************************************************************
	! Fields for the Control file
	!************************************************************

	CASE "PP_CONTROL::CUSNUM"
		TEXTVALUE$ = PP_CONTROL::CUSNUM

	CASE "PP_CONTROL::AR_ACCOUNT"
		TEXTVALUE$ = PP_CONTROL::AR_ACCOUNT

	CASE "PP_CONTROL::HOST_NUM"
		TEXTVALUE$ = PP_CONTROL::HOST_NUM

	CASE "PP_CONTROL::DIS_DAYS"
		TEXTVALUE$ = PP_CONTROL::DIS_DAYS

	CASE "PP_CONTROL::INVDATE"
		TEXTVALUE$ = PRNT_DATE(PP_CONTROL::INVDATE, 8%)

	CASE "PP_CONTROL::LAST_INV"
		TEXTVALUE$ = PP_CONTROL::LAST_INV

	!************************************************************
	! Fields for the Discount file
	!************************************************************

	CASE "PP_DISCOUNT::CODE"
		TEXTVALUE$ = PP_DISCOUNT::CODE

	CASE "PP_DISCOUNT::DESCRIPTION"
		TEXTVALUE$ = PP_DISCOUNT::DESCRIPTION

	CASE "PP_DISCOUNT::METHOD"
		TEXTVALUE$ = PP_DISCOUNT::METHOD

	CASE "PP_DISCOUNT::OVER1"
		REALVALUE = PP_DISCOUNT::OVER(0%)

	CASE "PP_DISCOUNT::OVER2"
		REALVALUE = PP_DISCOUNT::OVER(1%)

	CASE "PP_DISCOUNT::OVER3"
		REALVALUE = PP_DISCOUNT::OVER(2%)

	CASE "PP_DISCOUNT::OVER4"
		REALVALUE = PP_DISCOUNT::OVER(3%)

	CASE "PP_DISCOUNT::OVER5"
		REALVALUE = PP_DISCOUNT::OVER(4%)

	CASE "PP_DISCOUNT::OVER6"
		REALVALUE = PP_DISCOUNT::OVER(5%)

	CASE "PP_DISCOUNT::OVER7"
		REALVALUE = PP_DISCOUNT::OVER(6%)

	CASE "PP_DISCOUNT::OVER8"
		REALVALUE = PP_DISCOUNT::OVER(7%)

	CASE "PP_DISCOUNT::OVER9"
		REALVALUE = PP_DISCOUNT::OVER(8%)

	CASE "PP_DISCOUNT::OVER10"
		REALVALUE = PP_DISCOUNT::OVER(9%)

	CASE "PP_DISCOUNT::RATE1"
		REALVALUE = PP_DISCOUNT::RATE(0%)

	CASE "PP_DISCOUNT::RATE2"
		REALVALUE = PP_DISCOUNT::RATE(1%)

	CASE "PP_DISCOUNT::RATE3"
		REALVALUE = PP_DISCOUNT::RATE(2%)

	CASE "PP_DISCOUNT::RATE4"
		REALVALUE = PP_DISCOUNT::RATE(3%)

	CASE "PP_DISCOUNT::RATE5"
		REALVALUE = PP_DISCOUNT::RATE(4%)

	CASE "PP_DISCOUNT::RATE6"
		REALVALUE = PP_DISCOUNT::RATE(5%)

	CASE "PP_DISCOUNT::RATE7"
		REALVALUE = PP_DISCOUNT::RATE(6%)

	CASE "PP_DISCOUNT::RATE8"
		REALVALUE = PP_DISCOUNT::RATE(7%)

	CASE "PP_DISCOUNT::RATE9"
		REALVALUE = PP_DISCOUNT::RATE(8%)

	CASE "PP_DISCOUNT::RATE10"
		REALVALUE = PP_DISCOUNT::RATE(9%)

	!************************************************************
	! Fields for the Site file
	!************************************************************

	CASE "PP_SITE::HOST"
		TEXTVALUE$ = PP_SITE::HOST

	CASE "PP_SITE::SITE"
		TEXTVALUE$ = PP_SITE::SITE

	CASE "PP_SITE::STYPE"
		TEXTVALUE$ = PP_SITE::STYPE

	CASE "PP_SITE::SNAME"
		TEXTVALUE$ = PP_SITE::SNAME

	CASE "PP_SITE::ADDRESS"
		TEXTVALUE$ = PP_SITE::ADDRESS

	CASE "PP_SITE::CITY"
		TEXTVALUE$ = PP_SITE::CITY

	CASE "PP_SITE::STATE"
		TEXTVALUE$ = PP_SITE::STATE

	CASE "PP_SITE::ZIP"
		TEXTVALUE$ = PP_SITE::ZIP

	CASE "PP_SITE::LOCSALE"
		TEXTVALUE$ = PP_SITE::LOCSALE

	CASE "PP_SITE::FORSALE"
		TEXTVALUE$ = PP_SITE::FORSALE

	CASE "PP_SITE::FORPUR"
		TEXTVALUE$ = PP_SITE::FORPUR

	!************************************************************
	! Fields for the Site Product file
	!************************************************************

	CASE "PP_SITE_PRODUCT::HOST"
		TEXTVALUE$ = PP_SITE_PRODUCT::HOST

	CASE "PP_SITE_PRODUCT::SITE"
		TEXTVALUE$ = PP_SITE_PRODUCT::SITE

	CASE "PP_SITE_PRODUCT::STYPE"
		TEXTVALUE$ = PP_SITE_PRODUCT::STYPE

	CASE "PP_SITE_PRODUCT::PRODUCT"
		TEXTVALUE$ = PP_SITE_PRODUCT::PRODUCT

	CASE "PP_SITE_PRODUCT::FED_INTP"
		TEXTVALUE$ = PP_SITE_PRODUCT::FED_INTP

	CASE "PP_SITE_PRODUCT::FED_RATE"
		REALVALUE = PP_SITE_PRODUCT::FED_RATE

	CASE "PP_SITE_PRODUCT::FED_ACCOUNT"
		TEXTVALUE$ = PP_SITE_PRODUCT::FED_ACCOUNT

	CASE "PP_SITE_PRODUCT::STA_INTP"
		TEXTVALUE$ = PP_SITE_PRODUCT::STA_INTP

	CASE "PP_SITE_PRODUCT::STA_RATE"
		REALVALUE = PP_SITE_PRODUCT::STA_RATE

	CASE "PP_SITE_PRODUCT::STA_ACCOUNT"
		TEXTVALUE$ = PP_SITE_PRODUCT::STA_ACCOUNT

	CASE "PP_SITE_PRODUCT::COU_INTP"
		TEXTVALUE$ = PP_SITE_PRODUCT::COU_INTP

	CASE "PP_SITE_PRODUCT::COU_RATE"
		REALVALUE = PP_SITE_PRODUCT::COU_RATE

	CASE "PP_SITE_PRODUCT::COU_ACCOUNT"
		TEXTVALUE$ = PP_SITE_PRODUCT::COU_ACCOUNT

	CASE "PP_SITE_PRODUCT::CTY_INTP"
		TEXTVALUE$ = PP_SITE_PRODUCT::CTY_INTP

	CASE "PP_SITE_PRODUCT::CTY_RATE"
		REALVALUE = PP_SITE_PRODUCT::CTY_RATE

	CASE "PP_SITE_PRODUCT::CTY_ACCOUNT"
		TEXTVALUE$ = PP_SITE_PRODUCT::CTY_ACCOUNT

	CASE "PP_SITE_PRODUCT::STX_INTP"
		TEXTVALUE$ = PP_SITE_PRODUCT::STX_INTP

	CASE "PP_SITE_PRODUCT::STX_RATE"
		REALVALUE = PP_SITE_PRODUCT::STX_RATE

	CASE "PP_SITE_PRODUCT::STX_ACCOUNT"
		TEXTVALUE$ = PP_SITE_PRODUCT::STX_ACCOUNT

	!************************************************************
	! Fields for the Transaction Type file
	!************************************************************

	CASE "PP_TRANTYPE::TRANTYPE"
		TEXTVALUE$ = PP_TRANTYPE::TRANTYPE

	CASE "PP_TRANTYPE::DESCRIPTION"
		TEXTVALUE$ = PP_TRANTYPE::DESCRIPTION

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "LINE_TOTAL"
		REALVALUE = LINE_TOTAL

	CASE "CARD_TOTAL"
		REALVALUE = CARD_TOTAL

	CASE "CARD_MILES"
		REALVALUE = CARD_MILES

	CASE "CARD_MPG"
		REALVALUE = CARD_MPG

	CASE "CARD_FUELSALES"
		REALVALUE = CARD_FUELSALES

	CASE "CARD_NONFUELSALES"
		REALVALUE = CARD_NONFUELSALES

	CASE "CARD_FUELQTY"
		REALVALUE = CARD_FUELQTY

	CASE "CARD_NONFUELQTY"
		REALVALUE = CARD_NONFUELQTY

	CASE "CUST_FUELQTY"
		REALVALUE = CUST_FUELQTY

	CASE "CUST_NONFUELQTY"
		REALVALUE = CUST_NONFUELQTY

	CASE "CUST_FUELSALES"
		REALVALUE = CUST_FUELSALES

	CASE "CUST_NONFUELSALES"
		REALVALUE = CUST_NONFUELSALES

	CASE "FED_TAX"
		REALVALUE = FED_TAX

	CASE "STA_TAX"
		REALVALUE = STA_TAX

	CASE "COU_TAX"
		REALVALUE = COU_TAX

	CASE "CTY_TAX"
		REALVALUE = CTY_TAX

	CASE "SAL_TAX"
		REALVALUE = SAL_TAX

	CASE "BASE_PRICE"
		REALVALUE = BASE_PRICE

	CASE "TOT_FED_TAX"
		REALVALUE = TOT_FED_TAX

	CASE "TOT_STA_TAX"
		REALVALUE = TOT_STA_TAX

	CASE "TOT_COU_TAX"
		REALVALUE = TOT_COU_TAX

	CASE "TOT_CTY_TAX"
		REALVALUE = TOT_CTY_TAX

	CASE "TOT_SAL_TAX"
		REALVALUE = TOT_SAL_TAX

	CASE "TOT_BASE_PRICE"
		REALVALUE = TOT_BASE_PRICE

	CASE "DIS_AMT"
		REALVALUE = DIS_AMT

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "INV_DATE"
		TEXTVALUE$ = PRNT_DATE(INVDATE$, 8%)

	CASE "INV_DATE6"
		TEXTVALUE$ = PRNT_DATE(INVDATE$, 6%)

	CASE "DIS_DATE"
		TEXTVALUE$ = PRNT_DATE(DISDATE$, 8%)

	CASE "DIS_DATE6"
		TEXTVALUE$ = PRNT_DATE(DISDATE$, 6%)

	END SELECT

	END SUB

