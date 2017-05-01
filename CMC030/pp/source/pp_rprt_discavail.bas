1	%TITLE "Print Pacific Pride Invoice Form"
	%SBTTL "PP_RPRT_DISCAVAIL"
	%IDENT "V3.6a Calico"

	!
	! Copyright (C) 1995 by Software Solutions
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
	! Software Solutions.
	!
	! Software Solutions assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions.
	!
	!++
	! ID:PP068
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_RPRT_DISCAVAIL/LINE
	!	$ LINK/EXECUTABLE=PP_EXE:*.EXE PP_RPRT_DISCAVAIL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_DISCAVAIL.OBJ;*
	!
	! Author:
	!
	!	03/08/95 - Kevin Handy
	!
	! Modification history:
	!
	!	03/08/95 - Kevin Handy
	!		Modified to leave off foreign sales.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Clean up (Check)
	!
	!	05/12/95 - Kevin Handy
	!		Open PP_CONTROL as .OPN instead of .MOD.
	!		Add REGARDLESS to get on PP_CONTROL .
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on comile.
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/96 - Kevin Handy
	!		Removed definition of 'FORM_GROUP',
	!		which was never used.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/06/99 - Kevin Handy
	!		Lose lines 17100, 17400, 17500 (never called)
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD

 !	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
 !	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP (PP_CONTROL)	PP_CONTROL_CDD		PP_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.HB"
	MAP (PP_DISCOUNT)	PP_DISCOUNT_CDD		PP_DISCOUNT

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

 !	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
 !	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

 !	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
 !	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get user input
	!
	YYYY_PP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Monthly Transaction Period\*
	!	.b
	!	.lm +5
	!	The ^*Monthly Transaction Period\* field enters the
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
 !	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"

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
 !	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.OPN"

670	!
	! Open PP transaction type file
	!
 !	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.OPN"

680	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.OPN"

		GET #PP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PP_CONTROL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DISCOUNT OFFERED LIST"
	TITLE$(2%) = "Pacific Pride System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Cust       Name                        " + &
		"Invoice   Discount"
	TITLE$(5%) = "."
	TITLE$(6%) = ""

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
	! The next thing we need to do is get a transaction sorted in
	! driver card order.
	!
2100	WHEN ERROR IN
		FIND #PP_MONTHLY.CH%, &
			KEY #1% EQ AR_35CUSTOM::CUSNUM, &
			REGARDLESS
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
	LINE_TOTAL = FUNC_ROUND(PP_MONTHLY::QUANTITY * &
		PP_MONTHLY::SELLPRICE, 3%)

	GOSUB CardTotal IF PP_MONTHLY::DRIVER <> TEST_CARD$ AND TEST_CARD$ <> ""

	IF TEST_CARD$ = ""
	THEN
		!
		! Find a card for this transaction
		!
		KEY$ = PP_MONTHLY::DRIVER
		GOSUB GetCard
	END IF

	TEST_CARD$ = PP_MONTHLY::DRIVER

	!
	! Load Driver array for later use
	!
	LINE_TOTAL = FUNC_ROUND(PP_MONTHLY::QUANTITY * &
		PP_MONTHLY::SELLPRICE, 3%)

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

	GOTO GetTranRec

 ExitTran1:
	GOSUB CardTotal
	GOSUB CustTotal

	!*******************************************************************
	! Print the driver totals
	!*******************************************************************

	TEST_CARD$ = ""
	CARD_TOTAL = 0.0
	TOT_FED_TAX = 0.0
	TOT_STA_TAX = 0.0
	TOT_COU_TAX = 0.0
	TOT_CTY_TAX = 0.0
	TOT_SAL_TAX = 0.0
	TOT_BASE_PRICE = 0.0
	CUST_FUELQTY = 0.0
	CUST_NONFUELQTY = 0.0
	CUST_FUELSALES = 0.0
	CUST_NONFUELSALES = 0.0

	!
	! Print the information
	!
	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISH(UTL_REPORTX)

 ExitControl:

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
		WHEN ERROR IN
			GET #PP_CARD.CH%, &
				KEY #0% EQ PP_MONTHLY::CUSNUM + KEY$, &
				REGARDLESS
		USE
			PP_CARD::CUSNUM = PP_MONTHLY::CUSNUM
			PP_CARD::CARD = KEY$
			PP_CARD::DESCRIPTION  = ""

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
17300	IF PP_SITE::SITE + PP_SITE::HOST + PP_SITE::STYPE <> &
		PP_MONTHLY::SITE + PP_MONTHLY::HOST + PP_MONTHLY::STYPE
	THEN
		PP_SITE::SITE   = PP_MONTHLY::SITE
		PP_SITE::HOST   = PP_MONTHLY::HOST
		PP_SITE::STYPE  = PP_MONTHLY::STYPE
		PP_SITE::SNAME  = "***"

		WHEN ERROR IN
			GET #PP_SITE.CH%, &
				KEY #0% EQ PP_MONTHLY::HOST + &
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
 ! GetSiteProduct:
 !17400	IF PP_SITE_PRODUCT::HOST + &
 !		PP_SITE_PRODUCT::HOST + &
 !		PP_SITE_PRODUCT::STYPE + &
 !		PP_SITE_PRODUCT::PRODUCT <> &
 !		PP_MONTHLY::SITE + &
 !		PP_MONTHLY::HOST + &
 !		PP_MONTHLY::STYPE + &
 !		PP_MONTHLY::PRODUCT
 !	THEN
 !		PP_SITE_PRODUCT::SITE    = PP_MONTHLY::SITE
 !		PP_SITE_PRODUCT::HOST    = PP_MONTHLY::HOST
 !		PP_SITE_PRODUCT::STYPE   = PP_MONTHLY::STYPE
 !		PP_SITE_PRODUCT::PRODUCT = PP_MONTHLY::PRODUCT
 !
 !		GET #PP_SITE_PRODUCT.CH%, KEY #0% EQ PP_MONTHLY::HOST + &
 !			PP_MONTHLY::SITE + &
 !			PP_MONTHLY::STYPE + &
 !			PP_MONTHLY::PRODUCT, REGARDLESS
 !	END IF
 !
 ! ExitSiteProduct:
 !	RETURN

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
17600	CUST_FUELQTY = CUST_FUELQTY + CARD_FUELQTY
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

	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 25%) + " " + &
		FORMAT$(CUST_FUELSALES + CUST_NONFUELSALES, &
			"######.##  ") + &
		FORMAT$(DIS_AMT, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CUST_FUELQTY      = 0.0
	CUST_NONFUELQTY   = 0.0
	CUST_FUELSALES    = 0.0
	CUST_NONFUELSALES = 0.0

	RETURN

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

32767	END
