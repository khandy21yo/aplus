1	%TITLE "Print Daily Transaction Journal"
	%SBTTL "PP_RPRT_LEDGER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1983 BY
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
	! ID:PP???
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Daily Transaction Journal\* Report contains the
	!	following information:
	!	.table 3,25
	!	.te
	!	Order Number	Order Type
	!	.te
	!	Order Date	Customer Number
	!	.te
	!	Customer Name	Customer PO Number
	!	.te
	!	Request Date	Product
	!	.te
	!	Description	Quantity Ordered
	!	.te
	!	Discount _#	Extended Price
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Journal
	!	.x Order Journal>Report
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_RPRT_LEDGER/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_LEDGER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_LEDGER.OBJ;*
	!
	! Author:
	!
	!	12/30/93 - Kevin Handy
	!		Taken from PP_RPRT_DAILY.
	!
	! Modification History:
	!
	!	01/25/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/12/2000 - Kevin Handy
	!		Added Wildcard Card Number.
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	12/23/2004 - Kevin Handy
	!		Don't print perind using date format.
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP (PP_MONTHLY)	PP_MONTHLY_CDD		PP_MONTHLY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	RECORD SUBTOTAL_CDD
		STRING	BUYFRAN = 3%	! Location
		STRING	PRODUCT = 14%	! Product
		LONG	COUNTER		! Number of lines
		REAL	QUANTITY	! Number of gallons
		REAL	PRICE		! Total paid
	END RECORD

	DIM SUBTOTAL_CDD SUBTOTAL(300%)
	DIM SUBTOTAL_CDD CUSSUBTOTAL(300%)

	RECORD TAXTOTAL_CDD
		STRING	STATE = 2%	! Location
		STRING	PRODUCT = 14%	! Product
		LONG	COUNTER		! Number of lines
		REAL	QUANTITY	! Number of gallons
		REAL	PRICE		! Total paid
	END RECORD

	DIM TAXTOTAL_CDD TAXTOTAL(300%)
	DIM TAXTOTAL_CDD CUSTAXTOTAL(300%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	YYYY_PP$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the
	!	batch to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*H\* - Host
	!	.end table
	!	An entry is required in this field.
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
	!	The ^*From Item\* field enters the
	!	item with which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (02) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field allows printing to
	!	end with a specified item.  The value entered must be in
	!	agreement with field (02) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
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
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	WLDCRD_CARD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Card\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Card\* field selects
	!	designated Driver or Vehicle card records. If either
	!	card matches it will be printed.
	!	.b
	!	For information on "Wildcarding" techniques refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	ONLY_TOTALS$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Only Totals\*
	!	.b
	!	.lm +5
	!	This field determines
	!	if full detail should be shown, or just the totals
	!	and subtotals.
	!	.lm -5
	!
	! Index:
	!	.x Only Totals
	!
	!--

	SHOW_SUBTOTALS$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 1%)

	!++
	! Abstract:FLD08
	!	^*(08) Show Subtotals\*
	!	.b
	!	.lm +5
	!	This field determines
	!	if subtotals should be shown.
	!	.lm -5
	!
	! Index:
	!	.x Show Subtotals
	!
	!--

300	!
	! Open Daily Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.OPN"
	USE
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Transaction Type
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PP_TRANTYPE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.OPN"
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
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
		K_NUM% = 0%
		TITLE$(1%) = "LEDGER BY CUSTOMER NUMBER"

	CASE "H"
		K_NUM% = 1%
		TITLE$(1%) = "LEDGER BY HOST"

	END SELECT

	TITLE$(2%) = "Period. " + YYYY_PP$
	TITLE$(3%) = "Pacific Pride System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "CustNum    Vehicle# Driver#  TranDate TraTm " + &
		"Host Site Styp Tran# SalTax Product        UM Quantity " + &
		"Odometer SellPrice TranCost MiscKey"

	TITLE$(6%) = SPACE$(14%) + "CapDate  CapTm ICBDate  TrSource " + &
		"EditA JuDay RStation Discount TranType"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	PRINT_LINE% = 0%
	LAST_CUSNUM$ = ""

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PP_MONTHLY.CH%, KEY #K_NUM%
		ELSE
			FIND #PP_MONTHLY.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next Daily record
	!
	WHEN ERROR IN
		GET #PP_MONTHLY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal &
			IF (PP_MONTHLY::CUSNUM > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING( &
			EDIT$(PP_MONTHLY::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "H"
		GOTO ExitTotal &
			IF (PP_MONTHLY::HOST > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING( &
			EDIT$(PP_MONTHLY::HOST, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	IF (WLDCRD_CARD$ <> "") AND (WLDCRD_CARD$ <> "*")
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PP_MONTHLY::VEHICLE, -1%), &
			WLDCRD_CARD$) = 0% AND &
			COMP_STRING(EDIT$(PP_MONTHLY::DRIVER, -1%), &
			WLDCRD_CARD$) = 0%
	END IF

	SELECT SORTBY$

	CASE "N"
		IF LAST_CUSNUM$ <> PP_MONTHLY::CUSNUM
		THEN
			WHEN ERROR IN
				GET #AR_35CUSTOM.CH%, &
					KEY #0% EQ PP_MONTHLY::CUSNUM, &
					REGARDLESS
			USE
				AR_35CUSTOM::CUSNAM = ""
			END WHEN

			IF PRINT_LINE%
			THEN
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 4%)
				GOSUB PrintCusTotal IF SHOW_SUBTOTALS$ <> "N"
			END IF

			TEXT$ = PP_MONTHLY::CUSNUM + " : " + &
				AR_35CUSTOM::CUSNAM

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
			GOTO ExitTotal IF UTL_REPORTX::STAT

			LAST_CUSNUM$ = PP_MONTHLY::CUSNUM
		END IF

	CASE "H"
		IF LAST_CUSNUM$ <> PP_MONTHLY::HOST
		THEN
			TEXT$ = PP_MONTHLY::HOST + " : "

			IF PRINT_LINE% AND SHOW_SUBTOTALS$ <> "N"
			THEN
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 4%)
				GOSUB PrintCusTotal
			END IF
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			GOTO ExitTotal IF UTL_REPORTX::STAT

			LAST_CUSNUM$ = PP_MONTHLY::HOST
		END IF

	END SELECT

	!
	! Print out one line
	!
	IF ONLY_TOTALS$ <> "Y"
	THEN
		TEXT$ = PP_MONTHLY::CUSNUM + " " + &
			PP_MONTHLY::VEHICLE + " " + &
			PP_MONTHLY::DRIVER + " " + &
			PRNT_DATE(PP_MONTHLY::TRANDATE, 6%) + " " + &
			PRNT_TIME(PP_MONTHLY::TRANTIME, 2048%) + " " + &
			PP_MONTHLY::HOST + "  " + &
			PP_MONTHLY::SITE + "   " + &
			PP_MONTHLY::STYPE + "    " + &
			PP_MONTHLY::TRNNUM + " " + &
			FORMAT$(PP_MONTHLY::STAXRATE, "##.##") + " " + &
			PP_MONTHLY::PRODUCT + " " + &
			PP_MONTHLY::UOM  + " " + &
			FORMAT$(PP_MONTHLY::QUANTITY, "####.###") + " " + &
			FORMAT$(PP_MONTHLY::ODOM, "#####.#") + "  " + &
			FORMAT$(PP_MONTHLY::SELLPRICE, "###.###") + " " + &
			FORMAT$(PP_MONTHLY::TRANCOST, "##.#####") + " " + &
			PP_MONTHLY::MISCKEYB

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT
	END IF

	!
	! Get the Transaction Type Description
	!
	PP_TRANTYPE::DESCRIPTION = STRING$(LEN(PP_TRANTYPE::DESCRIPTION), &
		A"?"B)

17100	WHEN ERROR IN
		GET #PP_TRANTYPE.CH%, KEY #0% EQ PP_MONTHLY::TRNTYPE, REGARDLESS
	USE
		CONTINUE PrintSecondLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PP_TRANTYPE"
		CONTINUE HelpError
	END WHEN

 PrintSecondLine:
	IF ONLY_TOTALS$ <> "Y"
	THEN
		TEXT$ = SPACE$(14%) + &
			PRNT_DATE(PP_MONTHLY::CAPDATE, 6%) + " " + &
			PRNT_TIME(PP_MONTHLY::CAPTIME, 2048%) + " " + &
			PRNT_DATE(PP_MONTHLY::ICBDATE, 6%) + " " + &
			PP_MONTHLY::TRANSOURCE + "        " + &
			PP_MONTHLY::EDITACT + "     " + &
			PP_MONTHLY::JULIANDAY + "   " + &
			PP_MONTHLY::RSTATION + "        " + &
			FORMAT$(PP_MONTHLY::DISCOUNT, "'LLL  ") + "   " + &
			PP_TRANTYPE::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT
	END IF

	PRINT_LINE% = -1%

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_MONTHLY::SLTYPE + "  "
	GOSUB AddSubtotal
	GOSUB AddCusSubtotal IF SHOW_SUBTOTALS$ <> "N"

	GOSUB AddTaxTotal
	GOSUB AddCusTaxTotal IF SHOW_SUBTOTALS$ <> "N"

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal
 !	GOSUB AddCusSubtotal

	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Calculate subtotals
	!*******************************************************************
 AddSubtotal:
	FOR I% = 1% TO SUBTOTAL%
		GOTO AddSubtotal1 &
			IF (SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$) AND &
			(SUBTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT)

		GOTO AddSubtotal2 &
			IF (SUBTOTAL(I%)::BUYFRAN + SUBTOTAL(I%)::PRODUCT) > &
			SUBBUYFRAN$ +  PP_MONTHLY::PRODUCT
	NEXT I%
	I% = SUBTOTAL% + 1%

 AddSubtotal2:
	!
	! Insert new subtotal at I%
	!
	SUBTOTAL(J% + 1%) = SUBTOTAL(J%) &
		FOR J% = SUBTOTAL% TO I% STEP -1%
	SUBTOTAL% = SUBTOTAL% + 1%
	SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$
	SUBTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT
	SUBTOTAL(I%)::COUNTER = 0%
	SUBTOTAL(I%)::QUANTITY = 0.0
	SUBTOTAL(I%)::PRICE = 0.0

 AddSubTotal1:
	!
	! Increment counts
	!
	SUBTOTAL(I%)::COUNTER = SUBTOTAL(I%)::COUNTER + 1%
	SUBTOTAL(I%)::QUANTITY = SUBTOTAL(I%)::QUANTITY + PP_MONTHLY::QUANTITY
	SUBTOTAL(I%)::PRICE = SUBTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_MONTHLY::SELLPRICE * PP_MONTHLY::QUANTITY, 2%)

	RETURN

	!
	! Calculate subtotals
	!
 AddTaxtotal:
	IF (PP_MONTHLY::HOST <> PP_SITE::HOST) OR &
		(PP_MONTHLY::SITE <> PP_SITE::SITE) OR &
		(PP_MONTHLY::STYPE <> PP_SITE::STYPE)
	THEN
		WHEN ERROR IN
			GET #PP_SITE.CH%, &
				KEY #0% EQ PP_MONTHLY::HOST + PP_MONTHLY::SITE + &
				PP_MONTHLY::STYPE, &
				REGARDLESS
		USE
			!
			! If we can"t read it, assume we don't want it
			!
			PP_SITE::HOST = PP_MONTHLY::HOST
			PP_SITE::SITE = PP_MONTHLY::SITE
			PP_SITE::STYPE = PP_MONTHLY::STYPE
			PP_SITE::STATE = "??"

			CONTINUE
		END WHEN
	END IF

	FOR I% = 1% TO TAXTOTAL%
		GOTO AddTaxtotal1 &
			IF (TAXTOTAL(I%)::STATE = PP_SITE::STATE) AND &
			(TAXTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT)

		GOTO AddTaxtotal2 &
			IF TAXTOTAL(I%)::STATE + TAXTOTAL(I%)::PRODUCT > &
				PP_SITE::STATE + PP_MONTHLY::PRODUCT
	NEXT I%
	I% = TAXTOTAL% + 1%

 AddTaxtotal2:
	!
	! Insert new subtotal at I%
	!
	TAXTOTAL(J% + 1%) = TAXTOTAL(J%) &
		FOR J% = TAXTOTAL% TO I% STEP -1%
	TAXTOTAL% = TAXTOTAL% + 1%
	TAXTOTAL(I%)::STATE = PP_SITE::STATE
	TAXTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT
	TAXTOTAL(I%)::COUNTER = 0%
	TAXTOTAL(I%)::QUANTITY = 0.0
	TAXTOTAL(I%)::PRICE = 0.0

 AddTaxTotal1:
	!
	! Increment counts
	!
	TAXTOTAL(I%)::COUNTER = TAXTOTAL(I%)::COUNTER + 1%
	TAXTOTAL(I%)::QUANTITY = TAXTOTAL(I%)::QUANTITY + PP_MONTHLY::QUANTITY
	TAXTOTAL(I%)::PRICE = TAXTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_MONTHLY::STAXRATE * PP_MONTHLY::QUANTITY, 2%)

	RETURN

	!*******************************************************************
	! Calculate subtotals
	!*******************************************************************
 AddCusSubtotal:
	FOR I% = 1% TO CUSSUBTOTAL%
		GOTO AddCusSubtotal1 &
			IF (CUSSUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$) AND &
			(CUSSUBTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT)

		GOTO AddCusSubtotal2 &
			IF (CUSSUBTOTAL(I%)::BUYFRAN + CUSSUBTOTAL(I%)::PRODUCT) > &
			SUBBUYFRAN$ +  PP_MONTHLY::PRODUCT
	NEXT I%
	I% = CUSSUBTOTAL% + 1%

 AddCusSubtotal2:
	!
	! Insert new subtotal at I%
	!
	CUSSUBTOTAL(J% + 1%) = CUSSUBTOTAL(J%) &
		FOR J% = CUSSUBTOTAL% TO I% STEP -1%
	CUSSUBTOTAL% = CUSSUBTOTAL% + 1%
	CUSSUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$
	CUSSUBTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT
	CUSSUBTOTAL(I%)::COUNTER = 0%
	CUSSUBTOTAL(I%)::QUANTITY = 0.0
	CUSSUBTOTAL(I%)::PRICE = 0.0

 AddCusSubTotal1:
	!
	! Increment counts
	!
	CUSSUBTOTAL(I%)::COUNTER = CUSSUBTOTAL(I%)::COUNTER + 1%
	CUSSUBTOTAL(I%)::QUANTITY = &
		CUSSUBTOTAL(I%)::QUANTITY + PP_MONTHLY::QUANTITY
	CUSSUBTOTAL(I%)::PRICE = CUSSUBTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_MONTHLY::SELLPRICE * PP_MONTHLY::QUANTITY, 2%)

	RETURN

	!
	! Calculate subtotals
	!
 AddCusTaxtotal:
	IF (PP_MONTHLY::HOST <> PP_SITE::HOST) OR &
		(PP_MONTHLY::SITE <> PP_SITE::SITE) OR &
		(PP_MONTHLY::STYPE <> PP_SITE::STYPE)
	THEN
		WHEN ERROR IN
			GET #PP_SITE.CH%, &
				KEY #0% EQ PP_MONTHLY::HOST + PP_MONTHLY::SITE + &
				PP_MONTHLY::STYPE, &
				REGARDLESS
		USE
			!
			! If we can"t read it, assume we don't want it
			!
			PP_SITE::HOST = PP_MONTHLY::HOST
			PP_SITE::SITE = PP_MONTHLY::SITE
			PP_SITE::STYPE = PP_MONTHLY::STYPE
			PP_SITE::STATE = "??"

			CONTINUE
		END WHEN
	END IF

	FOR I% = 1% TO CUSTAXTOTAL%
		GOTO AddCusTaxtotal1 &
			IF (CUSTAXTOTAL(I%)::STATE = PP_SITE::STATE) AND &
			(CUSTAXTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT)

		GOTO AddCusTaxtotal2 &
			IF CUSTAXTOTAL(I%)::STATE + CUSTAXTOTAL(I%)::PRODUCT > &
				PP_SITE::STATE + PP_MONTHLY::PRODUCT
	NEXT I%
	I% = CUSTAXTOTAL% + 1%

 AddCusTaxtotal2:
	!
	! Insert new subtotal at I%
	!
	CUSTAXTOTAL(J% + 1%) = CUSTAXTOTAL(J%) &
		FOR J% = CUSTAXTOTAL% TO I% STEP -1%
	CUSTAXTOTAL% = CUSTAXTOTAL% + 1%
	CUSTAXTOTAL(I%)::STATE = PP_SITE::STATE
	CUSTAXTOTAL(I%)::PRODUCT = PP_MONTHLY::PRODUCT
	CUSTAXTOTAL(I%)::COUNTER = 0%
	CUSTAXTOTAL(I%)::QUANTITY = 0.0
	CUSTAXTOTAL(I%)::PRICE = 0.0

 AddCusTaxTotal1:
	!
	! Increment counts
	!
	CUSTAXTOTAL(I%)::COUNTER = CUSTAXTOTAL(I%)::COUNTER + 1%
	CUSTAXTOTAL(I%)::QUANTITY = &
		CUSTAXTOTAL(I%)::QUANTITY + PP_MONTHLY::QUANTITY
	CUSTAXTOTAL(I%)::PRICE = CUSTAXTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_MONTHLY::STAXRATE * PP_MONTHLY::QUANTITY, 2%)

	RETURN

	%PAGE

 ExitTotal:

	!
	! Dump out subtotals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	TEXT$ = "   " + &
		"Product       " + &
		"       Count " + &
		"      Quantity " + &
		"       Dollars"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	XTOTAL$ = "~~~~~~~~"
	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO SUBTOTAL%

		IF SUBTOTAL(I%)::BUYFRAN <> XTOTAL$
		THEN
			IF COUNTER%
			THEN
				TEXT$ = "      Total       " + &
					FORMAT$(COUNTER%, "###,###,### ") + &
					FORMAT$(QUANTITY, "###,###,###.## ") + &
					FORMAT$(PRICE, "###,###,###.##")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

				COUNTER% = 0%
				QUANTITY = 0.0
				PRICE = 0.0
			END IF

			SELECT SUBTOTAL(I%)::BUYFRAN
			CASE "~~~"
				TEXT$ = "Total of all Locations"
			CASE "F"
				TEXT$ = "Total of Foreign Sales (F)"
			CASE "P"
				TEXT$ = "Total of Foreign Purchases(P)"
			CASE ELSE
				TEXT$ = "Total of Local Sales (" + &
					SUBTOTAL(I%)::BUYFRAN + ")"
			END SELECT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			XTOTAL$ = SUBTOTAL(I%)::BUYFRAN
		END IF

		TEXT$ = "   " + &
			SUBTOTAL(I%)::PRODUCT + &
			FORMAT$(SUBTOTAL(I%)::COUNTER, " ###,###,### ") + &
			FORMAT$(SUBTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(SUBTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + SUBTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + SUBTOTAL(I%)::QUANTITY
		PRICE = PRICE + SUBTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Total       " + &
			FORMAT$(COUNTER%, "###,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! Sales Tax Totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Sales Tax Totals", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO TAXTOTAL%

		TEXT$ = "   " + &
			TAXTOTAL(I%)::STATE + " " + &
			TAXTOTAL(I%)::PRODUCT + &
			FORMAT$(TAXTOTAL(I%)::COUNTER, "##,###,### ") + &
			FORMAT$(TAXTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(TAXTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + TAXTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + TAXTOTAL(I%)::QUANTITY
		PRICE = PRICE + TAXTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Salestax Total" + &
			FORMAT$(COUNTER%, "##,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF


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

 PrintCustotal:
	!***************************************************************
	! Print customer totals
	!***************************************************************

	!
	! Dump out subtotals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	TEXT$ = "   " + &
		"Product       " + &
		"       Count " + &
		"      Quantity " + &
		"       Dollars"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	XTOTAL$ = "~~~~~~~~"
	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO CUSSUBTOTAL%

		IF CUSSUBTOTAL(I%)::BUYFRAN <> XTOTAL$
		THEN
			IF COUNTER%
			THEN
				TEXT$ = "      Total       " + &
					FORMAT$(COUNTER%, "###,###,### ") + &
					FORMAT$(QUANTITY, "###,###,###.## ") + &
					FORMAT$(PRICE, "###,###,###.##")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

				COUNTER% = 0%
				QUANTITY = 0.0
				PRICE = 0.0
			END IF

			SELECT CUSSUBTOTAL(I%)::BUYFRAN
			CASE "~~~"
				TEXT$ = "Total of all Locations"
			CASE "F"
				TEXT$ = "Total of Foreign Sales (F)"
			CASE "P"
				TEXT$ = "Total of Foreign Purchases(P)"
			CASE ELSE
				TEXT$ = "Total of Local Sales (" + &
					SUBTOTAL(I%)::BUYFRAN + ")"
			END SELECT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			XTOTAL$ = CUSSUBTOTAL(I%)::BUYFRAN

		END IF

		TEXT$ = "   " + &
			CUSSUBTOTAL(I%)::PRODUCT + &
			FORMAT$(CUSSUBTOTAL(I%)::COUNTER, " ###,###,### ") + &
			FORMAT$(CUSSUBTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(CUSSUBTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + CUSSUBTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + CUSSUBTOTAL(I%)::QUANTITY
		PRICE = PRICE + CUSSUBTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Total       " + &
			FORMAT$(COUNTER%, "###,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! Sales Tax Totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Sales Tax Totals", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO CUSTAXTOTAL%

		TEXT$ = "   " + &
			CUSTAXTOTAL(I%)::STATE + " " + &
			CUSTAXTOTAL(I%)::PRODUCT + &
			FORMAT$(CUSTAXTOTAL(I%)::COUNTER, "##,###,### ") + &
			FORMAT$(CUSTAXTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(CUSTAXTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + CUSTAXTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + CUSTAXTOTAL(I%)::QUANTITY
		PRICE = PRICE + CUSTAXTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Salestax Total" + &
			FORMAT$(COUNTER%, "##,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	CUSSUBTOTAL% = 0%
	CUSTAXTOTAL% = 0%

	RETURN

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
