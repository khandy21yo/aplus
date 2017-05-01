1	%TITLE "Print Daily Transaction Journal"
	%SBTTL "PP_RPRT_LEDGER_CITY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
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
	!	$ BAS PP_SOURCE:PP_RPRT_LEDGER_CITY/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_LEDGER_CITY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_LEDGER_CITY.OBJ;*
	!
	! Author:
	!
	!	09/15/2004 - Kevin Handy
	!		Taken from PP_RPRT_LEDGER.
	!		(Yes the name should be _STATE not _CITY, oops.)
	!
	! Modification History:
	!
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
	MAP (PP_MONTHLY)		PP_MONTHLY_CDD		PP_MONTHLY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	RECORD SUBTOTAL_CDD
		STRING	BUYFRAN = 3%	! Location
		STRING	PRODUCT = 14%	! Product
		LONG	COUNTER		! Number of lines
		REAL	QUANTITY	! Number of gallons
		REAL	PRICE		! Total paid
	END RECORD

	DIM SUBTOTAL_CDD SUBTOTAL(300%)

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

	ONE_STATE$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 2%)

	!++
	! Abstract:FLD07
	!	^*(07) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field selects
	!	designated site States to be printed.
	!	.b
	!	For information on "Wildcarding" techniques refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	ONE_PRODUCT$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 14%)

	!++
	! Abstract:FLD08
	!	^*(08) Product\*
	!	.b
	!	.lm +5
	!	The ^*State\* field selects
	!	designated site States to be printed.
	!	.b
	!	For information on "Wildcarding" techniques refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	ONLY_TAX$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Only Taxable\*
	!	.b
	!	.lm +5
	!	The ^*Only Taxable\* field determines
	!	weither all or just taxable items should be printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 1%)

	!++
	! Abstract:FLD10
	!	^*(10) Totals Only\*
	!	.b
	!	.lm +5
	!	The ^*Totals Only\* field determines
	!	if detail entries will be printed.
	!	.lm -5
	!
	! Index:
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
	! Open site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"
	USE
		FILENAME$ = "PP_CARDEXEMPT"
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
		TITLE$(1%) = "STATE LIST BY CUSTOMER NUMBER " + ONE_PRODUCT$

	CASE "H"
		K_NUM% = 1%
		TITLE$(1%) = "STATE LIST BY HOST " + ONE_PRODUCT$

	END SELECT

	TITLE$(2%) = "Period. " + YYYY_PP$ + &
		" State " + ONE_STATE$ + &
		" Product " + ONE_PRODUCT$

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
	CUS_QUAN = 0.0
	CUS_TRAN = 0.0

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
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

		IF (PP_MONTHLY::CUSNUM <> LAST_CUSNUM$)
		THEN
			GOSUB PrintCusTotal
			LAST_CUSNUM$ = PP_MONTHLY::CUSNUM
		END IF

	CASE "H"
		GOTO ExitTotal &
			IF (PP_MONTHLY::HOST > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING( &
			EDIT$(PP_MONTHLY::HOST, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF (PP_MONTHLY::HOST <> LAST_HOST$)
		THEN
			GOSUB PrintCusTotal
			LAST_HOST$ = PP_MONTHLY::HOST
		END IF

	END SELECT

	IF (WLDCRD_CARD$ <> "") AND (WLDCRD_CARD$ <> "*")
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PP_MONTHLY::VEHICLE, -1%), &
			WLDCRD_CARD$) = 0% AND &
			COMP_STRING(EDIT$(PP_MONTHLY::DRIVER, -1%), &
			WLDCRD_CARD$) = 0%
	END IF

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
			CONTINUE GetNextRec
		END WHEN
	END IF

	IF PP_SITE::STATE <> ONE_STATE$
	THEN
		GOTO GetNextRec UNLESS ONE_STATE$ = ""
	END IF

	IF (ONE_PRODUCT$ <> PP_MONTHLY::PRODUCT)
	THEN
		GOTO GetNextRec UNLESS ONE_PRODUCT$ = ""
	END IF

	!
	! Is this card exempt?
	!
	PP_CARDEXEMPT::CUSNUM = PP_MONTHLY::CUSNUM
	PP_CARDEXEMPT::CARD = PP_MONTHLY::VEHICLE
	PP_CARDEXEMPT::PRODUCT = PP_MONTHLY::PRODUCT
	PP_CARDEXEMPT::STATE = PP_MONTHLY::STATE
	EXEMPT_FLAG$ = "**"

	IF PP_MONTHLY::DRIVER <> "0000000 "
	THEN
		WHEN ERROR IN
			GET #PP_CARDEXEMPT.CH%, &
				KEY #0% EQ PP_MONTHLY::DRIVER + &
				PP_MONTHLY::PRODUCT + &
				PP_MONTHLY::STATE, &
				REGARDLESS
			EXEMPT_FLAG$ = ""
		USE
			PP_CARDEXEMPT::CARD = PP_MONTHLY::DRIVER
			CONTINUE
		END WHEN
	END IF

	IF EXEMPT_FLAG$ <> "" AND &
		PP_MONTHLY::VEHICLE <> "0000000 "
	THEN
		WHEN ERROR IN
			GET #PP_CARDEXEMPT.CH%, &
				KEY #0% EQ PP_MONTHLY::VEHICLE + &
				PP_MONTHLY::PRODUCT + &
				PP_MONTHLY::STATE, &
				REGARDLESS
			EXEMPT_FLAG$ = ""
		USE
			PP_CARDEXEMPT::CARD = PP_MONTHLY::VEHICLE
			CONTINUE
		END WHEN
	END IF

	LAST_EXEMPT$ = PP_CARDEXEMPT::CUSNUM + &
		PP_CARDEXEMPT::CARD + &
		PP_CARDEXEMPT::PRODUCT + &
		PP_CARDEXEMPT::STATE

	!
	! Skip items if non-taxable?
	!
	IF (ONLY_TAX$ = "Y" AND EXEMPT_FLAG$ <> "")
	THEN
		GOTO GetNextRec
	END IF

	!
	! Print out one line
	!
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

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
		IF PRINT_LINE%
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CUS_QUAN = CUS_QUAN + PP_MONTHLY::QUANTITY
	CUS_TRAN = CUS_TRAN + PP_MONTHLY::TRANCOST
	CUS_COUNT% = CUS_COUNT% + 1%

	!
	! Get the Transaction Type Description
	!
	PP_TRANTYPE::DESCRIPTION = STRING$(LEN(PP_TRANTYPE::DESCRIPTION), &
		A"?"B)

17100	WHEN ERROR IN
		GET #PP_TRANTYPE.CH%, &
			KEY #0% EQ PP_MONTHLY::TRNTYPE, &
			REGARDLESS
	USE
		CONTINUE PrintSecondLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PP_TRANTYPE"
		CONTINUE HelpError
	END WHEN

 PrintSecondLine:
	TEXT$ = SPACE$(14%) + &
		PRNT_DATE(PP_MONTHLY::CAPDATE, 6%) + " " + &
		PRNT_TIME(PP_MONTHLY::CAPTIME, 2048%) + " " + &
		PRNT_DATE(PP_MONTHLY::ICBDATE, 6%) + " " + &
		PP_MONTHLY::TRANSOURCE + "        " + &
		PP_MONTHLY::EDITACT + "     " + &
		PP_MONTHLY::JULIANDAY + "   " + &
		PP_MONTHLY::RSTATION + "        " + &
		FORMAT$(PP_MONTHLY::DISCOUNT, "'LLL  ") + "   " + &
		PP_TRANTYPE::DESCRIPTION + " " + &
		EXEMPT_FLAG$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_LINE% = -1%

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_MONTHLY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal

	GOTO GetNextRec

	%PAGE

	!
	! Calculate subtotals
	!
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

	%PAGE

 PrintCusTotal:
	IF CUS_COUNT% <> 0%
	THEN
		TEXT$ = "          " + " " + &
			"TOTAL   " + " " + &
			"        " + " " + &
			"        " + " " + &
			"     " + " " + &
			"    " + "  " + &
			"    " + "   " + &
			" " + "    " + &
			"     " + " " + &
			"     " + " " + &
			"             " + " " + &
			""  + "" + &
			FORMAT$(CUS_QUAN, "########.###") + " " + &
			"       " + "  " + &
			"   " + " " + &
			FORMAT$(CUS_TRAN, "######.#####")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
			IF PRINT_LINE%
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CUS_COUNT% = 0%
	CUS_QUAN = 0.0
	CUS_TRAN = 0.0

	RETURN

 ExitTotal:

	GOSUB PrintCusTotal

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
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
				GOTO ExitProgram IF UTL_REPORTX::STAT

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
			GOTO ExitProgram IF UTL_REPORTX::STAT
			XTOTAL$ = SUBTOTAL(I%)::BUYFRAN
		END IF

		TEXT$ = "   " + &
			SUBTOTAL(I%)::PRODUCT + &
			FORMAT$(SUBTOTAL(I%)::COUNTER, " ###,###,### ") + &
			FORMAT$(SUBTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(SUBTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

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
		GOTO ExitProgram IF UTL_REPORTX::STAT
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
