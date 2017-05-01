1	%TITLE "Print Daily Transaction Journal"
	%SBTTL "PP_RPRT_DAILY"
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
	!	$ BAS PP_SOURCE:PP_RPRT_DAILY/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_DAILY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_DAILY.OBJ;*
	!
	! Author:
	!
	!	01/07/93 - Dan Perkins
	!
	! Modification History:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/04/93 - Kevin Handy
	!		Changed PP_DAILY::DISCOUNT to a string.
	!
	!	04/07/93 - Kevin Handy
	!		Added subtotals and totals
	!
	!	04/13/93 - Kevin Handy
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
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.HB"
	MAP (PP_DAILY)		PP_DAILY_CDD		PP_DAILY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

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

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))

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

300	!
	! Open Daily Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.OPN"
	USE
		FILENAME$ = "PP_DAILY"
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

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "DAILY TRANSACTION JOURNAL REPORT " + &
			"BY CUSTOMER NUMBER"

	CASE "H"
		K_NUM% = 1%
		TITLE$(1%) = "DAILY TRANSACTION JOURNAL REPORT " + &
			"BY HOST"

	END SELECT

	TITLE$(2%) = "BATCH NO. " + PRNT_DATE(BATCH_NO$, 8%)
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

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PP_DAILY.CH%, KEY #K_NUM%
		ELSE
			FIND #PP_DAILY.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PP_DAILY"
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
		GET #PP_DAILY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PP_DAILY"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (PP_DAILY::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING( &
			EDIT$(PP_DAILY::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "H"
		GOTO ExitTotal IF (PP_DAILY::HOST > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING( &
			EDIT$(PP_DAILY::HOST, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = PP_DAILY::CUSNUM + " " + &
		PP_DAILY::VEHICLE + " " + &
		PP_DAILY::DRIVER + " " + &
		PRNT_DATE(PP_DAILY::TRANDATE, 6%) + " " + &
		PRNT_TIME(PP_DAILY::TRANTIME, 2048%) + " " + &
		PP_DAILY::HOST + "  " + &
		PP_DAILY::SITE + "   " + &
		PP_DAILY::STYPE + "    " + &
		PP_DAILY::TRNNUM + " " + &
		FORMAT$(PP_DAILY::STAXRATE, "##.##") + " " + &
		PP_DAILY::PRODUCT + " " + &
		PP_DAILY::UOM + " " + &
		FORMAT$(PP_DAILY::QUANTITY, "####.###") + " " + &
		FORMAT$(PP_DAILY::ODOM, "#####.#") + "  " + &
		FORMAT$(PP_DAILY::SELLPRICE, "###.###") + " " + &
		FORMAT$(PP_DAILY::TRANCOST, "##.#####") + " " + &
		PP_DAILY::MISCKEYB

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF PRINT_LINE%
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get the Transaction Type Description
	!
17100	WHEN ERROR IN
		GET #PP_TRANTYPE.CH%, KEY #0% EQ PP_DAILY::TRNTYPE
	USE
		PP_TRANTYPE::DESCRIPTION = &
			STRING$(LEN(PP_TRANTYPE::DESCRIPTION), A"?"B)

		CONTINUE PrintSecondLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PP_TRANTYPE"
		CONTINUE HelpError
	END WHEN

 PrintSecondLine:
	TEXT$ = SPACE$(14%) + &
		PRNT_DATE(PP_DAILY::CAPDATE, 6%) + " " + &
		PRNT_TIME(PP_DAILY::CAPTIME, 2048%) + " " + &
		PRNT_DATE(PP_DAILY::ICBDATE, 6%) + " " + &
		PP_DAILY::TRANSOURCE + "        " + &
		PP_DAILY::EDITACT + "     " + &
		PP_DAILY::JULIANDAY + "   " + &
		PP_DAILY::RSTATION + "        " + &
		FORMAT$(PP_DAILY::DISCOUNT, "'LLL  ") + "   " + &
		PP_TRANTYPE::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	PRINT_LINE% = -1%

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
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
			(SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT)

		GOTO AddSubtotal2 &
			IF (SUBTOTAL(I%)::BUYFRAN + SUBTOTAL(I%)::PRODUCT) > &
			SUBBUYFRAN$ +  PP_DAILY::PRODUCT
	NEXT I%
	I% = SUBTOTAL% + 1%

 AddSubtotal2:
	!
	! Insert new subtotal at I%
	!
	SUBTOTAL(J%+1%) = SUBTOTAL(J%) FOR J% = SUBTOTAL% TO I% STEP -1%
	SUBTOTAL% = SUBTOTAL% + 1%
	SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$
	SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT
	SUBTOTAL(I%)::COUNTER = 0%
	SUBTOTAL(I%)::QUANTITY = 0.0
	SUBTOTAL(I%)::PRICE = 0.0

 AddSubTotal1:
	!
	! Increment counts
	!
	SUBTOTAL(I%)::COUNTER = SUBTOTAL(I%)::COUNTER + 1%
	SUBTOTAL(I%)::QUANTITY = SUBTOTAL(I%)::QUANTITY + PP_DAILY::QUANTITY
	SUBTOTAL(I%)::PRICE = SUBTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_DAILY::SELLPRICE * PP_DAILY::QUANTITY, 2%)

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
