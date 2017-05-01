1	%TITLE "Transaction Summary Between Dates"
	%SBTTL "SA_RPRT_COMMISSION"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
	!
	! Sofoftware Solutions, Inc.
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
	! Software Solutions
	!
	! Software Solutions assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions.
	!
	!++
	! ID:0017
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report will display the total transactions that occured
	!	between two dates in the Transaction Journal.
	!	.lm -5
	!
	! Index:
	!	.x Report>Transaction Summary
	!	.x Transaction Summary>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_COMMISSION/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_COMMISSION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_COMMISSION.OBJ;*
	!
	! Author:
	!
	!	06/29/2004 - Kevin Handy
	!		Based on SA_RPRT_TRANSUM
	!
	! Modification History:
	!
	!	07/08/2004 - Kevin Handy
	!		Drop cost column, break down better.
	!
	!	12/14/2004 - Kevin Handy
	!		Change 'LOCATION' field to 'WILDCARD LOCATION'.
	!
	!	04/04/2006 - Kevin Handy
	!		Match sign of cost/price to that of qty.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	EXTERNAL REAL   FUNCTION PC_READ_PRICE

	!
	! Include cdd
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	RECORD TEMP_RECORD
		STRING	VENPROD = 14%
		STRING	PRODUCT = 14%
		REAL	QUAN
		REAL	COST
		REAL	PRICE
	END RECORD

	MAP (TEMP_MAP) TEMP_RECORD TEMP_MAP

	!
	! Dimension statements
	!
	DIM IC_TRANSACTION_FILE$(300%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Start Period\*
	!	.p
	!	The ^*Period\* field enters the
	!	period to print.
	!
	! Index:
	!	.x Period
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) End Period\*
	!	.p
	!
	! Index:
	!	.x Period
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) From Date\*
	!	.b
	!	.lm +5
	!	Specified the date of the first transaction to include.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) To Date\*
	!	.b
	!	.lm +5
	!	Specifies the date of the last transaction to include.
	!	.lm -5
	!
	! Index:
	!
	!--

	TRAN_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Tran Type\*
	!	.b
	!	.lm +5
	!	Specifies the type of transaction to include.
	!	Wildcards are allowed.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06)Wildcard Location\*
	!	.b
	!	.lm +5
	!	Specifies the location(s) to generate the report for.
	!	.lm -5
	!
	! Index:
	!
	!--

 !	FROM_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(6%))
	WILD_CUSTOMER$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Wildcard Customer\*
	!	.b
	!	Uses wildcarding to select specific customers.
	!
	! Index:
	!
	!--

 !	TO_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*(08) To Product\*
	!	.b
	!	.lm +5
	!	Specifies the last product number to include.
	!	.lm -5
	!
	! Index:
	!
	!--

	COMMISSION_RATE = VAL(TRM$(UTL_REPORTX::OPTDEF(8%)))

	!++
	! Abstract:FLD09
	!	^*(09) Commission Rate\*
	!	.b
	!	.lm +5
	!	Specifies the commission rate to use.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRIORITY$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 1%)

	!++
	! Abstract:FLD10
	!	^*(10) Priority\*
	!	.b
	!	.lm +5
	!	Specifies which priority to report on.
	!	.lm -5
	!
	! Index:
	!
	!--

320	!
	! Open Product Description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

330	!
	! PO_PARTCROSS
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

340	!
	! AR_35CUSTOM
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN


	%PAGE

1000	!*******************************************************************
	! Generate File Sorted by Product
	!*******************************************************************

	!
	! Create work file
	!
	CALL ASSG_CHANNEL(TEMP.CH%, STAT%)

	OPEN "SA_TEMP.TMP" FOR OUTPUT AS FILE TEMP.CH%, &
		TEMPORARY, &
		ORGANIZATION INDEXED FIXED, &
		MAP TEMP_MAP, &
		PRIMARY KEY (TEMP_MAP::VENPROD, TEMP_MAP::PRODUCT), &
		ACCESS MODIFY, &
		ALLOW NONE

	!
	! Search for transaction files
	!
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), &
		16%, "", "")

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))

	IF IC_TRANSACTION_FILE%
	THEN
		IC_TRANSACTION_FILE$(LOOP%) = &
			MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%) &
				FOR LOOP% = 1% TO IC_TRANSACTION_FILE%
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find any inventory ledger file", 0%)
		GOTO ExitTotal
	END IF

	!
	! Initilize everything
	!
	THIS_PRODUCT$ = "123456789012345"	! Invalid, force start
	THIS_QTY = 0.0
	THIS_COST = 0.0
	THIS_PRICE = 0.0

	!
	! Scan through all transaction files
	!
2000	FOR LOOP% = IC_TRANSACTION_FILE% TO 1% STEP -1%

		!
		! Maybe open up the transaction file
		!
		YYYYPP$ = IC_TRANSACTION_FILE$(LOOP%)

		GOTO SkipLoop IF YYYYPP$ < FROM_PERIOD$
		GOTO SkipLoop IF YYYYPP$ > TO_PERIOD$

		CLOSE IC_TRANSACTION.CH%

		CALL ENTR_3MESSAGE(SCOPE, "Processing " + YYYYPP$, 1%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
		USE
			CONTINUE 2200
		END WHEN

2010		WHEN ERROR IN
			RESET #IC_TRANSACTION.CH%, KEY #0%
		USE
			CONTINUE 2200
		END WHEN

2100		WHEN ERROR IN
			GET #IC_TRANSACTION.CH%
		USE
			CONTINUE 2200 IF ERR = 11%
			CONTINUE HelpError
		END WHEN

 !		GOTO 2200 IF (TO_ITEM$ <> "") AND &
 !			(IC_TRANSACTION::PRODUCT > TO_ITEM$)

 !if IC_TRANSACTION::PRODUCT = "JRC652-5044"
 !then
 !print "Test 1 "; &
 !IC_TRANSACTION::PRODUCT, IC_TRANSACTION::QUANTITY_A, &
 !IC_TRANSACTION::COST, IC_TRANSACTION::PRICE
 !end if

		IF WILD_CUSTOMER$ <> ""
		THEN
			GOTO 2100 &
				IF COMP_STRING(IC_TRANSACTION::CROSS_REF, &
				WILD_CUSTOMER$) = 0%
		END IF

		IF LOCATION$ <> ""
		THEN
			GOTO 2100 &
				IF COMP_STRING(IC_TRANSACTION::LOCATION, &
				LOCATION$) = 0%
		END IF
 ! if IC_TRANSACTION::PRODUCT = "JRC652-5044"
 !then
 !print "Test 2 "; IC_TRANSACTION::PRODUCT
 !end if

		GOTO 2100 &
			IF IC_TRANSACTION::TRANS_DATE < FROM_DATE$
		GOTO 2100 &
			IF IC_TRANSACTION::TRANS_DATE > TO_DATE$

		GOSUB DoSummary &
			IF IC_TRANSACTION::PRODUCT <> THIS_PRODUCT$

 !if IC_TRANSACTION::PRODUCT = "JRC652-5044"
 !then
 !print "Test 3 "; IC_TRANSACTION::PRODUCT
 !end if
 !		IF COMP_STRING(IC_TRANSACTION::TYPE_A, TRAN_TYPE$) AND &
 !			IC_TRANSACTION::PRICE <> 0.0
		IF COMP_STRING(IC_TRANSACTION::TYPE_A, TRAN_TYPE$)
		THEN
			THIS_QTY = THIS_QTY + IC_TRANSACTION::QUANTITY_A
			THIS_COST = THIS_COST + &
				ABS(IC_TRANSACTION::COST) * &
				SGN(IC_TRANSACTION::QUANTITY_A)

			!
			! We may have to come up with a price ourself
			!
			IF (IC_TRANSACTION::PRICE = 0.0)
			THEN
				GOSUB CalcPrice

				THIS_PRICE = THIS_PRICE + &
					FUNC_ROUND(CALC_PRICE * &
					IC_TRANSACTION::QUANTITY_A, 2%)
			ELSE
				THIS_PRICE = THIS_PRICE + &
					ABS(IC_TRANSACTION::PRICE) * &
					SGN(IC_TRANSACTION::QUANTITY_A)
			END IF
 !if IC_TRANSACTION::PRODUCT = "JRC652-5044"
 !then
 !print "Test 4 "; &
 !IC_TRANSACTION::PRODUCT, IC_TRANSACTION::QUANTITY_A, &
 !IC_TRANSACTION::COST, IC_TRANSACTION::PRICE; "--"; &
 !THIS_QTY ; THIS_COST ; THIS_PRICE
 !end if

		END IF

 !		IF COMP_STRING(IC_TRANSACTION::TYPE_B, TRAN_TYPE$) AND &
 !			IC_TRANSACTION::PRICE <> 0.0
		IF COMP_STRING(IC_TRANSACTION::TYPE_B, TRAN_TYPE$) AND &
			IC_TRANSACTION::PRICE <> 0.0
		THEN
			THIS_QTY = THIS_QTY + IC_TRANSACTION::QUANTITY_B

			!
			! Adjust Cost (A) to Cost(B)
			!
			THIS_COST = THIS_COST + &
				ABS(FUNC_ROUND((IC_TRANSACTION::COST / &
				IC_TRANSACTION::QUANTITY_A) * &
				IC_TRANSACTION::QUANTITY_B, 2%)) * &
				SGN(IC_TRANSACTION::QUANTITY_B)

			!
			! We may have to come up with a price ourself
			!
			IF (IC_TRANSACTION::PRICE = 0.0)
			THEN
				GOSUB CalcPrice

				THIS_PRICE = THIS_PRICE + &
					FUNC_ROUND(CALC_PRICE * &
					IC_TRANSACTION::QUANTITY_B, 2%)
			ELSE
				THIS_PRICE = THIS_PRICE + &
					ABS(FUNC_ROUND((IC_TRANSACTION::PRICE / &
					IC_TRANSACTION::QUANTITY_A) * &
					IC_TRANSACTION::QUANTITY_B, 2%)) * &
					SGN(IC_TRANSACTION::QUANTITY_B)
			END IF
		END IF

		GOTO 2100

 SkipLoop:
2200	NEXT LOOP%

	GOSUB DoSummary
	CLOSE IC_TRANSACTION.CH%

	GOTO ReportTitle

	!
	! Summarize information into temp file
	!
 DoSummary:
3000	GOTO 3090 IF THIS_QTY = 0.0 AND THIS_COST = 0.0 AND THIS_PRICE = 0.0

	!
	! Get vendor product, skip out if none found
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #0% EQ THIS_PRODUCT$ + PRIORITY$, &
			REGARDLESS
	USE
		CONTINUE 3090
	END WHEN

	WHEN ERROR IN
		GET #TEMP.CH%, &
			KEY #0% EQ PO_PARTCROSS::VENPROD + THIS_PRODUCT$
	USE
		CONTINUE 3050
	END WHEN

	TEMP_MAP::QUAN = TEMP_MAP::QUAN + THIS_QTY
	TEMP_MAP::COST = TEMP_MAP::COST + THIS_COST
	TEMP_MAP::PRICE = TEMP_MAP::PRICE + THIS_PRICE

	UPDATE #TEMP.CH%

	GOTO 3090

3050	TEMP_MAP::PRODUCT = THIS_PRODUCT$
	TEMP_MAP::VENPROD = PO_PARTCROSS::VENPROD

	TEMP_MAP::QUAN = THIS_QTY
	TEMP_MAP::COST = THIS_COST
	TEMP_MAP::PRICE = THIS_PRICE

	PUT #TEMP.CH%

 !if THIS_PRODUCT$ = "JRC652-5044"
 !then
 !print "Test 5 "; &
 !TEMP_MAP::PRODUCT; " "; TEMP_MAP::VENPROD; " "; &
 !THIS_QTY; THIS_COST; THIS_PRICE
 !end if

3090	THIS_PRODUCT$ = IC_TRANSACTION::PRODUCT
	THIS_QTY = 0.0
	THIS_COST = 0.0
	THIS_PRICE = 0.0

	RETURN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Summary Of Transaction Journals"
	TITLE$(2%) = "For period " + FROM_PERIOD$ + " To " + TO_PERIOD$
	TITLE$(3%) = "From date " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(4%) = "Location: " + LOCATION$
	TITLE$(4%) = TITLE$(4%) + " Customer: " + WILDCARD_CUSTOMER$ &
		IF WILDCARD_CUSTOMER$ <> ""
	TITLE$(5%) = "Sales Analysis System"
	TITLE$(6%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "VendorProduct  ProductNumber  Description              " + &
		"             Qty       Price  Commission"
	TITLE$(7%) = "."
	TITLE$(8%) = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	!
	! If from ITEM blank then reset file
	! else try to find the first record
	!
	RESET #TEMP.CH%

	TOTAL_QTY = 0.0
	TOTAL_COST = 0.0
	TOTAL_PRICE = 0.0
	TOTAL_COMMISSION = 0.0

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #TEMP.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

17030	!
	! Get product description
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ TEMP_MAP::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = "** Undefined **"
		CONTINUE 17050
	END WHEN

17050	!
	! Get record from Product Description file
	!
	COMMISSION = &
		FUNC_ROUND(TEMP_MAP::PRICE * COMMISSION_RATE / 100.0, 2%)

	TEXT$ = TEMP_MAP::VENPROD + " " + &
		TEMP_MAP::PRODUCT + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 30%) + " " + &
		FORMAT$(TEMP_MAP::QUAN, "#######.##") + " " + &
		FORMAT$(TEMP_MAP::PRICE, "########.##") + " " + &
		FORMAT$(COMMISSION, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL_QTY = TOTAL_QTY + TEMP_MAP::QUAN
	TOTAL_COST = TOTAL_COST + TEMP_MAP::COST
	TOTAL_PRICE = TOTAL_PRICE + TEMP_MAP::PRICE
	TOTAL_COMMISSION = TOTAL_COMMISSION + COMMISSION

	GOTO GetNextRec

	%PAGE

	!
	! Try to calculate a reasonable price when none has been given
	!
 CalcPrice:
	CALC_PRICE = 0.0

	!
	! We need a customer to get their standard price type
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, &
			KEY #0% EQ IC_TRANSACTION::CROSS_REF, &
			REGARDLESS
	USE
		CONTINUE EndCalcPrice
	END WHEN

	!
	! Look up rate
	!
	CALC_PRICE = PC_READ_PRICE(IC_TRANSACTION::PRODUCT, &
		IC_TRANSACTION::LOCATION, &
		AR_35CUSTOM::TTYPE, &
		IC_TRANSACTION::TRANS_DATE, &
		"", &
		EFFDATE$, EFFTIME$)

 EndCalcPrice:
	RETURN

 ExitTotal:
	!
	! Handle end of the report
	!
	TEXT$ = "               " + &
		"               " + &
		"                   Grand Total " + &
		FORMAT$(TOTAL_QTY, "#######.##") + " " + &
		FORMAT$(TOTAL_PRICE, "########.##") + " " + &
		FORMAT$(TOTAL_COMMISSION, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
	!
	! Exit from the program after showing error message
	!
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
