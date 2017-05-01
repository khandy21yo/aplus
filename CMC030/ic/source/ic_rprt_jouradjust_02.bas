1	%TITLE "Cycle Count Adjustment Journal"
	%SBTTL "IC_RPRT_JOURADJUST_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	! supported by CMC.
	!
	!++
	! ID:IC004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Adjusted Quantities\* option will provide a report
	!	which will contain the following:
	!	.table 3,25
	!	.te
	!	Product _#
	!	.te
	!	Description
	!	.te
	!	Unit of Measure
	!	.te
	!	Type
	!	.te
	!	Category
	!	.te
	!	Adjusted Quantity
	!	.te
	!	Standard Cost
	!	.te
	!	Extended Cost
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Adjusted Quantities
	!	.x Adjusted Quantities>Report
	!	.x Print>Adjusted Quantities Report
	!	.x Adjusted Quantities Report>Print
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_JOURADJUST_02/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_JOURADJUST_02, -
	!	FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE IC_RPRT_JOURADJUST_02.OBJ;*
	!
	!
	! Author:
	!
	!	11/01/98 - Kevin Handy
	!
	! Modification History:
	!
	!	11/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/09/2001 - Kevin Handy
	!		Change maximum tran files from 100 to 300.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP(IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG   FUNCTION IC_READ_35BALANCE

	DIM IC_TRANSACTION_FILE$(300%)
	DIM IC_TRANSACTION.CH%(48%)		! More open than this is crazy

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the batch _#
	!	corresponding to the Journal which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Adjustment Journal
	!	.x Number>Batch
	!	.x Adjustment Journal>Batch Number
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.B
	!	.LM +5
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.B
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!	.x Locations>Adjustment Journal
	!	.x Adjustment Journal>Locations
	!
	!--

	DOLLAR_LEVEL = ABS(VAL(UTL_REPORTX::OPTDEF(5%)))

	!++
	! Abstract:FLD06
	!	^*(06) Minimum Dollar Level\*
	!	.B
	!	.LM +5
	!	Used to specify that only thise items with a dollar value
	!	greater than or equal to this amount.
	!	It compares the magnitude, and not the sign of the number.
	!	.LM -5
	!
	! Index:
	!	.x Locations>Adjustment Journal
	!	.x Adjustment Journal>Locations
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field prints the
	!	report in a selected order.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Adjustment Journal
	!	.x Adjustment Journal>Sort
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		SORT_DESCR$ = "Category "

	CASE "D"
		SORT_KEY% = 3%
		SORT_DESCR$ = ""

	CASE "P"
		SORT_KEY% = 0%
		SORT_DESCR$ = ""

	CASE "T"
		SORT_KEY% = 1%
		SORT_DESCR$ = "ProdType "
	END SELECT

	LAST_LOCATION$ = ""
	LIN% = 0%

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
	USE
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

325	!
	! Figure out which transaction files exist
	!
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)
	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), 16%, "", "")

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))
	IC_LIST% = 0%

330	!
	! Pick out the intresting ones (Current period and later)
	!
	FOR LOOP% = 1% TO IC_TRANSACTION_FILE%

		YYYYPP$ = MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%)
		IF YYYYPP$ >= IC_CONTROL::PERIOD
		THEN
			IC_LIST% = IC_LIST% + 1%

			!
			! Open up the offending transaction file
			!
			IC_TRANSACTION.CH% = 0%
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
			IC_TRANSACTION.CH%(IC_LIST%) = IC_TRANSACTION.CH%
		END IF

	NEXT LOOP%

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
	USE
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

350	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
		CALL ASSG_FREECHANNEL(IC_CONTROL%)
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

360	!
	! Jourcount header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.OPN"
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CYCLE  COUNT  ADJUSTMENT  JOURNAL"
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description                      " + &
		"        UOM Tp Cat    AdjustQty      CountQty       OnHand"

	TITLE$(6%) = "."

	LYT_LINE$ = "$Product#:015,$Description:056,$UOM:060,$Tp:063," + &
		"$Cat:070,VAdjustQty:087,VStdCost:098,VExtendCost:108"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	!
	! Start at the beginning of the location file
	!
	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	!
	! Get the (next) location record
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! Get the next location if this one is not one we wanted
	!
	GOTO NextLocation IF &
		(COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), &
		LOCATION$) = 0%) AND &
		(LOCATION$ <> "")

	!
	! Get the Cycle journal record tied in to that location number
	!
17010	WHEN ERROR IN
		GET #IC_CYCLEJOUR.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Reset the title
	!
	TITLE$(2%) = "  AT LOCATION " + TRM$(UTL_LOCATION::LOCATION) + " " + &
		TRM$(UTL_LOCATION::LOCNAME) + " ON " + &
		PRNT_DATE(IC_CYCLEJOUR::COUNTDATE, 8%)

	!
	! Start at the beginning of the product file
	!
	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

	LAST_LOCATION$ = UTL_LOCATION::LOCATION
	TOTAL_LOCATION = 0.0

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	!
	! Check if inactive product
	!
 !	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

17200	WHEN ERROR IN
		GET #IC_JOURADJUST.CH%, &
			KEY #0% EQ (UTL_LOCATION::LOCATION + &
			PD_PRODUCT::PRODUCT_NUM), &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	SELECT SORT_BY$

	CASE "C"
		GOSUB 18100 IF &
			(TEST_CATEGORY$ <> "") AND &
			(TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY) AND &
			(PRINT_LINE%)

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "T"
		GOSUB 18100 IF &
			(TEST_PRODTYPE$ <> "") AND &
			(TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE) AND &
			(PRINT_LINE%)

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17250	!
	! Grab the original count
	!
	COUNTED = 0.0

	WHEN ERROR IN
		FIND #IC_JOURCOUNT.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION + &
			PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

17260	WHEN ERROR IN
		GET #IC_JOURCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

	IF (IC_JOURCOUNT::LOCATION = UTL_LOCATION::LOCATION) AND &
		(IC_JOURCOUNT::PRODUCT = PD_PRODUCT::PRODUCT_NUM)
	THEN
		COUNTED = COUNTED + IC_JOURCOUNT::QUANTITY
		GOTO 17260
	END IF

17300	!
	! Print out one line
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, IC_CYCLEJOUR::COUNTDATE, "")

	EXTCOST = FUNC_ROUND(COST*IC_JOURADJUST::QUANTITY, 2%)

	GOSUB 18000

	!
	! Skip this item if it isn't at the right magnitude
	!
	GOTO GetNextRec IF ABS(EXTCOST) < DOLLAR_LEVEL

	TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + &
		FORMAT$(IC_JOURADJUST::QUANTITY, " ###,###.###") + &
		FORMAT$(COUNTED, " ###,###.###") + &
		FORMAT$(ONHAND, " ###,###.###")

	!
	! Get the Inventory account number
	!
	ST% = PD_READ_ACCOUNT(IC_JOURADJUST::LOCATION, &
		PD_PRODUCT::PROD_TYPE, PD_ACCOUNT_EXAM)

	GOTO ExitProgram IF (ST% = CMC$_UNTERROR)

	!
	! Print location total
	!
	IF (LAST_LOCATION$ <> UTL_LOCATION::LOCATION) AND &
		(UTL_REPORTX::PAGENO)
	THEN
		LIN% = 999%

		TEXT$ = SPACE$(LEN(TEXT_LIN$) - 13%) + &
			FORMAT$(TOTAL_LOCATION, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		LAST_LOCATION$ = UTL_LOCATION::LOCATION
		TOTAL_LOCATION = 0.0

	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)

	PRINT_LINE% = -1%

	LIN% = 0%

	!
	! Generate subtotals
	!
	TOTAL_LOCATION = TOTAL_LOCATION + EXTCOST
	SUBTOTAL_LOCATION = SUBTOTAL_LOCATION + EXTCOST

	!
	! Put the Debit/Credit information into the temporary file
	! (Inventory record)
	!
	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM(OPT_ADDREC, PD_ACCOUNT_EXAM::INVACCT, &
		0.0, EXTCOST, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Put the Debit/Credit information into the temporary file
	! (Expense record)
	!
	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, IC_JOURADJUST::ACCOUNT, &
		0.0, -EXTCOST, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	SELECT SORT_BY$

	CASE "C", "T"
		GOSUB 18100 IF PRINT_LINE%

	END SELECT

	!
	! Print location total
	!
	IF UTL_REPORTX::PAGENO
	THEN
		LIN% = 999%
		TEXT$ = "               Location Total" + &
			SPACE$(LEN(TEXT_LIN$) - 42%) + &
			FORMAT$(TOTAL_LOCATION, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

 ExitProgram:
	FOR IC_LOOP% = 1% TO IC_LIST%
		CLOSE #IC_TRANSACTION.CH%(IC_LOOP%)
		CALL ASSG_FREECHANNEL(IC_TRANSACTION.CH%(IC_LOOP%))
	NEXT IC_LOOP%

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

18000	!
	! Compare balance file with cycle couting
	!
	TEST_PRODUCT$ = PD_PRODUCT::PRODUCT_NUM
	TEST_LOCATION$ = IC_JOURCOUNT::LOCATION

	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		IC_JOURCOUNT::LOCATION, BALANCE(,))

	ONHAND = BALANCE(1%, 1%)

18010	FOR IC_LOOP% = 1% TO IC_LIST%

		WHEN ERROR IN
			FIND #IC_TRANSACTION.CH%(IC_LOOP%), &
				KEY #0% EQ TEST_PRODUCT$ + TEST_LOCATION$, &
				REGARDLESS
		USE
			CONTINUE 18025 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

18020		WHEN ERROR IN
			GET #IC_TRANSACTION.CH%(IC_LOOP%), REGARDLESS
		USE
			CONTINUE 18025 IF ERR = 11%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		GOTO 18025 IF TEST_PRODUCT$ <> IC_TRANSACTION::PRODUCT OR &
			TEST_LOCATION$ <> IC_TRANSACTION::LOCATION OR &
			IC_TRANSACTION::TRANS_DATE > IC_CYCLEJOUR::COUNTDATE

		IF IC_TRANSACTION::TYPE_A <> "" AND &
			IC_TRANSACTION::QUANTITY_A <> 0.0
		THEN
			SELECT IC_TRANSACTION::TYPE_A
			CASE "CC", "IS", "MA", "RE", "RT", "SA", "SE", "SP", &
				"TR", "WA", "WR"

				ONHAND = ONHAND + IC_TRANSACTION::QUANTITY_A

			CASE ELSE
				! NO IMPACT ON HAND
			END SELECT
		END IF

		IF IC_TRANSACTION::TYPE_B <> "" AND &
			IC_TRANSACTION::QUANTITY_B <> 0.0
		THEN
			SELECT IC_TRANSACTION::TYPE_B
			CASE "SO", "MO", "RQ", "TO", "LS", "PO", "WO"
				! NO IMPACT ON HAND

			CASE ELSE
				ONHAND = ONHAND + IC_TRANSACTION::QUANTITY_B

			END SELECT
		END IF
		GOTO 18020

18025	NEXT IC_LOOP%

	RETURN

18100	!
	! Print subtotal cost
	!
	TEXT$ = SPACE$(15%) + SORT_DESCR$ + "Total" + SPACE$(132%)

	TEXT$ = LEFT(TEXT$, LEN(TEXT_LIN$) - 13%) + &
		FORMAT$(SUBTOTAL_LOCATION, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	PRINT_LINE% = 0%
	SUBTOTAL_LOCATION = 0.0

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
