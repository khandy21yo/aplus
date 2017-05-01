1	%TITLE "Cycle Count Adjustment Journal"
	%SBTTL "IC_RPRT_JOURADJUST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	$ BAS IC_SOURCE:IC_RPRT_JOURADJUST/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_JOURADJUST, -
	!	FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE IC_RPRT_JOURADJUST.OBJ;*
	!
	!
	! Author:
	!
	!	08/02/87 - Frank Starman
	!
	! Modification History:
	!
	!	08/14/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	06/10/91 - Craig Tanner
	!		Modified to use GL_OUTP_ACCTSUM to do debit/credit page.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in this function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/09/94 - Kevin Handy
	!		Modified to fir in 80 columns.
	!
	!	02/09/94 - Kevin Handy
	!		Modified to not ignore inactive products, so that
	!		King B could get a print out.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/26/98 - Kevin Handy
	!		Add field to allow for limiting the dollar volumn
	!		that gets printed on the-report.
	!
	!	10/30/98 - Kevin Handy
	!		Clean up work on generating GL summary information.
	!
	!	11/07/2000 - Kevin Handy
	!		Use WHEN ERROR NI
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL REAL	FUNCTION PC_READ_COST

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
	!	The ^*Batch Number\* field is to be entered with the batch _#
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

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
	USE
		FILENAME$ = "IC_CYCLEJOUR"
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
		"        UOM Tp Cat    AdjustQty        StdCost    ExtendCost"

	TITLE$(6%) = "."

	LYT.LINE$ = "$Product#:015,$Description:056,$UOM:060,$Tp:063," + &
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
		FILENAME$ = "PD_PRODUCT"
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

17300	!
	! Print out one line
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, IC_CYCLEJOUR::COUNTDATE, "")

	EXTCOST = FUNC_ROUND(COST * IC_JOURADJUST::QUANTITY, 2%)

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
		FORMAT$(COST, " ###,###.######") + &
		FORMAT$(EXTCOST, " ##,###,###.##")

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
		TOTAL_LOCATION = 0.

	END IF

	CALL OUTP_LINE(LYT.LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)

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
