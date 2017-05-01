1	%TITLE "Cycle Count Journal"
	%SBTTL "IC_RPRT_JOURCOUNT"
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
	! ID:IC007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Counted Quantities\* option will provide a report
	!	which will contain the following:
	!	.table 3,25
	!	.te
	!	Product _#	Description
	!	.te
	!	Type	Category
	!	.te
	!	Pack	Form
	!	.te
	!	Unit of Measure	Pack Factor
	!	.te
	!	Count Pack	Count Unit
	!	.te
	!	Extended Cost
	!	.end table
	!	.lm -10
	!
	! Index:
	!	.x Counted Quantities>Report
	!	.x Report>Counted Quantities
	!	.x Print>Counted Quantities Report
	!	.x Counted Quantities Report>Print
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_JOURCOUNT/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_JOURCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_JOURCOUNT.OBJ;*
	!
	!
	! Author:
	!
	!	08/02/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	02/26/92 - Kevin Handy
	!		Removed references to PD_PACK file that Frank
	!		deleted so programs couldn't compile.
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/98 - Kevin Handy
	!		Add control number to printout
	!		Allow to leave off zero's
	!
	!	11/08/2000 - Kevin Handy
	!		Add a SORTBY option
	!
	!	12/28/2000 - Kevin Handy
	!		Add code to subtotal by type/category
	!
	!	01/08/2003 - Kevin Handy
	!		Change title from "StdCst" to "Factor".
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	RECORD IC_SORT_CDD
		STRING LOCATION = 4%
		STRING SORTKEY = 16%
		IC_JOURCOUNT_CDD IC_JOURCOUNT
	END RECORD

	MAP (IC_SORT) IC_SORT_CDD IC_SORT

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL	REAL    FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
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
	!	The ^*Batch Number\* field enters the number
	!	of the batch which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Cycle Count Journal
	!	.x Cycle Count Journal>Batch Number
	!	.x Number>Batch
	!
	!--

	SHOW_ZERO$ = LEFT$(UTL_REPORTX::OPTDEF(1%), 1%)

	!++
	! Abstract:FLD02
	!	^*(02) Show Zero's\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Show Zero>Cycle Count Journal
	!	.x Zero>Cycle Count Journal
	!
	!--

	SHOW_STD$ = LEFT$(UTL_REPORTX::OPTDEF(2), 1%)

	!++
	! Abstract:FLD03
	!	^*(03) Show StdCst or Factor
	!	.b
	!	.lm +5
	!	Determines if one column in the report will display the usage
	!	factor (F), or the standard cost (C).
	!	.lm -5
	!
	! Index:
	!	.x Show StdCst>Cycle Count Journal
	!	.x Show Factor>Cycle Count Journal
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Cycle Count Journal
	!	.x Cycle Count Journal>Locations
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
		SORT_DESCR$ = "Category "

	CASE "D"
		SORT_DESCR$ = "Description"

	CASE "T"
		SORT_DESCR$ = "ProdType "

	CASE ELSE
		SORT_DESCR$ = "Part Number"
		SORT_BY$ = "P"

	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
	USE
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.OPN"
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

400	!
	! Create sorting file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Building Sort...", 1%)

	WHEN ERROR IN
		CALL ASSG_CHANNEL(IC_SORT.CH%, STAT%)

		OPEN IC_JOURCOUNT.DEV$ + "IC_TEMP.SORT" &
			FOR OUTPUT AS FILE IC_SORT.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_SORT, &
			TEMPORARY, &
			EXTENDSIZE 27%, &
			PRIMARY KEY &
			( &
				IC_SORT::LOCATION, &
				IC_SORT::SORTKEY &
			) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "IC_SORT"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		RESET #IC_JOURCOUNT.CH%
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		GET #IC_JOURCOUNT.CH%
	USE
		CONTINUE 490 IF ERR = 11%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO 410 IF IC_JOURCOUNT::QUANTITY = 0.0 AND SHOW_ZERO$ = "N"

	!
	! Get the product master file if we need it to sort
	!
	SELECT SORT_BY$

	CASE "C", "D", "T"

		V% = PD_EXAM_PRODUCT(IC_JOURCOUNT::PRODUCT, PD_PRODUCT_EXAM)

	END SELECT


	IC_SORT::LOCATION = IC_JOURCOUNT::LOCATION
	IC_SORT::IC_JOURCOUNT = IC_JOURCOUNT

	SELECT SORT_BY$

	CASE "C"
		IC_SORT::SORTKEY = PD_PRODUCT_EXAM::CATEGORY

	CASE "D"
		IC_SORT::SORTKEY = PD_PRODUCT_EXAM::DESCRIPTION

	CASE "P"
		IC_SORT::SORTKEY = IC_JOURCOUNT::PRODUCT

	CASE "T"
		IC_SORT::SORTKEY = PD_PRODUCT_EXAM::PROD_TYPE
	END SELECT

	WHEN ERROR IN
		PUT #IC_SORT.CH%
	USE
		FILENAME$ = "IC_SORT"
		CONTINUE HelpError
	END WHEN

	GOTO 410

490	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CYCLE  COUNT  JOURNAL"
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	IF SHOW_STD$ = "C"
	THEN
		ITEM1$ = "StdCst"
	ELSE
		ITEM1$ = "Factor"
	END IF

	TITLE$(5%) = "Product#       Description                 "  + &
		"             UOM Tp Cat  PackUOM   " + ITEM1$ + "   " + &
		"CountPack    CountUnt    ExtendCost"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	LAST_SORT$ = "~~~~~~~~~~"
	LAST_SORT% = 0%

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

17010	WHEN ERROR IN
		FIND #IC_CYCLEJOUR.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS

		GET #IC_CYCLEJOUR.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155% OR ERR = 11%
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

	TITLE$(2%) = "  AT LOCATION " + TRM$(UTL_LOCATION::LOCATION) + " " + &
		TRM$(UTL_LOCATION::LOCNAME) + " ON " + &
		PRNT_DATE(IC_CYCLEJOUR::COUNTDATE, 8%)

17015	WHEN ERROR IN
		FIND #IC_SORT.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_SORT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #IC_SORT.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_SORT"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation &
		IF IC_SORT::IC_JOURCOUNT::LOCATION <> UTL_LOCATION::LOCATION

 !	GOTO GetNextRec IF IC_SORT::IC_JOURCOUNT::QUANTITY = 0.0 AND SHOW_ZERO$ = "N"

	!
	! Read Product file
	!
	V% = PD_EXAM_PRODUCT(IC_SORT::IC_JOURCOUNT::PRODUCT, PD_PRODUCT_EXAM)

	SELECT SORT_BY$
	CASE "C"
		IF LAST_SORT$ <> PD_PRODUCT_EXAM::CATEGORY
		THEN
			GOSUB 18100
		END IF

		LAST_SORT$ = PD_PRODUCT_EXAM::CATEGORY

	CASE "T"
		IF LAST_SORT$ <> PD_PRODUCT_EXAM::PROD_TYPE
		THEN
			GOSUB 18100
		END IF

		LAST_SORT$ = PD_PRODUCT_EXAM::PROD_TYPE
	END SELECT

	!
	! Print out one line
	!
	COST = PC_READ_COST(IC_SORT::IC_JOURCOUNT::PRODUCT, &
		UTL_LOCATION::LOCATION, IC_CYCLEJOUR::COUNTDATE, "")

	EXTCOST = FUNC_ROUND(COST * IC_SORT::IC_JOURCOUNT::QUANTITY, 2%)

	IF PD_PRODUCT_EXAM::PRODUCT_FACTOR
	THEN
		PACK_QTY = IC_SORT::IC_JOURCOUNT::QUANTITY / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR <> 0.0
	ELSE
		PACK_QTY = IC_SORT::IC_JOURCOUNT::QUANTITY
	END IF

	IF SHOW_STD$ = "C"
	THEN
		ITEM1$ = FORMAT$(COST, "#######.##")
	ELSE
		ITEM1$ = FORMAT$(PD_PRODUCT_EXAM::PRODUCT_FACTOR, "########.#")
	END IF

	TEXT$ = IC_SORT::IC_JOURCOUNT::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + " " + &
		PD_PRODUCT_EXAM::UOM + "  " + &
		PD_PRODUCT_EXAM::PROD_TYPE + " " + &
		PD_PRODUCT_EXAM::CATEGORY + "     " + &
		PD_PRODUCT_EXAM::BOMUOM + &
		ITEM1$ + " " + &
		FORMAT$(PACK_QTY, "###,###.###") + " " + &
		FORMAT$(IC_SORT::IC_JOURCOUNT::QUANTITY, "###,###,###") + " " + &
		FORMAT$(EXTCOST, "##,###,###.## ") + &
		IC_SORT::IC_JOURCOUNT::CONTROL

	LIN% = 0%
	LAST_SORT% = LAST_SORT% + 1%

	IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
	THEN
		IF UTL_REPORTX::PAGENO
		THEN
			LIN% = 999%
			GOSUB 18000
			TOTAL_COST = 0.0
		END IF
		LAST_LOCATION$ = UTL_LOCATION::LOCATION
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	TOTAL_COST = TOTAL_COST + EXTCOST
	SUBTOTAL_COST = SUBTOTAL_COST + EXTCOST

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB 18100
	GOSUB 18000

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

18000	IF UTL_REPORTX::PAGENO
	THEN
		GOSUB 18100

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = SPACE$(98%) + "TOTAL:     " + &
			FORMAT$(TOTAL_COST, "##,###,###.##")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	RETURN

18100	IF LAST_SORT% <> 0%
	THEN
		SELECT SORT_BY$
		CASE "C"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

			TEXT$ = SPACE$(86%) + "CATEGORY SUBTOTAL:     " + &
				FORMAT$(SUBTOTAL_COST, "##,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		CASE "T"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

			TEXT$ = SPACE$(90%) + "TYPE SUBTOTAL:     " + &
				FORMAT$(SUBTOTAL_COST, "##,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		END SELECT
	END IF

	SUBTOTAL_COST = 0.0
	LAST_SORT% = 0%

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
