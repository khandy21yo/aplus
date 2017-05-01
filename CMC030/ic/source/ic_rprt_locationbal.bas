1	%TITLE "Location Balances"
	%SBTTL "IC_RPRT_LOCATIONBAL"
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
	! ID:IC025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Locations Balances\* option prints a report which
	!	contains the following information:
	!	.table 3,25
	!	.te
	!	Product _#
	!	.te
	!	Description
	!	.te
	!	Type
	!	.te
	!	Category
	!	.te
	!	On Hand
	!	.te
	!	Allocated
	!	.te
	!	On Order
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Location Balances>Report
	!	.x Report>Location Balances
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_LOCATIONBAL/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_LOCATIONBAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_LOCATIONBAL.OBJ;*
	!
	! Author:
	!
	!	06/13/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantity values to integer values.
	!
	!	10/22/92 - Dan Perkins
	!		Added Secondary Code to sort options.  Display
	!		Unit of Measure.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/29/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	08/29/95 - Kevin Handy
	!		Modified so that it will not ignore a product if it
	!		can't read the balance file for it.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/28/98 - Kevin Handy
	!		Check for nonzero modified to compare against
	!		a small number instead of 0
	!
	!	10/28/98 - Kevin Handy
	!		Add record counts
	!
	!	10/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins the report
	!	printing with a selected item number.  The value entered
	!	must be in agreement with field (10) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Location Balances
	!	.x Location Balances>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item number
	!	with which the report will end printing.  The value entered
	!	must be in agreement with field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Location Balances
	!	.x Location Balances>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	item numbers to be printed by
	!	entering a "Wildcard" selection in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Location Balances
	!	.x Location Balances>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the locations
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Location Balances
	!	.x Location Balances>Locations
	!
	!--

	ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Zero\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero\* balances setting
	!	provides the option of printing the items which have or do
	!	not have zero balances.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Zero>Location Balances
	!	.x Location Balances>Print Zero
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects an order
	!	by which the report will PRINT_
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Secondary Code
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Location Balances
	!	.x Location Balances>Sort By
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE IC_CONTROL.CH%

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	END SELECT

	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  LOCATION  BALANCES  " + ADD_TITLE$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"                Tp Cat  Sec_Code   UOM" + &
		"       OnHand         Alloc       OnOrder"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	COUNT_GROUP% = 0%
	COUNT_TOTAL% = 0%
	LAST_GROUP$ = ""

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	GOSUB SubTotal

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	TITLE$(2%) = ADD_TITLE$ + "  IN  " + YYYYPP$ + "  " + &
		TRM$(PERIOD_DESCR$) + "  AT LOCATION " + &
		UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

17010	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, &
				KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <>"" AND PRINT_LINE%
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	CASE "S"
		GOTO NextLocation IF (PD_PRODUCT::SECONDARY_CODE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	!
	! Print out one line
	!
	IF (IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) AND 1%) = 0%
	THEN
		ONHAND = 0.0
		ALLOC   = 0.0
		ONORDER = 0.0

		CUR_ONHAND = 0.0
		CUR_ALLOC = 0.0
		CUR_ONORDER = 0.0
	ELSE
		ONHAND = BALANCE(1%, 1%)
		ALLOC   = BALANCE(2%, 1%)
		ONORDER = BALANCE(3%, 1%)

		CUR_ONHAND = BALANCE(1%, 2%)
		CUR_ALLOC = BALANCE(2%, 2%)
		CUR_ONORDER = BALANCE(3%, 2%)
	END IF

	ZERO_TOTAL = ABS(ONHAND) + ABS(ALLOC) + ABS(ONORDER) + &
		ABS(CUR_ONHAND) + ABS(CUR_ALLOC) + ABS(CUR_ONORDER)

	IF (ZERO$ = "Y") OR (ABS(ZERO_TOTAL) > 0.001)
	THEN
		COUNT_GROUP% = COUNT_GROUP% + 1%
		COUNT_TOTAL% = COUNT_TOTAL% + 1%

		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			PD_PRODUCT::DESCRIPTION + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + " " + &
			PD_PRODUCT::UOM + "  " + &
			FORMAT$(ONHAND, "############") + "  " + &
			FORMAT$(ALLOC, "############") + "  " + &
			FORMAT$(ONORDER, "############")

		LIN% = 2%

		IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
		THEN
			IF PRINT_FLAG%
			THEN
				LIN% = 999%
				!UTL_REPORTX::PAGENO = 0%
			END IF

			LAST_LOCATION$ = UTL_LOCATION::LOCATION
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

		PRINT_FLAG% = -1%
		PRINT_LINE% = -1%

		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + "  " + &
			SPACE$(LEN(PD_PRODUCT::DESCRIPTION) - 3%) + "CUR " + &
			SPACE$(LEN(PD_PRODUCT::PROD_TYPE)) + " " + &
			SPACE$(LEN(PD_PRODUCT::CATEGORY)) + " " + &
			SPACE$(LEN(PD_PRODUCT::SECONDARY_CODE)) + " " + &
			SPACE$(LEN(PD_PRODUCT::UOM)) + " " + &
			FORMAT$(CUR_ONHAND, "############") + "  " + &
			FORMAT$(CUR_ALLOC, "############") + "  " + &
			FORMAT$(CUR_ONORDER, "############")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + "  " + &
			SPACE$(LEN(PD_PRODUCT::DESCRIPTION) - 5%) + "TOTAL " + &
			SPACE$(LEN(PD_PRODUCT::PROD_TYPE)) + " " + &
			SPACE$(LEN(PD_PRODUCT::CATEGORY)) + " " + &
			SPACE$(LEN(PD_PRODUCT::SECONDARY_CODE)) + " " + &
			SPACE$(LEN(PD_PRODUCT::UOM)) + " " + &
			FORMAT$(CUR_ONHAND + ONHAND, "############") + "  " + &
			FORMAT$(CUR_ALLOC + ALLOC, "############") + "  " + &
			FORMAT$(CUR_ONORDER + ONORDER, "############")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

	!
	! Print out subtotals
	!
 SubTotal:
	IF COUNT_GROUP%
	THEN
		TEXT$ = "Subtotal of " + NUM1$(COUNT_GROUP%) + " Parts"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	END IF
	COUNT_GROUP% = 0%

	RETURN

	!
	! Print out totals
	!
 ExitTotal:
	GOSUB SubTotal
	TEXT$ = "Total of " + NUM1$(COUNT_TOTAL%) + " Parts"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

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
