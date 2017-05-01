1	%TITLE "Safety Stock Balances"
	%SBTTL "PO_RPRT_FREESTOCK"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! ID:PO053
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Safety Stock Balances\* report determines the
	!	current stock level and compares it with the recommended level. This report
	!	determines if the stock level is above the safety level thus suppling free
	!	stock for sale. The following fields are included in this report:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Type	Category
	!	.te
	!	Free Stock	Safety Stock on Hand
	!	.te
	!	Available Stock	Allocated
	!	.te
	!	On order	Back Order
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Safety Stock Balances Report
	!	.x Report>Saftey Stock Balances
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_FREESTOCK/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_FREESTOCK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_FREESTOCK.OBJ;*
	!
	! Author:
	!
	!	10/24/2000 - Kevin Handy
	!		Based on IC_RPRT_FREESTOCK
	!
	! Modification History:
	!
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

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

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* setting in this field determines
	!	the order in which the report will print.
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
	!	^*S\* - Product Secondary Code
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Safety Stock Balances
	!	.x Safety Stock Balances>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a selected
	!	item from which the report will begin printing.
	!	The value entered must be in agreement with field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Safety Stock Balances
	!	.x Safety Stock Balances>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an
	!	item number with which the report will end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Safety Stock Balances
	!	.x Safety Stock Balances>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	items to be printed by entering a "wildcard"
	!	value.
	!	.b
	!	The "Wildcard" value entered must be in agreement with
	!	field (01) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Safety Stock Balances
	!	.x Safety Stock Balances>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

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
	!	.x Locations>Safety Stock Balances
	!	.x Safety Stock Balances>Locations
	!
	!--

	ONLY_CTD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Only Beginning\*
	!	.b
	!	.lm +5
	!	The ^*Only Beginning\* field prints the report containing
	!	only the beginning of the period information or all information including the
	!	present period.
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
	!	.x Only Beginning>Safety Stock Balances
	!	.x Safety Stock Balances>Only Beginning
	!
	!--

	ONLY_NEG$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Only Negative\*
	!	.b
	!	.lm +5
	!	The ^*Only Negative\* field prints all results or only
	!	the negative results of the stock balances.
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
	!	.x Only Negative>Safety Stock Balances
	!	.x Safety Stock Balances>Only Negative
	!
	!--

	SKIP_ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Skip Zero\*
	!	.b
	!	.lm +5
	!	This field allows leaving zero balances off the report.
	!	If all columns for a product are zero,
	!	and this field is set to "yes",
	!	then that product will be left off the report.
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
	!	.x Skip Zero>Safety Stock Balances
	!	.x Safety Stock Balances>Skip Zero
	!
	!--

	ONLY_VENDOR$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 10%)

	!++
	! Abstract:FLD10
	!	^*(10) Only Vendor\*
	!	.b
	!	.lm +5
	!	This field selects a single vendor whose products should
	!	be included.
	!	.lm -5
	!
	! Index:
	!	.x Skip Zero>Safety Stock Balances
	!	.x Safety Stock Balances>Skip Zero
	!
	!--

	SELECT ONLY_CTD$
	CASE "Y"
		IN_TITLE$ = "IN  BEGINNING  "

	CASE "N"
		IN_TITLE$ = ""
	END SELECT

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

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

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

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  FREE  STOCK  BALANCES  " + ADD_TITLE$

	IF ONLY_VENDOR$ = ""
	THEN
		TITLE$(3%) = "FOR ALL VENDORS"
	ELSE
		TITLE$(3%) = "FOR VENDOR " + ONLY_VENDOR$
	END IF

	TITLE$(4%) = "Inventory Control System"
	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product#       Description             " + &
		"       Tp Cat  SecCode       FreeStk  SafetyStk " + &
		"  AvailStk     OnHand      Alloc    OnOrder"

	TITLE$(7%) = "."
	TITLE$(8%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	TITLE$(2%) = IN_TITLE$ + "  IN  " + YYYYPP$ + "  " + &
		TRM$(PERIOD_DESCR$) + "  AT LOCATION " + &
		UTL_LOCATION::LOCATION + " " + UTL_LOCATION::LOCNAME

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

	!
	! Let's check up on if the vendor has something defined for
	! this product. (And I'm using GET here because I know that
	! I'll soon need to display stuff from this file)
	!
	IF NOT (ONLY_VENDOR$ = "")
	THEN
		WHEN ERROR IN
			GET #PO_PARTCROSS.CH%, &
				KEY #1% EQ ONLY_VENDOR$ + PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
		USE
			CONTINUE GetNextRec
		END WHEN
	END IF

	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO NextLocation IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17200	IC_BINMAP::SAFETY = 0.0

	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	GOTO GetNextRec IF 1% AND IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) = 0%

	ONHAND = BALANCE(1%, 1%)
	ALLOC   = BALANCE(2%, 1%)
	ONORDER = BALANCE(3%, 1%)

	IF ONLY_CTD$ = "N"
	THEN
		ONHAND = ONHAND + BALANCE(1%, 2%) + BALANCE(1%, 3%)
		ALLOC = ALLOC  + BALANCE(2%, 2%) + BALANCE(2%, 3%)
		ONORDER = ONORDER + BALANCE(3%, 2%) + BALANCE(3%, 3%)
	END IF

	ONHAND = FUNC_ROUND(ONHAND, 0%)
	ALLOC  = FUNC_ROUND(ALLOC, 0%)
	ONORDER = FUNC_ROUND(ONORDER, 0%)

	AVAIL_STOCK = ONHAND + ALLOC
	FREE_STOCK = FUNC_ROUND(AVAIL_STOCK + ONORDER - IC_BINMAP::SAFETY, 0%)

	PRINT_FLAG% = (SKIP_ZERO$ <> "Y") OR &
		(ONHAND <> 0.0) OR (ALLOC <> 0.0) OR (ONORDER <> 0.0) OR &
		(FREE_STOCK <> 0.0)

	IF ((FREE_STOCK < 0.0) OR (ONLY_NEG$ = "N")) AND (PRINT_FLAG% <> 0%)
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 30%) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + " " + &
			FORMAT$(FREE_STOCK, "##########") + " " + &
			FORMAT$(IC_BINMAP::SAFETY, "##########") + " " + &
			FORMAT$(AVAIL_STOCK, "##########") + " " + &
			FORMAT$(ONHAND, "##########") + " " + &
			FORMAT$(-ALLOC, "##########") + " " + &
			FORMAT$(ONORDER, "##########")

		LIN% = 1%

		IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
		THEN
			IF PRINT_FLAG%
			THEN
				LIN% = 999%
			END IF

			LAST_LOCATION$ = UTL_LOCATION::LOCATION
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
		PRINT_FLAG% = -1%
		PRINT_LINE% = -1%

	END IF

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

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
