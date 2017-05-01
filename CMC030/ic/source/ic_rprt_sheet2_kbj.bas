1	%TITLE "Location Balances"
	%SBTTL "IC_RPRT_SHEET2_KBJ"
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
	! ID:IC025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Locations Balances\* option will print a report which
	!	will contain the following information:
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
	!	$ BAS IC_SOURCE:IC_RPRT_SHEET2_KBJ/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_SHEET2_KBJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_SHEET2_KBJ.OBJ;*
	!
	! Author:
	!
	!	10/17/2000 - Kevin Handy
	!
	! Modification History:
	!
	!	10/18/2000 - Kevin Handy
	!		New formula for safety stock
	!
	!	10/19/2000 - Kevin Handy
	!		Add from/to period to title
	!		Use Posted balance instead of current balance
	!
	!	11/29/2000 - Kevin Handy
	!		Use BM_ASSEMBLY to get more information.
	!
	!	11/30/2000 - Kevin Handy
	!		Added debug code to see why it hangs up on specific
	!		parts. Damn, salt is used in EVERYTHING, but the
	!		program actually is working correctly, if slowly.
	!
	!	11/30/2000 - Kevin Handy
	!		Add MAX_CLIMB% to limit how far to climb.
	!
	!	12/15/2000 - Kevin Handy
	!		Loop through the xbalance array (1..3) instead
	!		os only looking at (1). Should make balance match
	!		the query better.
	!
	!	12/22/2000 - Kevin Handy
	!		Yet another try at the ONHAND qty's
	!
	!	12/26/2000 - Kevin Handy
	!		ONHAND should be based on the main product, not
	!		on one of the compoments
	!
	!	01/15/2000 - Kevin Handy
	!		Use POSTED balance instead of RUNNING balance
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	!
	! Arays
	!
	DIM IC_35HISTORY.CH%(10%)

	DECLARE INTEGER CONSTANT MAX_WORK_BM = 500%

	RECORD WORK_BM_CDD
		STRING COMPONENT = 14		! Component
		INTEGER LEVEL			! What level is it on
		GFLOAT MULTIPLIER		! How many needed of this
	END RECORD

	DIM WORK_BM_CDD WORK_BM(MAX_WORK_BM)

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
	!	entering a "Wildcard" selection.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Location Balances
	!	.x Location Balances>Wildcard
	!
	!--

	MAX_CLIMB% = VAL%(TRM$(UTL_REPORTX::OPTDEF(3%)))
	MAX_CLIMB% = 99% IF MAX_CLIMB% < 1%

	!++
	! Abstract:FLD04
	!	^*(04) Maximum Level\*
	!	.b
	!	.lm +5
	!	Determines how many BM levels to climb up before giving up.
	!	A zero is assumed to specify 99 levels (so it will not loop
	!	forever).
	!	A one specifies only look at the products, and
	!	ignore any BM assemblies.
	!	Two specifies this product and it immediate parents.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Location Balances
	!	.x Location Balances>Wildcard
	!
	!--

	LOCATION$ = LEFT$(UTL_REPORTX::OPTDEF(4%), 4%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
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
	!	prints the items which have or do
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

	FROM_PERIOD$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 6%)

	!++
	! Abstract:FLD08
	!	^*(08) From Period\*
	!	.b
	!	.lm +5
	!	Specifies the starting period for the "average" calculation
	!	.lm -5
	!
	! Index:
	!	.x From Period>Location Balances
	!	.x Location Balances>From Period
	!
	!--

	TO_PERIOD$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 6%)

	IF (TO_PERIOD$ < FROM_PERIOD$)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"'To Period' must be at or after 'From Period'", 0%)
		GOTO ExitTotal
	END IF

	!++
	! Abstract:FLD09
	!	^*(09) To Period\*
	!	.b
	!	.lm +5
	!	Specifies the ending period for the "average" calculation
	!	.lm -5
	!
	! Index:
	!	.x To Period>Location Balances
	!	.x Location Balances>To Period
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects an order
	!	by which the report will print.
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

	FROM_YEAR% = VAL%(LEFT(FROM_PERIOD$, 4%))
	FROM_PERIOD% = VAL%(RIGHT(FROM_PERIOD$, 5%))
	TO_YEAR% = VAL%(LEFT(TO_PERIOD$, 4%))
	TO_PERIOD% = VAL%(RIGHT(TO_PERIOD$, 5%))

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
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

330	FOR YEARS% = FROM_YEAR% TO TO_YEAR%

		IC_35HISTORY.CH% = 0%
		YYYY$ = FORMAT$(YEARS%, "<0>###")

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
			IC_35HISTORY.CH%(YEARS% - FROM_YEAR%) = &
				IC_35HISTORY.CH%
		USE
			IC_35HISTORY.CH%(YEARS% - FROM_YEAR%) = 0%
		END WHEN

	NEXT YEARS%

340	!
	! Bill of material assembly file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
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
	TITLE$(2%) = "For Location " + LOCATION$ + &
		" From Period " + FROM_PERIOD$ + " To Period " + TO_PERIOD$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "                                        " + &
		"                             On      Safety  " + &
		"      Lead         Min Averg Three"

	TITLE$(6%) = "Product#       Description              " + &
		"                Tp         Hand       Stock  " + &
		"      Time       Order Month Usage"

	TITLE$(7%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	COUNT_GROUP% = 0%
	COUNT_TOTAL% = 0%
	LAST_GROUP$ = ""

17010	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$)
		END IF

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <>"" AND PRINT_LINE%
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "P"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "T"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
				WLDCRD$) = 0%
		END IF

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	CASE "S"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE> TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
				WLDCRD$) = 0%
		END IF
	END SELECT

17300	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS

		LEAD = PO_PARTCROSS::LEAD
		MINORD = PO_PARTCROSS::MINQTY
	USE
		LEAD = 0.0
		MINORD = 0.0
	END WHEN

	GOSUB CalcAve

	!
	! 1st try at formula
	!
 !	IF LEAD <> 0.0
 !	THEN
 !		SAFETY = AVE3MONTH / (21.67 * LEAD * 5 / 7) * 1.25
 !	ELSE
 !		SAFETY = 0.0
 !	END IF

	!
	! 2nd try at formula
	!
	SAFETY = (LEAD / 30.0) * AVE3MONTH * 1.25

	IF (ZERO$ = "Y") OR &
		(ONHAND <> 0.0) OR &
		(LEAD <> 0.0) OR (MINORD <> 0.0) OR &
		(AVE3MONTH <> 0.0)
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			PD_PRODUCT::DESCRIPTION + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			FORMAT$(ONHAND, "############") + &
			FORMAT$(SAFETY, "############") + &
			FORMAT$(LEAD, "############") + &
			FORMAT$(MINORD, "############") + &
			FORMAT$(AVE3MONTH, "############")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	END IF

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

	!
	! Need to set this up
	!
 SubTotal:
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

 CalcAve:
18000	!*******************************************************************
	! Calculate the period average
	!*******************************************************************

	!
	! TOTAL_PERIODS% must not be zero, but this has been checked for
	! in a previous test, until the user finds some way around it.
	!
	TOTAL_PERIODS% = (TO_YEAR% * 12% + TO_PERIOD%) - &
		(FROM_YEAR% * 12% + FROM_PERIOD%) + 1%

	AVE3MONTH = 0.0
	TOTAL = 0.0
	ONHAND = 0.0

	!
	! Get current balances
	!
	IF (IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION$, BALANCE(,)) AND 1%) <> 0%
	THEN
		!
		! Stop at posted balance
		!
 !		ONHAND = ONHAND + (BALANCE(LOOP1%, 1%) + &
 !			BALANCE(LOOP1%, 2%)) * FACTOR &
 !			FOR LOOP1% = 1% TO 3%

		ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%)
	END IF

	WORK_BM(1%)::COMPONENT = PD_PRODUCT::PRODUCT_NUM
	WORK_BM(1%)::LEVEL = 1%
	WORK_BM(1%)::MULTIPLIER = 1.0

	WORK_BM_COUNT% = 1%

 ! TEXT$ = "Starting: " + &
 !	PD_PRODUCT::PRODUCT_NUM
 ! CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)


18100	!
	! Pull one part off the stack
	!
	PRODUCT$ = WORK_BM(WORK_BM_COUNT%)::COMPONENT
	FACTOR = WORK_BM(WORK_BM_COUNT%)::MULTIPLIER
	LEVEL% = WORK_BM(WORK_BM_COUNT%)::LEVEL
	WORK_BM_COUNT% = WORK_BM_COUNT% - 1%

 ! TEXT$ = "Trying: " + &
 !	PRODUCT$ + &
 !	format$(FACTOR, " ######.####") + &
 !	format$(LEVEL%, "  ###") + &
 !	"  (" + Num1$(WORK_BM_COUNT%) + ")"
 ! CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOSUB 18200

	!
	! Limit how many levels to climb up
	!
	GOTO 18100 IF LEVEL% >= MAX_CLIMB%

	!
	! Now see if we can find any components to add to the list
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #1% EQ PRODUCT$, &
			REGARDLESS
	USE
		CONTINUE 18180 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	WHILE BM_RELATION::COMPONENT = PRODUCT$

		IF WORK_BM_COUNT% >= MAX_WORK_BM
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Too many components to handle (" + &
				PRODUCT$ + " > " + &
				NUM1$(WORK_BM_COUNT%), 0%)
			GOTO 18100
		END IF

		!
		! Push information onto the stack
		!
		WORK_BM_COUNT% = WORK_BM_COUNT% + 1%

		WORK_BM(WORK_BM_COUNT%)::COMPONENT = BM_RELATION::PRODUCT
		WORK_BM(WORK_BM_COUNT%)::LEVEL = LEVEL% + 1%
		WORK_BM(WORK_BM_COUNT%)::MULTIPLIER = FUNC_ROUND(FACTOR * &
			BM_RELATION::QUANTITY, 4%)

		!
		! Look at the next relation
		!
		WHEN ERROR IN
			GET #BM_RELATION.CH%, &
				REGARDLESS
		USE
			CONTINUE 18180 IF ERR = 11%
			FILENAME$ = "BM_RELATION"
			CONTINUE HelpError
		END WHEN

	NEXT

18180	GOTO 18100 IF WORK_BM_COUNT% > 0%

18190	!
	! We can finally calculate an average
	!
	AVE3MONTH = TOTAL / TOTAL_PERIODS%

	RETURN

18200	!
	! Go through all the required years
	!
	FOR YEARS% = FROM_YEAR% TO TO_YEAR%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(YEARS% - FROM_YEAR%), &
				KEY #0% EQ PRODUCT$, &
				REGARDLESS
		USE
			CONTINUE 18280
		END WHEN

		!
		! In this year, what is the range of periods we need
		! to look at.
		!
		IF YEARS% = FROM_YEAR%
		THEN
			START% = FROM_PERIOD% - 1%
		ELSE
			START% = 0%
		END IF

		IF YEARS% = TO_YEAR%
		THEN
			FINISH% = TO_PERIOD% - 1%
		ELSE
			FINISH% = 11%
		END IF

18220		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(YEARS% - FROM_YEAR%), &
				REGARDLESS
		USE
			CONTINUE 18280
		END WHEN

		!
		! If this is the right type, then summarize the data
		!
		IF IC_35HISTORY::PRODUCT = PRODUCT$
		THEN
			IF IC_35HISTORY::TRANSTYPE = "SA" AND &
				IC_35HISTORY::LOCATION = LOCATION$
			THEN
				FOR LOOP% = START% TO FINISH%

					TOTAL = TOTAL - &
						IC_35HISTORY::PQUANTITY(LOOP%) * &
						FACTOR

				NEXT LOOP%
			END IF

			GOTO 18220
		END IF

18280	NEXT YEARS%

18290	RETURN

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
