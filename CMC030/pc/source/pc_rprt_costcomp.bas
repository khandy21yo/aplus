1	%TITLE "Cost Comparison"
	%SBTTL "PC_RPRT_COSTCOMP"
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
	! ID:PC011
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Cost Comparison\* report compares the standard
	!	costs between two selected dates. The two costs and the increase are then
	!	printed. This report includes the following fields:
	!	.table 3,25
	!	.te
	!	Product Number
	!	.te
	!	Description
	!	.te
	!	Type
	!	.te
	!	Category
	!	.te
	!	Date 1 Price
	!	.te
	!	Date 2 Price
	!	.te
	!	Increase
	!	.te
	!	Percent Change
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Standard Cost Comparison>Report
	!	.x Report>Standard Cost Comparison
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_COSTCOMP/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_COSTCOMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_COSTCOMP.OBJ;*
	!
	! Author:
	!
	!	06/28/88 - Frank Starman
	!
	! Modification History:
	!
	!	08/17/92 - Dan Perkins
	!		Added option to print line only if costs differ.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code
	!
	!	03/28/97 - Kevin Handy
	!		Add a percent change column.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/14/2002 - Kevin Handy
	!		Reformat source code
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	begins the report with a selected item number.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Cost Comparison
	!	.x Cost Comparison>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end with a selected item number.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Cost Comparison
	!	.x Cost Comparison>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a ^*Wildcard\* selection.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Cost Comparison
	!	.x Cost Comparison>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters a location code
	!	which is established in the Company Profile file which is located
	!	in the Utility system.
	!	.b
	!	This field will accommodate up to twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Cost Comparison
	!	.x Cost Comparison>Locations
	!
	!--

	TODATE1$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) Date 1\*
	!	.b
	!	.lm +5
	!	The ^*Date 1\* field enters the first of the dates which
	!	will be compared for cost differences.
	!	.lm -5
	!
	! Index:
	!	.x Date 1
	!
	!--

	TODATE2$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))

	!++
	! Abstract:FLD08
	!	^*(08) Date 2\*
	!	.b
	!	.lm +5
	!	The ^*Date 2\* field enters the second of the dates which
	!	will be compared for cost differences.
	!	.lm -5
	!
	! Index:
	!	.x Date 2
	!
	!--

	DIFFLAG$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Differing Costs Only (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Differing Costs Only\* field prints
	!	only those costs that differ from each other.
	!	.lm -5
	!
	! Index:
	!	.x Differing Costs Only>Cost Comparison
	!	.x Cost Comparison>Differing Costs Only
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort (C,D,P,S,T)\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field select an order
	!	by which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	C = Product Category
	!	.te
	!	D = Product Description
	!	.te
	!	P = Product Number
	!	.te
	!	S = Product Secondary Code
	!	.te
	!	T = Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Cost Comparison
	!	.x Cost Comparison>Sort
	!
	!--


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
		ADD_TITLE$ = "BY  SECONDARY NUMBER"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "STANDARD  COST  COMPARISON"
	TITLE$(3%) = "Product Price & Cost System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"                Tp Cat        " + PRNT_DATE(TODATE1$, 8%) + &
		"          " + PRNT_DATE(TODATE2$, 8%) + &
		"         Increase     %Change"
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

	TITLE$(2%) = "AT LOCATION " + TRM$(UTL_LOCATION::LOCATION) + " " + &
		TRM$(UTL_LOCATION::LOCNAME) + "  SORT  " + ADD_TITLE$

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
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$  = "PD_PRODUCT"
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
			TEST_CATEGORY$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 2%)
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
			TEST_PRODTYPE$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17300	TEXT$ = ""

	COST1 = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, TODATE1$, "")

	COST2 = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, TODATE2$, "")

	IF (COST1 = 0.0)
	THEN
		PERCNT = 0.0
	ELSE
		PERCNT = 100.0 * (COST2 - COST1) / COST1
	END IF

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + "  " + &
		FORMAT$(COST1, "#,###,###.#####     ") + &
		FORMAT$(COST2, "#,###,###.##### ") + &
		FORMAT$(COST2 - COST1, "<%>#,###,###.##### ") + &
		FORMAT$(PERCNT, "######.###%")

	LIN% = 1%
	IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
	THEN
		IF PRINT_FLAG%
		THEN
			LIN% = 999%
		END IF
		LAST_LOCATION$ = UTL_LOCATION::LOCATION
	END IF

	IF DIFFLAG$ = "N" OR COST1 <> COST2
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
		PRINT_FLAG% = -1%
		PRINT_LINE% = -1%
	END IF


17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

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
