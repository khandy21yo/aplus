1	%TITLE "Price Comparison"
	%SBTTL "PC_RPRT_PRICECOMP"
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
	! ID:PC001
	!
	! Abstract:HELP
	!	.p
	!	The ^*Price Comparison\* report prints price sheet on two selected
	!	dates and compares both amounts. This report will be useful to see the
	!	increase or decrease in price during a period. The following fields are
	!	included in this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Category
	!	.le
	!	Date 1
	!	.le
	!	Time 1
	!	.le
	!	Date 2
	!	.le
	!	Time 2
	!	.le
	!	Increase
	!	.els
	!
	! Index:
	!	.x Price Comparison>Report
	!	.x Report>Price Comparison
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_PRICECOMP/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_PRICECOMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_PRICECOMP.OBJ;*
	!
	! Author:
	!
	!	08/06/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	06/24/88 - Frank F. Starman
	!		Only for price (no for cost)
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/12/94 - Kevin Handy
	!		Fixed sort by secondary product to that to item
	!		would work.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/14/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP	(PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_PRICE

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
	!	.p
	!	The ^*From Item\* field causes the report
	!	to begin with a selected item number.
	!	.p
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!
	! Index:
	!	.x From Item>Price Comparison
	!	.x Price Comparison>From Item
	!	.x Item>Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with a selected item number.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!
	! Index:
	!	.x To Item>Price Comparison
	!	.x Price Comparison>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a ^*Wildcard\* selection.
	!
	! Index:
	!	.x Wildcard>Price Comparison
	!	.x Price Comparison>Wildcard
	!
	!--

	PRICE_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) Price Type\*
	!	.p
	!	The ^*Price Type\* field refers to the two character code which indicates
	!	which type of price category this price falls. The valid codes are entered
	!	in the Price Type Table.
	!
	! Index:
	!	.x Price Type
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.p
	!	The ^*Locations\* field enters a location code
	!	which is established in the Company Profile file which is located
	!	in the Utility system.
	!	.p
	!	This field will accommodate up to twenty (20) alphanumeric characters.
	!
	! Index:
	!	.x Locations>Price Comparison
	!	.x Price Comparison>Locations
	!
	!--

	TODATE1$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	!++
	! Abstract:FLD06
	!	^*(06) Date 1\*
	!	.p
	!	The ^*Date 1\* field enters the first of the dates which
	!	will be compared for price differences.
	!
	! Index:
	!	.x Date 1
	!
	!--

	TOTIME1$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	!++
	! Abstract:FLD07
	!	^*(07) Time 1\*
	!	.p
	!	The ^*Time 1\* field enters the first of the times which
	!	will be compared for price differences.
	!
	! Index:
	!	.x Time 1
	!
	!--

	TODATE2$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))
	!++
	! Abstract:FLD08
	!	^*(08) Date 2\*
	!	.p
	!	The ^*Date 2\* field enters the second of the dates which
	!	will be compared for price differences.
	!
	! Index:
	!	.x Date 2
	!
	!--

	TOTIME2$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)
	! Abstract:FLD09
	!	^*(09) Time 2\*
	!	.p
	!	The ^*Time 2\* field enters the second of the times which
	!	will be compared for price differences.
	!
	! Index:
	!	.x Time 2
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)
	!++
	! Abstract:FLD10
	!	^*(10) Sort (C,D,P,S,T)\*
	!	.p
	!	The ^*Sort\* field selects an order
	!	by which the report will print.
	!	.p
	!	Valid settings are:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	C = Product Category
	!	.le
	!	D = Product Description
	!	.le
	!	P = Product Number
	!	.le
	!	S = Product Secondary Code
	!	.le
	!	T = Product Type
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort>Price Comparison
	!	.x Price Comparison>Sort
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

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
		GET #PC_PRCTYPE.CH%, KEY #0% EQ PRICE_TYPE$, REGARDLESS
	USE
		PC_PRCTYPE::DESCRIPTION = &
			STRING$(LEN(PC_PRCTYPE::DESCRIPTION), A"?"B)

		CONTINUE ReportTitle IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRICE  COMPARISON  FOR  " + PC_PRCTYPE::DESCRIPTION
	TITLE$(3%) = "Product Price & Cost System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"                Tp Cat " + PRNT_DATE(TODATE1$, 8%) + &
		"(" + PRNT_TIME(TOTIME1$, 2048%) + &
		")   " + PRNT_DATE(TODATE2$, 8%) + "(" + &
		PRNT_TIME(TOTIME2$, 2048%) + ")      Increase"
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
				KEY #SORT_KEY% GE FROM_ITEM$, REGARDLESS
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
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	CASE "S"
		GOTO NextLocation &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	TEXT$ = ""

	PRICE1 = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, PRICE_TYPE$, TODATE1$, TOTIME1$, "", "")

	PRICE2 = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, PRICE_TYPE$, TODATE2$, TOTIME2$, "", "")

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + "     " + &
		FORMAT$(PRICE1, "#,###,###.##") + "        " + &
		FORMAT$(PRICE2, "#,###,###.##") + " " + &
		FORMAT$(PRICE2 - PRICE1, "<%>#,###,###.##") &

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
