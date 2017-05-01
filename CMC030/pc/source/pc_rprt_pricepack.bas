1	%TITLE "Product Price Sheet per Pack"
	%SBTTL "PC_RPRT_PRICEPACK"
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
	! ID:PC009
	!
	! Abstract:HELP
	!	.p
	!	The ^*Product Price per Pack Sheet\* prints the price
	!	for larger units of measure than by item. The information is taken
	!	from the Pack file, thus if there is no record in the pack file  this report
	!	will use the same information as the Price  per Unit report and they will
	!	print the same. The field included in this report are:
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
	!	Price
	!	.le
	!	Effective Date
	!	.le
	!	Form
	!	.le
	!	Pack Factor
	!	.le
	!	Unit of Measure
	!	.els
	!
	! Index:
	!	.x Product Price per Pack Sheet>Report
	!	.x Report>Product Price per Pack Sheet
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_PRICEPACK/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_PRICEPACK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_PRICEPACK.OBJ;*
	!
	! Author:
	!
	!	06/25/88 - Frank Starman
	!
	! Modification History:
	!
	!	02/26/92 - Kevin Handy
	!		Removed references to PD_PACK file that Frank
	!		deleted so programs couldn't compile.
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
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
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
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
	!	.x From Item>Price Sheet per Pack
	!	.x Price Sheet per Pack>From Item
	!	.x Item>From
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
	!	.x To Item>Price Sheet per Pack
	!	.x Price Sheet per Pack>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated items
	!	to be printed on the report by entering a ^*Wildcard\* selection.
	!
	! Index:
	!	.x Wildcard>Price Sheet per Pack
	!	.x Price Sheet per Pack>Wildcard
	!	.x Wildcard
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
	!	.x Locations>Price Sheet per Pack
	!	.x Price Sheet per Pack>Locations
	!
	!--

	EFF_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!++
	! Abstract:FLD06
	!	^*(06) Print Effective Date (Y/N)\*
	!	.p
	!	The ^*Print Effective Date\* field prints the date
	!	the price went into EFFECT_ A ^*Y\* input causes the report to
	!	include the effective date while a ^*N\* input causes the report to
	!	exclude the date.
	!
	! Index:
	!	.x Print Effective Date
	!
	!--

	ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	!++
	! Abstract:FLD07
	!	^*(07) Print Zero\*
	!	.p
	!	The ^*Print Zero\* field prints all units enabling
	!	the identification of the units missing a price by entering a ^*Y\* input.
	!	By entering a ^*N\*, those units containing a zero will be excluded from
	!	printing. This is necessary when products are purposely assigned a zero price.
	!
	! Index:
	!	.x Print Zero
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))
	!++
	! Abstract:FLD08
	!	^*(08) Date\*
	!	.p
	!	The ^*Date\* field enters the date of the time frame
	!	for which the assigned price is in question.
	!
	! Index:
	!	.x Date
	!
	!--

	TOTIME$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)
	!++
	! Abstract:FLD09
	!	^*(09) Time\*
	!	.p
	!	The ^*Time\* field enters the time of day in which
	!	the price goes into EFFECT_
	!
	! Index:
	!	.x Time
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
	!	.x Sort>Price Sheet per Pack
	!	.x Price Sheet per Pack>Sort
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

		CONTINUE 340 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PC_PRCTYPE"
		CONTINUE HelpError
	END WHEN

340 !	%INCLUDE "SOURCE:[PD.OPEN]PD_PACK.OPN"


 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  PRICE  PER  PACK  SHEET  FOR  " + &
		TRM$(PC_PRCTYPE::DESCRIPTION) + "  ON " + &
		PRNT_DATE(TODATE$, 8%) + &
		"(" + PRNT_TIME(TOTIME$, 2048%) + ")"

	TITLE$(3%) = "Product Price & Cost System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"                Tp Cat             Price" + &
		"     EffDate    Form PackFac UOM "
	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
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

	GOTO NextLocation &
		IF LOCATION$ <> "" AND &
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
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation &
			IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
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
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17300	PRICE = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, PRICE_TYPE$, &
		TODATE$,TOTIME$, EFFECT_DATE$, EFFECT_TIME$)

	GOTO 17350 IF PRICE = 0.0 AND ZERO$ = "N"

	E_DATE$ = "          "
	E_DATE$ = PRNT_DATE(EFFECT_DATE$, 8%) IF EFF_DATE$ = "Y"

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + "     " + &
		FORMAT$(PRICE, "#,###,###.##") + "     " + &
		E_DATE$

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
