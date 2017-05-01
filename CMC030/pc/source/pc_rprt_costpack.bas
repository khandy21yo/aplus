1	%TITLE "Product Standard Cost Sheet per Pack"
	%SBTTL "PC_RPRT_COSTPACK"
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
	! ID:PC019
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Standard Cost per Pack Sheet\* prints the standard
	!	cost for larger units of measure than by item. The information is taken
	!	from the Pack file, thus if there is no record in the pack file this report
	!	will use the same information as the Cost per Unit report and will
	!	print the same. The fields included in this report are:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Type	Category
	!	.te
	!	Cost	Effective Date
	!	.te
	!	Form	Pack Factor
	!	.te
	!	Unit of Measure
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Standard Cost per Pack Sheet>Report
	!	.x Report>Standard Cost per Pack Sheet
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_COSTPACK/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_COSTPACK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_COSTPACK.OBJ;*
	!
	! Author:
	!
	!	06/25/88 - Frank Starman
	!
	! Modification History:
	!
	!	02/26/92 - Kevin Handy
	!		Modified to remove references to PD_PACK file that
	!		Frank deleted so programs couldn't compile.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

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
	!	begins the report with a selected item number.  The value entered must
	!	be in agreement with the value in field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Standard Cost Sheet per Pack
	!	.x Product Standard Cost Sheet per Pack>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	ends the report  with a selected item number.  The value entered must
	!	be in agreement with the value in field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Standard Cost Sheet per Pack
	!	.x Product Standard Cost Sheet per Pack>To Item
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
	!	.x Wildcard>Product Standard Cost Sheet per Pack
	!	.x Product Standard Cost Sheet per Pack>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters a location code
	!	which is established in the Company Profile file.
	!	.b
	!	This field will accommodate up to twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Product Standard Cost Sheet per Pack
	!	.x Product Standard Cost Sheet per Pack>Locations
	!
	!--

	EFF_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!++
	! Abstract:FLD06
	!	^*(06) Print Effective Date (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Print Effective Date\* field prints the date
	!	the standard cost went into effect. A ^*Y\* input causes the report to
	!	include the effective date while a ^*N\* input causes the report to
	!	exclude the date.
	!	.lm -5
	!
	! Index:
	!	.x Print Effective Date
	!
	!--

	ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	!++
	! Abstract:FLD07
	!	^*(07) Print Zero\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero\* field
	!	prints all units enabling
	!	the identification of the units missing a cost by entering a ^*Y\* input.
	!	By entering a ^*N\*, those units containing a zero will be excluded. This
	!	is necessary when products are purposely assigned a zero cost.
	!	.lm -5
	!
	! Index:
	!	.x Print Zero
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))
	!++
	! Abstract:FLD08
	!	^*(08) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date for the
	!	assigned cost.
	!	.lm -5
	!
	! Index:
	!	.x Date
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)
	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field selects an order
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
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Product Standard Cost Sheet per Pack
	!	.x Product Standard Cost Sheet per Pack>Sort
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

340 !	%INCLUDE "SOURCE:[PD.OPEN]PD_PACK.OPN"


 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  STANDARD  COST  PER  PACK  SHEET  ON " + &
		PRNT_DATE(TODATE$, 8%)
	TITLE$(3%) = "Product Price & Cost System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"                Tp Cat              Cost" + &
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
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0%

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

17300	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, TODATE$, EFFECT_DATE$)

	GOTO 17350 IF COST = 0.0 AND ZERO$ = "N"

	E_DATE$ = "          "
	E_DATE$ = PRNT_DATE(EFFECT_DATE$, 8%) IF EFF_DATE$ = "Y"

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + "     " + &
		FORMAT$(COST, "#,###,###.##") + "     " + &
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
