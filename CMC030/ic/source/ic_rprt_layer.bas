1	%TITLE "Product Layer Quantities"
	%SBTTL "IC_RPRT_LAYER"
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
	! ID:IC037
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Accessing the ^*Product Layer Quantities\* report setting screen
	!	will provide a report which will contain the following information:
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
	!	Cost Method
	!	.te
	!	Date
	!	.te
	!	Quantity
	!	.te
	!	Cost
	!	.te
	!	Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Layer Quantities>Report
	!	.x Report>Product Layer Quantities
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_LAYER/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_LAYER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_LAYER.OBJ;*
	!
	! Author:
	!
	!	06/23/88 - Frank Starman
	!
	! Modification History:
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
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
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/20/2000 - Kevin Handy
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
	MAP	(IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_LAYER.HB"
	MAP	(IC_LAYER)	IC_LAYER_CDD		IC_LAYER

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

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
	!	The ^*From Item\* field causes the
	!	report to begin printing with a selected item number.
	!	The value entered must be in agreement with
	!	field (10) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Layer Quantities
	!	.x Product Layer Quantities>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the report to end printing with a selected item number.
	!	The value entered must be in agreement with
	!	field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Layer Quantities
	!	.x Product Layer Quantities>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to print by using the wildcard techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Layer Quantities Report
	!	.x Product Layer Quantities Report>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

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
	!	.x Locations>Product Layer Quantities Report
	!	.x Product Layer Quantities Report>Locations
	!
	!--

	ONLY_CTD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field prints the
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
	!	.x Sort By>Product Layer Quantitiess Report
	!	.x Product Layer Quantities Report>Sort By
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

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"
	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD
	CLOSE IC_CONTROL.CH%

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
		%INCLUDE "SOURCE:[IC.OPEN]IC_LAYER.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "IC_LAYER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  LAYER  QUANTITIES  " + ADD_TITLE$
	TITLE$(3%) = "Inventory control system"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"       Tp Cat  CostMethod Date             " + &
		"Quantity         Cost        Amount "
	TITLE$(6%) = "               Check#  VendorName       Invoice "
	TITLE$(7%) = "."

	! LYT.LINE$  More that one line per record

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
		TRM$(PERIOD_DESCR$) + &
		"  AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

17010	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, KEY #SORT_KEY% GE FROM_ITEM$, &
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

	END SELECT

	TEXT1$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::METHOD + "       "

17200	WHEN ERROR IN
		FIND #IC_LAYER.CH%, &
			KEY #0% GE PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION + START.DATE$, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 9% OR ERR = 155%
		FILENAME$ = "IC_LAYER"
		CONTINUE HelpError
	END WHEN

17250	WHEN ERROR IN
		GET #IC_LAYER.CH%, REGARDLESS
	USE
		CONTINUE ProdTotal IF ERR = 11%
		FILENAME$ = "IC_LAYER"
		CONTINUE HelpError
	END WHEN

	GOTO ProdTotal IF IC_LAYER::PRODUCT <> PD_PRODUCT::PRODUCT_NUM OR &
		IC_LAYER::LOCATION <> UTL_LOCATION::LOCATION

	!
	! Print out one line
	!
	TEXT$ = TEXT1$ + &
		PRNT_DATE(IC_LAYER::TRANSDATE, 8%) + " " + &
		FORMAT$(IC_LAYER::QUANTITY, "##,###,###,###") + " " + &
		FORMAT$(IC_LAYER::COST, "#,###,###.##") + " " + &
		FORMAT$(IC_LAYER::QUANTITY * IC_LAYER::COST, "##,###,###.##")

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

	TEXT$ = SPACE$(15%) + IC_LAYER::CHECK + "  " + &
		IC_LAYER::VENDORALF + "  " + &
		IC_LAYER::INVOICE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	PRINT_FLAG% = -1%
	PRINT_LINE% = -1%
	TEXT1$ = PD_PRODUCT::PRODUCT_NUM + &
		SPACE$(LEN(TEXT1$) - LEN(PD_PRODUCT::PRODUCT_NUM))

	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO 17250

 ProdTotal:
17350	!
	! Try for next record
	!
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
