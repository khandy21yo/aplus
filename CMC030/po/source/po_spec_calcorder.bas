1	%TITLE "Create Purchase Order Load Journal"
	%SBTTL "PO_SPEC_CALCORDER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:IC023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This procedure generates a Purchase order Load Journal by looking
	!	at various registers to determine what products need to
	!	be ordered to fill current requirements.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_SPEC_CALCORDER/LINE
	!	$ LINK/EXE=PO_EXE: PO_SPEC_CALCORDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_SPEC_CALCORDER.OBJ;*
	!
	! Author:
	!
	!	05/18/94 - Kevin Handy
	!		Borrowed from IC_RPRT_FREESTOCK with MANY changes.
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.HB"
	MAP (PO_CALCORDER)	PO_CALCORDER_CDD	PO_CALCORDER

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)	PO_CONTROL_CDD		PO_CONTROL

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
	!	The ^*Sort by\* setting determines
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
	!	The value must be in agreement with field
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
	!	The value must be in agreement with
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
	!	value in this field.
	!	.b
	!	The "Wildcard" value must be in agreement with
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
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Safety Stock Balances
	!	.x Safety Stock Balances>Locations
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
		%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.OPN"
		GET #PO_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE PO_CONTROL.CH%
	USE
		FILENAME$ = "PO_CONTROL"
		CONTINUE HelpError
	END WHEN

335	CALL READ_DEVICE("PO_CALCORDER",PO_CALCORDER.DEV$, STAT%)
 !	KILL PO_CALCORDER.DEV$ + "PO_CALCORDER.JRL"

	SMG_STATUS% = LIB$DELETE_FILE(PO_CALCORDER.DEV$ + "PO_CALCORDER.JRL;*")

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.CRE"
	USE
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PURCHASE ORDER LOAD FILE GENERATE " + ADD_TITLE$
	TITLE$(2%) = "."
	TITLE$(3%) = "Purchase Order System"
	TITLE$(4%) = ""

	TITLE$(5%) = "."
	TITLE$(6%) = ""

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

17010	IF FROM_ITEM$ = ""
	THEN
		WHEN ERROR IN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		USE
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		USE
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

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

17200	CALL PO_FUNC_CALCORDER(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, &
		"", &
		PO_CONTROL::LOAD_FORMULA, &
		PO_CALCORDER)

	IF PO_CALCORDER::ORDER > 0
	THEN
		PUT #PO_CALCORDER.CH%
		TOTAL_RECORDS% = TOTAL_RECORDS% + 1%
	END IF

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitProgram:
	TEXT$ = "Total of " + NUM1$(TOTAL_RECORDS%) + &
		" Lines Loaded"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

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
	!+-+-+
	!++
	! Abstract:FLD10
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* setting determines
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
