1	%TITLE "Product Yield Report"
	%SBTTL "WP_RPRT_PRODYIELD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:WP0023
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Product Yield Report\*
	!
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_PRODYIELD/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_PRODYIELD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_PRODYIELD.OBJ;*
	!
	! Author:
	!
	!	02/22/93 - Dan Perkins
	!
	! Modification History:
	!
	!	02/26/93 - Dan Perkins
	!		Added Date range and also print actual yield for
	!		each line item.
	!
	!	06/07/93 - Frank F. Starman
	!		Print detail regardless od issue or requisition qty.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/06/93 - Frank F. Starman
	!		Set REQQTY as a ISSQTY if REQQTY is zero.
	!
	!	09/06/93 - Frank F. Starman
	!		Added BOM to calculata REQQTY.
	!
	!	10/19/93 - Frank F. Starman
	!		Increase dim of arrays to 200.
	!
	!	04/21/94 - Kevin Handy
	!		Modified to look at component instead of product
	!		when looking for item in BM tree.
	!
	!	06/29/94 - Kevin Handy
	!		Format to 80 columns.
	!
	!	09/15/94 - Kevin Handy
	!		Modified default FROM_DATE and TO_DATE used when
	!		no dates are given from the incorrect
	!		MMDDYYYY format to the correct YYYYMMDD format.
	!
	!	09/15/94 - Kevin Handy
	!		Removed a massive amount of commented out code.
	!
	!	11/09/94 - Kevin Handy
	!		Fixed calculation for REQQTY so it doesn't stop
	!		after the first one is found.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	DECLARE			JC_JOB_CDD		SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD	BM_RELATION

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DIM			WP_REGLINE_CDD		REGLINE(200%)

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ
	DIM			WP_REQREGISTER_CDD	REQREGISTER(200%)

	!
	! Declare external functions
	!
	EXTERNAL INTEGER FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL INTEGER FUNCTION WP_READ_REQREGISTER

	DIM RFA RFA_LEVEL(500%)
	DIM REAL QTY_LEVEL(500%)
	DIM STRING TEST_PRODUCT(500%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
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
	!	The value must be in agreement with the
	!	field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
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
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) From Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date with which
	!	the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	dated item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the date with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the most recent
	!	date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	DETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Line Detail (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Print Line Detail\* optionally displays
	!	the line detail pertaining to this product.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open Location File
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! Open Product File
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Open REGLINE file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open BM RELATION file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY%  = 0%
		ADD_TITLE$ = "BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY%  = 1%
		ADD_TITLE$ = "BY PRODUCT TYPE"

	CASE "C"
		SORT_KEY%  = 2%
		ADD_TITLE$ = "BY CATEGORY"

	CASE "D"
		SORT_KEY%  = 3%
		ADD_TITLE$ = "BY DESCRIPTION"

	CASE "S"
		SORT_KEY%  = 4%
		ADD_TITLE$ = "BY PRODUCT SECONDARY CODE"

	END SELECT

	TITLE$(1%) = "PRODUCT YIELD REPORT " + ADD_TITLE$
	TITLE$(3%) = "Work In Process System"

	TITLE$(4%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)

	TITLE$(4%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(4%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(4%) = "For All Dates" IF FROM_DATE$ = "" AND TO_DATE$ = ""

	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product        Description                  " + &
		"                     RawQty  FinQty  ActYield " + &
		" StdYield Variance"

	IF DETAIL$ = "Y"
	THEN
		TITLE$(7%) = "   Job#       Line CompDate   " + &
		"ReqNum     ReqLine Product"

		TITLE$(8%) = "."
	ELSE
		TITLE$(7%) = "."
	END IF

	!
	! Set some date ranges if FROM_DATE$
	! and TO_DATE$ are blank
	!
	FROM_DATE$ = "00010101" IF FROM_DATE$ = ""
	TO_DATE$   = "99993112" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	TEST_LOCATION$ = ""
	LIN% = 0%

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

	TITLE$(2%) = "FOR LOCATION " + UTL_LOCATION::LOCATION + " " + &
		EDIT$(UTL_LOCATION::LOCNAME, 32% + 128%)

	LIN% = 999% IF TEST_LOCATION$ <> ""

	TEST_LOCATION$ = UTL_LOCATION::LOCATION

17100	WHEN ERROR IN
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
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17120	!
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
	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO NextLocation IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

	END SELECT

	BUYQTY, REQQTY, ISSQTY = 0.0
	COUNTER% = 0%

	!
	! Get WP_REGLINE info to see how much was bought off
	!
17200	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #1% EQ "M" + PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegline:
17220	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLine IF WP_REGLINE::ITEMCODE <> PD_PRODUCT::PRODUCT_NUM

	!
	! See if the dates are in range
	!
	GOTO GetRegline IF WP_REGLINE::COMP_DATE < FROM_DATE$ OR &
		WP_REGLINE::COMP_DATE > TO_DATE$

	!
	! We are only interested in the BUYOFF which is record type 02
	!
	GOTO GetRegline IF WP_REGLINE::REC_TYPE <> "02"

	!
	! See if we can find the header for this line
	!
	GOTO GetRegline IF SB_EXAM_SUBACCOUNT("J", WP_REGLINE::JOB, &
		SB_SUBACCOUNT_EXAM) <> CMC$_NORMAL

	!
	! See if we are on the same location
	!
	GOTO GetRegline &
		IF SB_SUBACCOUNT_EXAM::LOCATION <> UTL_LOCATION::LOCATION

	!
	! See if this job has been closed
	!
	GOTO GetRegline IF SB_SUBACCOUNT_EXAM::SSTATUS <> "C"

	!
	! We are done if we have a zero quantity
	!
	IF WP_REGLINE::QTY = 0.0
	THEN
		GOTO GetRegline
	ELSE
		BUYQTY = BUYQTY + WP_REGLINE::QTY
	END IF

	!
	! See what this product is made of
	!
	! Get WP_REQREGISTER info to see how much was issued
	!
	REQNUMBER$ = "          "
	REQLINE$ = "    "

	IF WP_READ_REQREGISTER(WP_REGLINE::JOB, &
		WP_REGLINE::LLINE, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL
	THEN
		WP_REQREGISTER_READ::REQNUM = ""
		WP_REQREGISTER_READ::REQLIN = ""
		WP_REQREGISTER_READ::PRODUCT = ""
	END IF

	!
	! We are done if we have a zero quantity
	!
	QTY(1%) = QTY(2%) IF QTY(1%) = 0.0
	ISSQTY = ISSQTY + QTY(2%)

	GOTO GetRegline IF DETAIL$ <> "Y"

	!
	! Load array to print when we are done
	!
	COUNTER% = COUNTER% + 1%

	REGLINE(COUNTER%)     = WP_REGLINE
	REQREGISTER(COUNTER%) = WP_REQREGISTER_READ

	REGLINE(COUNTER%)::QTY     = WP_REGLINE::QTY
	REQREGISTER(COUNTER%)::QTY = QTY(2%)

	GOTO GetRegline

 PrintLine:
	GOSUB 17250
	GOTO GetNextRec IF (BUYQTY = 0.0) OR (ISSQTY = 0.0) OR (REQQTY = 0.0)

	ISSQTY = 1.0 IF ISSQTY = 0.0
	ACTYIELD = FUNC_ROUND(BUYQTY / ISSQTY * 100.0, 2%)

	REQQTY = 1.0 IF REQQTY = 0.0
	STDYIELD = FUNC_ROUND(1.0 / REQQTY * 100.0, 2%)

	VAR = ACTYIELD - STDYIELD

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + "         " + &
		FORMAT$(ISSQTY, "###,###") + " " + &
		FORMAT$(BUYQTY, "###,###") + "  " + &
		FORMAT$(ACTYIELD, "###.##%") + "  " + &
		FORMAT$(STDYIELD, "###.##%") + "  " + &
		FORMAT$(VAR, "####.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LIN% = 0%

	GOTO ExitPrintLine IF DETAIL$ <> "Y"

	FOR I% = 1% TO COUNTER%

		IF REQREGISTER(I%)::QTY = 0.0
		THEN
			ACTYIELD = 100.00
		ELSE
			ACTYIELD = FUNC_ROUND(REGLINE(I%)::QTY / &
				REQREGISTER(I%)::QTY * 100.0, 2%)
		END IF

		TEXT$ = "   " + &
			REGLINE(I%)::JOB + " " + &
			REGLINE(I%)::LLINE + " " + &
			PRNT_DATE(REGLINE(I%)::COMP_DATE, 8%) + " " + &
			REQREGISTER(I%)::REQNUM + " " + &
			REQREGISTER(I%)::REQLIN + "    " + &
			REQREGISTER(I%)::PRODUCT + " " + &
			FORMAT$(REQREGISTER(I%)::QTY, "###,###") + " " + &
			FORMAT$(REGLINE(I%)::QTY, "###,###") + "  " + &
			FORMAT$(ACTYIELD, "####.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

 ExitPrintLine:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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

17250	REQQTY = 0.0
	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE EndBOM IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	QTY_LEVEL(0%) = 1.0
	LEVEL% = 1%

 GoDownTree:

	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)


	IF BM_RELATION::COMPONENT = WP_REQREGISTER_READ::PRODUCT
	THEN
		REQQTY = REQQTY + QTY_LEVEL(LEVEL%)
		GOTO GoUpTree
	END IF

17320	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 17330 IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	LEVEL% = LEVEL% - 1%
	GOTO 17350 IF LEVEL% = 0%

17330	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

 EndBOM:
17350	RETURN

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
