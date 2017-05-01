1	%TITLE "Product Yield Report"
	%SBTTL "WP_REPT_FINYIELD"
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
	!	$ BAS WP_SOURCE:WP_REPT_FINYIELD/LINE
	!	$ LINK/EXE=WP_EXE: WP_REPT_FINYIELD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_REPT_FINYIELD.OBJ;*
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
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/04/93 - Kevin Handy
	!		Changed internal program name from "WP_RPRT_PRODYIELD"
	!		to "WP_REPT_FINYIELD" so internal and external names
	!		would match.
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DIM			WP_REGLINE_CDD		REGLINE(100%)

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ
	DIM			WP_REQREGISTER_CDD	REQREGISTER(100%)

	!
	! Declare external functions
	!
	EXTERNAL INTEGER FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL INTEGER FUNCTION WP_READ_REQREGISTER

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT.BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* setting in this field will determine
	!	the order in which the report will print or be displayed.
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

	FROM.ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field is provided to enter a selected
	!	item from which the report will begin printing or displaying.
	!	The value entered must be in agreement with the value in field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO.ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field provides a means to enter an
	!	item number with which the report will end printing or displaying.
	!	The value entered must be in agreement with the value entered in
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
	!	The ^*Wildcard\* field provides the means to select
	!	items to be printed on the report by entering a "wildcard"
	!	value in this field.
	!	.b
	!	The "Wildcard" value entered must be in agreement with the value
	!	in field (01) Sort by.
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
	!	The ^*Locations\* field provides the means to enter the locations
	!	codes (which have been established in the Utility system) that are
	!	to be printed or displayed in the report.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM.DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field provides a means to enter the date with which
	!	the report will begin printing or displaying.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	dated item in the file.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO.DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field provides a means to enter the date with which the
	!	report will end printing or displaying.
	!	.b
	!	A blank field will cause the report to end with the most recent
	!	date in the file.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
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
	!	The ^*Print Line Detail\* allows the user the option to display
	!	the line detail pertaining to this product.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open Location File
	!
300	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"

	!
	! Open Product File
	!
310	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

	!
	! Open REGLINE file
	!
320	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"

 ReportTitle:
	!
	! Title
	!
	SELECT SORT.BY$

	CASE "P"
		SORT.KEY%  = 0%
		ADD.TITLE$ = "BY PRODUCT NUMBER"

	CASE "T"
		SORT.KEY%  = 1%
		ADD.TITLE$ = "BY PRODUCT TYPE"

	CASE "C"
		SORT.KEY%  = 2%
		ADD.TITLE$ = "BY CATEGORY"

	CASE "D"
		SORT.KEY%  = 3%
		ADD.TITLE$ = "BY DESCRIPTION"

	CASE "S"
		SORT.KEY%  = 4%
		ADD.TITLE$= "BY PRODUCT SECONDARY CODE"

	END SELECT

	TITLE$(1%) = "PRODUCT YIELD REPORT " + ADD.TITLE$
	TITLE$(3%) = "Work In Process System"

	TITLE$(4%) = "From " + PRNT_DATE(FROM.DATE$,8%) + " To " + &
		PRNT_DATE(TO.DATE$, 8%)

	TITLE$(4%) = "Before " + PRNT_DATE(TO.DATE$, 8%) IF FROM.DATE$ = ""
	TITLE$(4%) = "After " + PRNT_DATE(FROM.DATE$, 8%) IF TO.DATE$ = ""
	TITLE$(4%) = "For All Dates" IF FROM.DATE$ + TO.DATE$ = ""

	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product        Description                  "  + &
		"                     RawQty  FinQty ActYield " + &
		"StdYield Variance"

	IF DETAIL$ = "Y"
	THEN
		TITLE$(7%) = "   Job#       Line CompDate   " + &
			"ReqNum     ReqLine Product"

		TITLE$(8%) = "."
	ELSE
		TITLE$(7%) = "."
	END IF

	!
	! Set some date ranges if FROM.DATE$
	! and TO.DATE$ are blank
	!
	FROM.DATE$ = "01010001" IF FROM.DATE$ = ""
	TO.DATE$   = "31129999" IF TO.DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	TEST_LOCATION$ = ""
	LIN% = 0%

	RESET #UTL_LOCATION.CH%

 NextLocation:
	GET #UTL_LOCATION.CH%, REGARDLESS

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION,-1%), LOCATION$) = 0%

	TITLE$(2%) = "FOR LOCATION " + UTL_LOCATION::LOCATION + " " + &
		EDIT$(UTL_LOCATION::LOCNAME, 32%+128%)

	LIN% = 999% IF TEST_LOCATION$ <> ""

	TEST_LOCATION$ = UTL_LOCATION::LOCATION

17100	IF FROM.ITEM$ = ""
	THEN
		RESET #PD_PRODUCT.CH%, KEY#SORT.KEY%
	ELSE
		FIND #PD_PRODUCT.CH%, KEY#SORT.KEY% GE FROM.ITEM$, REGARDLESS
	END IF

 GetNextRec:
17120	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	GET #PD_PRODUCT.CH%, REGARDLESS

	!
	! Check current record
	!
	!GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT.BY$

	CASE "C"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO.ITEM$) AND &
			TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY,-1%), &
			WLDCRD$) = 0%

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION,-1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM,-1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO NextLocation IF (PD_PRODUCT::SECONDARY_CODE > TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE,-1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO.ITEM$) &
			AND TO.ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE,-1%), &
			WLDCRD$) = 0%

	END SELECT

	BUYQTY, REQQTY, ISSQTY = 0.0
	COUNTER% = 0%

	!
	! Get WP_REGLINE info to see how much was bought off
	!
17200	FIND #WP_REGLINE.CH%, KEY#1% EQ "M" + &
		PD_PRODUCT::PRODUCT_NUM, REGARDLESS

 GetRegline:
17220	GET #WP_REGLINE.CH%, REGARDLESS

	GOTO PrintLine IF WP_REGLINE::ITEMCODE <> PD_PRODUCT::PRODUCT_NUM

	!
	! See if the dates are in range
	!
	GOTO GetRegline IF WP_REGLINE::COMP_DATE < FROM.DATE$ OR &
		WP_REGLINE::COMP_DATE > TO.DATE$

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
	GOTO GetRegline IF SB_SUBACCOUNT_EXAM::LOCATION <> UTL_LOCATION::LOCATION

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
	REQNUMBER$        = "          "
	REQLINE$          = "    "

	GOTO GetRegline IF WP_READ_REQREGISTER(WP_REGLINE::JOB, &
		WP_REGLINE::LLINE, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	!
	! We are done if we have a zero quantity
	!
	IF QTY(1%) = 0.0 OR QTY(2%) = 0.0
	THEN
		GOTO GetRegline
	ELSE
		REQQTY = REQQTY + QTY(1%)
		ISSQTY = ISSQTY + QTY(2%)
	END IF

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
	GOTO GetNextRec IF (BUYQTY = 0.0) OR (ISSQTY = 0.0) OR (REQQTY = 0.0)

	ISSQTY = 1.0 IF ISSQTY = 0.0
	ACTYIELD = FUNC_ROUND(BUYQTY/ISSQTY * 100.0, 2%)

	REQQTY = 1.0 IF REQQTY = 0.0
	STDYIELD = FUNC_ROUND(BUYQTY/REQQTY * 100.0, 2%)

	VAR = ACTYIELD - STDYIELD

	TEXT$ = PD_PRODUCT::PRODUCT_NUM      + " "          + &
		PD_PRODUCT::DESCRIPTION      + "         "  + &
		FORMAT$(ISSQTY, "###,###")   + " "          + &
		FORMAT$(BUYQTY, "###,###")   + "  "         + &
		FORMAT$(ACTYIELD, "###.##%") + "  "         + &
		FORMAT$(STDYIELD, "###.##%") + "  "         + &
		FORMAT$(VAR, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LIN% = 0%

	GOTO ExitPrintLine IF DETAIL$ <> "Y"

	FOR I% = 1% TO COUNTER%

		REQREGISTER(I%)::QTY = 1.0 IF REQREGISTER(I%)::QTY = 0.0

		ACTYIELD = FUNC_ROUND(REGLINE(I%)::QTY/REQREGISTER(I%)::QTY * &
			100.0, 2%)

		TEXT$ = "   " + REGLINE(I%)::JOB                 + " "    + &
			REGLINE(I%)::LLINE                       + " "    + &
			PRNT_DATE(REGLINE(I%)::COMP_DATE, 8%)    + " "    + &
			REQREGISTER(I%)::REQNUM                  + " "    + &
			REQREGISTER(I%)::REQLIN                  + "    " + &
			REQREGISTER(I%)::PRODUCT                 + " "    + &
			FORMAT$(REQREGISTER(I%)::QTY, "###,###") + " "    + &
			FORMAT$(REGLINE(I%)::QTY, "###,###")	+ "  "   + &
			FORMAT$(ACTYIELD, "###.##%")

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

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	SELECT ERR
	CASE 154%
		!
		! Wait for 5 seconds if record is locked
		!
		SLEEP 5%
		RESUME
	END SELECT

	SELECT ERL

	CASE 300%
		!
		! Exit if can't open LOCATION file
		!
		FILENAME$ = "UTL_LOCATION"

	CASE 310%
		!
		! Exit if can't open PRODUCT file
		!
		FILENAME$ = "PD_PRODUCT"

	CASE 320%
		!
		! Exit if can't open REGLINE file
		!
		FILENAME$ = "WP_REGLINE"

		!
		! End of LOCATION file
		!
	CASE 17000%
		RESUME ExitProgram IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"

		!
		! Can't find PRODUCT record
		!
	CASE 17100%
		RESUME NextLocation IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"

		!
		! End of PRODUCT file
		!
	CASE 17120%
		RESUME NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"

		!
		! Can't find REGLINE record
		!
	CASE 17200%
		RESUME GetNextRec IF ERR = 155%
		FILENAME$ = "WP_REGLINE"

		!
		! End of REGLINE file
		!
	CASE 17220%
		RESUME PrintLine IF ERR = 11%
		FILENAME$ = "WP_REGLINE"

	END SELECT

	!
	! Resume to display untrapped error
	!
	RESUME HelpError

32767	END
