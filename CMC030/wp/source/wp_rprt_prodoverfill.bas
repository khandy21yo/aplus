1	%TITLE "Product Overfill Report"
	%SBTTL "WP_RPRT_PRODOVERFILL"
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
	! ID:WP0024
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Product Overfill Report\*
	!
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_PRODOVERFILL/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_PRODOVERFILL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_PRODOVERFILL.OBJ;*
	!
	! Author:
	!
	!	02/26/93 - Dan Perkins
	!
	! Modification History:
	!
	!	03/04/93 - Dan Perkins
	!		Added Pound Variance column to report.  Extended
	!		cost is now Cost times Cases.
	!
	!	03/10/93 - Dan Perkins
	!		Read cost at the time there is a good REQREGISTER
	!		quantity so we have the cost if we only print a
	!		summary report.
	!
	!	04/15/93 - Dan Perkins
	!		Added BUYQTY and EXTCOST totals to report.
	!
	!	05/14/93 - Dan Perkins
	!		Round weight variance to two decimal places instead
	!		of none.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/28/2003 - Kevin Handy
	!		Total finished weight.
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
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB_EXAM

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
	EXTERNAL LONG	FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL REAL   FUNCTION PC_READ_COST

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
	!	The value must be in agreement with
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
	!	items to be printed on the report by entering a "wildcard"
	!	value.
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
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date with which
	!	the report will begin printing.
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
	!	The ^*Print Line Detail\* displays
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

	TITLE$(1%) = "PRODUCT OVERFILL REPORT " + ADD_TITLE$
	TITLE$(3%) = "Work In Process System"

	TITLE$(4%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)

	TITLE$(4%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(4%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(4%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""

	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product        Description                  " + &
		"                     FinWgt   Cases  ActWgt  " + &
		"StdWgt   WtVar OverFill   StdCost   ExtCost"

	IF DETAIL$ = "Y"
	THEN
		TITLE$(7%) = "   Job#       CompDate   " + &
			"ReqNum     Product"

		TITLE$(8%) = "."
	ELSE
		TITLE$(7%) = "."
	END IF

	!
	! Set some date ranges if FROM_DATE$
	! and TO_DATE$ are blank
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	TEST_LOCATION$ = ""
	LIN% = 0%

	TOT_BUYQTY  = 0.0
	TOT_EXTCOST = 0.0
	TOT_ACTWGT = 0.0
	TOT_FINWGT = 0.0

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

	TITLE$(2%) = "FOR LOCATION " + UTL_LOCATION::LOCATION + " " + &
		EDIT$(UTL_LOCATION::LOCNAME, 32% + 128%)

	LIN% = 999% IF TEST_LOCATION$ <> ""

	TEST_LOCATION$ = UTL_LOCATION::LOCATION

17100	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
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
	!GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

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

	BUYQTY, ISSQTY = 0.0
	COUNTER% = 0%

	!
	! Get WP_REGLINE info to see how much was bought off
	!
17200	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #1% EQ "M" + PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 155%
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
		JC_JOB_EXAM) <> CMC$_NORMAL

	!
	! See if we are on the same location
	!
	GOTO GetRegline IF JC_JOB_EXAM::LOCATION <> UTL_LOCATION::LOCATION

	!
	! See if this job has been closed
	!
	GOTO GetRegline IF JC_JOB_EXAM::SSTATUS <> "C"

	!
	! We are done if we have a zero quantity
	!
	GOTO GetRegline IF FUNC_ROUND(WP_REGLINE::QTY, 5%) = 0.0

	!
	! See what this product is made of
	!
	! Get WP_REQREGISTER info to see how much was issued
	!
	REQNUMBER$ = "          "
	REQLINE$ = "    "

	GOTO GetRegline IF WP_READ_REQREGISTER(WP_REGLINE::JOB, &
		WP_REGLINE::LLINE, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	!
	! We are done if we have a zero quantity
	!
	GOTO GetRegline IF FUNC_ROUND(QTY(2%), 5%) = 0.0

	BUYQTY = BUYQTY + WP_REGLINE::QTY
	ISSQTY = ISSQTY + QTY(2%)

	COST = PC_READ_COST(WP_REQREGISTER_READ::PRODUCT, &
		UTL_LOCATION::LOCATION, WP_REGLINE::COMP_DATE, "")

	GOTO GetRegline IF DETAIL$ <> "Y"

	!
	! Load array to print when we are done
	!
	COUNTER% = COUNTER% + 1%

	REGLINE(COUNTER%) = WP_REGLINE
	REQREGISTER(COUNTER%) = WP_REQREGISTER_READ

	REGLINE(COUNTER%)::QTY = WP_REGLINE::QTY
	REQREGISTER(COUNTER%)::QTY = QTY(2%)

	GOTO GetRegline

 PrintLine:
	GOTO GetNextRec IF BUYQTY = 0.0 OR ISSQTY = 0.0

	TOT_BUYQTY = TOT_BUYQTY + BUYQTY
	ACTWGT = FUNC_ROUND(ISSQTY / BUYQTY, 2%)

	IF PD_PRODUCT::WEIGHT <> 0.0
	THEN
		STDWGT = PD_PRODUCT::WEIGHT
	ELSE
		STDWGT = 1.0
	END IF

	WTVAR = FUNC_ROUND(ISSQTY - (BUYQTY * STDWGT), 2%)

	VAR = FUNC_ROUND((ACTWGT - STDWGT) / STDWGT * 100.0, 2%)

	EXTCOST     = FUNC_ROUND(WTVAR * COST, 2%)
	TOT_EXTCOST = TOT_EXTCOST + EXTCOST
	TOT_ACTWGT = TOT_ACTWGT + ACTWGT
	TOT_FINWGT = TOT_FINWGT + ISSQTY

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + "         " + &
		FORMAT$(ISSQTY, "###,###") + " " + &
		FORMAT$(BUYQTY, "###,###") + "  " + &
		FORMAT$(ACTWGT, "###.##") + "  " + &
		FORMAT$(PD_PRODUCT::WEIGHT, "###.##") + "  " + &
		FORMAT$(WTVAR, "###.##") + "  " + &
		FORMAT$(VAR, "###.##%") + "  " + &
		FORMAT$(COST, "#,###.##") + "  " + &
		FORMAT$(EXTCOST, "#,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LIN% = 0%

	ISSQTY, BUYQTY, ACTWGT, WTVAR, VAR, COST, EXTCOST = 0.0

	GOTO ExitPrintLine IF DETAIL$ <> "Y"

	FOR I% = 1% TO COUNTER%

		IF REGLINE(I%)::QTY <> 0.0
		THEN
			BUYQTY = REGLINE(I%)::QTY
		ELSE
			BUYQTY = 1.0
		END IF

		ACTWGT = FUNC_ROUND(REQREGISTER(I%)::QTY/BUYQTY, 2%)

		WTVAR = FUNC_ROUND(REQREGISTER(I%)::QTY - (BUYQTY * STDWGT), 2%)

		VAR = FUNC_ROUND((ACTWGT - STDWGT) / STDWGT * 100.0, 2%)

		COST = PC_READ_COST(REQREGISTER(I%)::PRODUCT, &
			UTL_LOCATION::LOCATION, REGLINE(I%)::COMP_DATE, "")

		EXTCOST = FUNC_ROUND(WTVAR * COST, 2%)

		TEXT$ = "   " + REGLINE(I%)::JOB + " " + &
			PRNT_DATE(REGLINE(I%)::COMP_DATE, 8%) + " " + &
			CONV_STRING(REQREGISTER(I%)::REQNUM, CMC$_LEFT) + " " + &
			REQREGISTER(I%)::PRODUCT + SPACE$(14%) + &
			FORMAT$(REQREGISTER(I%)::QTY, "###,###") + " " + &
			FORMAT$(REGLINE(I%)::QTY, "###,###") + "  " + &
			FORMAT$(ACTWGT, "###.##") + "  " + &
			FORMAT$(PD_PRODUCT::WEIGHT, "###.##") + "  " + &
			FORMAT$(WTVAR, "###.##") + "  " + &
			FORMAT$(VAR, "###.##%") + "  " + &
			FORMAT$(COST, "#,###.##") + "  " + &
			FORMAT$(EXTCOST, "#,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		REQREGISTER(I%)::QTY = 0.0
		REGLINE(I%)::QTY     = 0.0
		ACTWGT, WTVAR, VAR, COST, EXTCOST = 0.0

	NEXT I%

 ExitPrintLine:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitTotal:
	TEXT$ = "Report Total" + SPACE$(53%) + &
		FORMAT$(TOT_FINWGT, "###,###") + &
		FORMAT$(TOT_BUYQTY, "###,###") + &
		FORMAT$(TOT_ACTWGT, "###,###") + &
		SPACE$(37%) + &
		FORMAT$(TOT_EXTCOST, "##,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAQME$ = ""
	RESUME HelpError

32767	END
