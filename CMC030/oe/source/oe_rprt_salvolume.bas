1	%TITLE "Sales Volume by Salesman/Opr Report"
	%SBTTL "OE_RPRT_SALVOLUME"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:OE033
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Volume by Salesman/Opr\* Report contains columns
	!	for the following information:
	!	.table 3,25
	!	.te
	!	Salesman/Operator Number
	!	.te
	!	Salesman/Operator Description
	!	.te
	!	Salesman/Operator Type
	!	.te
	!	Salesman/Operator Class
	!	.te
	!	Sales
	!	.te
	!	Cost
	!	.te
	!	Sales Margin
	!	.te
	!	Margin Percentage
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Salesman/Operator Daily Sales
	!	.x Salesman/Operator Daily Sales>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SALVOLUME/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SALVOLUME, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SALVOLUME.OBJ;*
	!
	! Author:
	!
	!	05/27/92 - Dan Perkins
	!
	! Modification History:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	!
	! Arrays for location stuff
	!
	DECLARE STRING LOCATION(50%)
	DECLARE REAL   LOCDAYSALE(50%)
	DECLARE REAL   LOCDAYCOST(50%)
	DECLARE REAL   LOCTOTSALE(50%)
	DECLARE REAL   LOCTOTCOST(50%)

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION UTL_EXAM_LOCATION

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort by
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*S#\* - Salesman Number
	!	.te
	!	^*T#\* - Salesman Type
	!	.te
	!	^*C#\* - Salesman Class
	!	.te
	!	^*O#\* - Operator
	!	.te
	!	^*OT\* - Operator Type
	!	.te
	!	^*OC\* - Operator Class
	!	.end table
	!	A setting is required in this field.
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
	!	The ^*From Item\* field enters the
	!	item with which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
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
	!	The ^*To Item\* field specifies the item
	!	with which the report is to end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects particular items to be printed
	!	by entering a "wildcard".  The value entered must be in agreement
	!	with field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field
	!	selects specific locations to be printed
	!	by entering a "wildcard".
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.x From Date>Salesman Daily Sales Rport
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date from which the report
	!	is to begin printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
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
	!	.x To Date>Salesman Daily Sales Rport
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies the date with which the
	!	report will end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the most recent date
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Register Header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Register Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Temp file
	!
	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	SELECT SORTBY$

	CASE  "S", "T", "C"

		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				MAP OE_REGHEADER, &
				BUFFER 32%, &
				PRIMARY KEY ( OE_REGHEADER::SALESMAN, &
					OE_REGHEADER::ORDNUM), &
				ACCESS MODIFY, ALLOW NONE
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	CASE  "O", "OT", "OC"

		WHEN ERROR IN
			OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE OE_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				MAP OE_REGHEADER, &
				BUFFER 32%, &
				PRIMARY KEY ( OE_REGHEADER::OPERATOR, &
					OE_REGHEADER::ORDNUM), &
				ACCESS MODIFY, ALLOW NONE
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	END SELECT

330	!
	! Open Subaccount file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "S"
		K_NUM% = 0%
		T_STRING$ = "Salesman#  "
		TITLE$(1%) = "DAILY SALES BY SALESMAN NUMBER"

	CASE "T"
		K_NUM% = 1%
		T_STRING$ = "Salesman#  "
		TITLE$(1%) = "DAILY SALES BY SALESMAN TYPE"

	CASE "C"
		K_NUM% = 2%
		T_STRING$ = "Salesman#  "
		TITLE$(1%) = "DAILY SALES BY SALESMAN CLASS"

	CASE "O"
		K_NUM% = 0%
		T_STRING$ = "Operator#  "
		TITLE$(1%) = "DAILY SALES BY OPERATOR NUMBER"

	CASE "OT"
		K_NUM% = 1%
		T_STRING$ = "Operator#  "
		TITLE$(1%) = "DAILY SALES BY OPERATOR TYPE"

	CASE "OC"
		K_NUM% = 2%
		T_STRING$ = "Operator#  "
		TITLE$(1%) = "DAILY SALES BY OPERATOR CLASS"

	END SELECT

	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TO_DATE$ = DATE_TODAY IF TO_DATE$ = ""

	IF FROM_DATE$ <> ""
	THEN
		DATE_LINE$ = PRNT_DATE(FROM_DATE$, 8%)
	ELSE
		FROM_DATE$ = "01010001"
		DATE_LINE$ = SPACE$(6%)
	END IF

	TITLE$(4%) = SPACE$(58%) + PRNT_DATE(TO_DATE$, 8%) + &
		SPACE$(32%) + &
		DATE_LINE$ + " To " + PRNT_DATE(TO_DATE$, 8%)

	TITLE$(5%) = T_STRING$ + "Name                 Tp Clas "            + &
		"     Sales       Cost     Margin   Gross%          "  + &
		"     Sales       Cost     Margin   Gross%"

	TITLE$(6%) = "."

	!
	! Initialize some variables
	!
	DAYSUBCOST = 0.0
	DAYSUBSALE = 0.0

	SUBCOST = 0.0
	SUBSALE = 0.0

	DAYTOTALCOST = 0.0
	DAYTOTALSALE = 0.0

	TOTALCOST = 0.0
	TOTALSALE = 0.0

	PRINT_FLAG%  = 0%
	LOC_COUNTER% = 0%

	%PAGE

17000	!***************************************************************
	! BUILD A TEMP FILE
	!***************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	WHEN ERROR IN
		RESET #OE_REGHEADER.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Header record
	!
17020	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF COMP_STRING(EDIT$( &
		OE_REGHEADER::LOCATION, -1%), LOC_WLDCRD$) = 0% &
		AND LOC_WLDCRD$ <> ""

17030	WHEN ERROR IN
		PUT #OE_TEMP.CH%
	USE
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

17100	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SA_SALESMAN"
		CONTINUE HelpError
	END WHEN

 GetSalRec:
	!
	! Get Salesman record
	!
17120	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SA_SALESMAN"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram IF SA_SALESMAN::SUBJECT <> "S"

	SELECT SORTBY$
	CASE "C", "OC"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetSalRec IF COMP_STRING(EDIT$( &
			SA_SALESMAN::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S", "O"
		GOTO ExitTotal IF (SA_SALESMAN::SALESMAN > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetSalRec IF COMP_STRING(EDIT$( &
			SA_SALESMAN::SALESMAN, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T", "OT"
		GOTO ExitTotal IF (SA_SALESMAN::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetSalRec IF COMP_STRING(EDIT$( &
			SA_SALESMAN::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Find the right Temp record
	!
17200	WHEN ERROR IN
		FIND #OE_TEMP.CH%, KEY #0% EQ SA_SALESMAN::SALESMAN, REGARDLESS
	USE
		CONTINUE GetSalRec IF ERR = 155%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

 GetTempRec:
17220	WHEN ERROR IN
		GET #OE_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTemp IF ERR = 11%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "S", "T", "C"
		GOTO ExitTemp IF OE_REGHEADER::SALESMAN <> &
			SA_SALESMAN::SALESMAN

	CASE "O", "OT", "OC"
		GOTO ExitTemp IF OE_REGHEADER::OPERATOR <> &
			SA_SALESMAN::SALESMAN
	END SELECT

	!
	! Check out the lines for this Temp record
	!
17300	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% EQ OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE GetTempRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegline:
17320	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE GetTempRec IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetTempRec IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	GOTO GetRegLine IF OE_REGLINE::TRANTYPE <> "02"

	GOTO GetRegline IF OE_REGLINE::QTY = 0.0

	GOTO GetRegline IF OE_REGLINE::TDATE < FROM_DATE$ OR &
		OE_REGLINE::TDATE > TO_DATE$

	!
	! Play games with the promo, discount, etc.
	!
	LINECOST = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::COST, 2%)

	LINESALE = FUNC_ROUND(OE_REGLINE::QTY * &
		((OE_REGLINE::PRICE - OE_REGLINE::PROMO) * &
		(1 - (OE_REGLINE::DISCOUNT / 100)) + &
		OE_REGLINE::MISCH + OE_REGLINE::MISCH2), 2%)

	SUBCOST = SUBCOST + LINECOST
	SUBSALE = SUBSALE + LINESALE

	FOR I% = 1% TO LOC_COUNTER%

		GOTO ExitLoop IF LOCATION(I%) = OE_REGHEADER::LOCATION
	NEXT I%

	I%, LOC_COUNTER% = LOC_COUNTER% + 1%

	LOCDAYSALE(LOC_COUNTER%) = 0.0
	LOCDAYCOST(LOC_COUNTER%) = 0.0
	LOCTOTSALE(LOC_COUNTER%) = 0.0
	LOCTOTCOST(LOC_COUNTER%) = 0.0

 ExitLoop:
	LOCATION(I%)   = OE_REGHEADER::LOCATION
	LOCTOTSALE(I%) = LOCTOTSALE(I%) + LINESALE
	LOCTOTCOST(I%) = LOCTOTCOST(I%) + LINECOST

	IF OE_REGLINE::TDATE = TO_DATE$
	THEN
		DAYSUBCOST = DAYSUBCOST + LINECOST
		DAYSUBSALE = DAYSUBSALE + LINESALE

		LOCDAYSALE(I%) = LOCDAYSALE(I%) + LINESALE
		LOCDAYCOST(I%) = LOCDAYCOST(I%) + LINECOST
	END IF

	PRINT_FLAG% = -1%

	GOTO GetRegLine

 ExitTemp:
	GOSUB PrintLine IF PRINT_FLAG%
	GOTO GetSalRec

 ExitTotal:
	GOSUB PrintLine IF PRINT_FLAG%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(11%) + &
		"Location Totals:"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR I% = 1% TO LOC_COUNTER%

		V% = UTL_EXAM_LOCATION(LOCATION(I%), UTL_LOCATION_EXAM)

		IF LOCDAYSALE(I%) = 0.0
		THEN
			DAYDIVIDE = 1.0
		ELSE
			DAYDIVIDE = LOCDAYSALE(I%)
		END IF

		IF LOCTOTSALE(I%) = 0.0
		THEN
			SUBDIVIDE = 1.0
		ELSE
			SUBDIVIDE = LOCTOTSALE(I%)
		END IF

		DAYGROSS = LOCDAYSALE(I%) - LOCDAYCOST(I%)
		DAYGROSSPERCENT = FUNC_ROUND((DAYGROSS / DAYDIVIDE) * 100.0, 2%)

		GROSS = LOCTOTSALE(I%) - LOCTOTCOST(I%)
		GROSSPERCENT = FUNC_ROUND((GROSS / SUBDIVIDE) * 100.0, 2%)

		TEXT$ = LOCATION(I%) + " " + &
			LEFT(UTL_LOCATION_EXAM::LOCNAME, 20%) + " " + &
			SPACE$(14%) + &
			FORMAT$(LOCDAYSALE(I%), "#######.##") + " " + &
			FORMAT$(LOCDAYCOST(I%), "#######.##") + " " + &
			FORMAT$(DAYGROSS, "#######.##") + " " + &
			FORMAT$(DAYGROSSPERCENT, "####.##%") + &
			SPACE$(10%) + &
			FORMAT$(LOCTOTSALE(I%), "#######.##") + " " + &
			FORMAT$(LOCTOTCOST(I%), "#######.##") + " " + &
			FORMAT$(GROSS, "#######.##") + " " + &
			FORMAT$(GROSSPERCENT, "####.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	IF DAYTOTALSALE = 0.0
	THEN
		DAYDIVIDE = 1.0
	ELSE
		DAYDIVIDE = DAYTOTALSALE
	END IF

	IF TOTALSALE = 0.0
	THEN
		SUBDIVIDE = 1.0
	ELSE
		SUBDIVIDE = TOTALSALE
	END IF

	DAYGROSS = DAYTOTALSALE - DAYTOTALCOST
	DAYGROSSPERCENT = FUNC_ROUND((DAYGROSS / DAYDIVIDE) * 100.0, 2%)

	GROSS = TOTALSALE - TOTALCOST
	GROSSPERCENT = FUNC_ROUND((GROSS / SUBDIVIDE) * 100.0, 2%)

	TEXT$ = SPACE$(11%) + &
		"Grand Totals:" + &
		SPACE$(16%) + &
		FORMAT$(DAYTOTALSALE, "#######.##") + " " + &
		FORMAT$(DAYTOTALCOST, "#######.##") + " " + &
		FORMAT$(DAYGROSS, "#######.##") + " " + &
		FORMAT$(DAYGROSSPERCENT, "####.##%") + &
		SPACE$(10%) + &
		FORMAT$(TOTALSALE, "#######.##") + " " + &
		FORMAT$(TOTALCOST, "#######.##") + " " + &
		FORMAT$(GROSS, "#######.##") + " " + &
		FORMAT$(GROSSPERCENT, "####.##%")

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

 PrintLine:
	IF DAYSUBSALE = 0.0
	THEN
		DAYDIVIDE = 1.0
	ELSE
		DAYDIVIDE = DAYSUBSALE
	END IF

	IF SUBSALE = 0.0
	THEN
		SUBDIVIDE = 1.0
	ELSE
		SUBDIVIDE = SUBSALE
	END IF

	DAYGROSS = DAYSUBSALE - DAYSUBCOST
	DAYGROSSPERCENT = FUNC_ROUND((DAYGROSS / DAYDIVIDE) * 100.0, 2%)

	GROSS = SUBSALE - SUBCOST
	GROSSPERCENT = FUNC_ROUND((GROSS / SUBDIVIDE) * 100.0, 2%)

	TEXT$ = SA_SALESMAN::SALESMAN + " " + &
		LEFT(SA_SALESMAN::DESCR, 20%) + " " + &
		SA_SALESMAN::TTYPE + " " + &
		SA_SALESMAN::CLASS + " " + &
		FORMAT$(DAYSUBSALE, "#######.##") + " " + &
		FORMAT$(DAYSUBCOST, "#######.##") + " " + &
		FORMAT$(DAYGROSS, "#######.##") + " " + &
		FORMAT$(DAYGROSSPERCENT, "####.##%") + &
		SPACE$(10%) + &
		FORMAT$(SUBSALE, "#######.##") + " " + &
		FORMAT$(SUBCOST, "#######.##") + " " + &
		FORMAT$(GROSS, "#######.##") + " " + &
		FORMAT$(GROSSPERCENT, "####.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	DAYTOTALCOST = DAYTOTALCOST + DAYSUBCOST
	DAYTOTALSALE = DAYTOTALSALE + DAYSUBSALE

	TOTALCOST = TOTALCOST + SUBCOST
	TOTALSALE = TOTALSALE + SUBSALE

	DAYSUBCOST = 0.0
	DAYSUBSALE = 0.0

	SUBCOST = 0.0
	SUBSALE = 0.0

	PRINT_FLAG% = 0%

	RETURN

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
