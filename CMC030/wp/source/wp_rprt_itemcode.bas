1	%TITLE "Active Item Codes"
	%SBTTL "WP_RPRT_ITEMCODE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:WP0060
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Active Item Codes\* report contains columns for the following
	!	information:
	!	.table 3,25
	!	.te
	!	Item Type	Item Code
	!	.te
	!	Description	Job Code
	!	.te
	!	Line number	Order Quantity
	!	.te
	!	Completed Quantity	Cancelled Quantity
	!	.te
	!	Balance	Expected Completion Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Active Item Codes
	!	.x Active Item Codes>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_ITEMCODE/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_ITEMCODE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_ITEMCODE.OBJ;*
	!
	! AUTHOR:
	!
	!	07/05/91 - Craig Tanner
	!
	! MODIFICATION HISTORY:
	!
	!	09/24/91 - Deborah K. Fries
	!		Used function to read file
	!		Cleaned source code
	!		Improved error trapping.
	!
	!	01/21/92 - Dan Perkins
	!		Changed quantity fields to integer values.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
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
	!	05/18/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_OLD
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION WP_READ_REGLINE

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Line Type\*
	!	.b
	!	.lm +5
	!	The ^*Line Type\* enters the line
	!	type (Labor or Materials) where the report will print.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*L\* - Labor
	!	.te
	!	^*M\* - Material
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Line Type
	!
	!--

	FROM_ITEMCODE$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item Code\*
	!	.b
	!	.lm +5
	!	The ^*From Item Code\* field enters the item code with which
	!	the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item Code
	!
	!--

	TO_ITEMCODE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item Code\*
	!	.b
	!	.lm +5
	!	The ^*To Item Code\* field enters the item code
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item Code
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items codes to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Register Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ITEMS IN THE PROCESS"
	TITLE$(2%) = "Work in Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "  ItemCode       Description                       " + &
		"        Job           Line    OrdQty     CompQty    " + &
		"CanQty   Balance CompDate"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If TTYPE$ is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEMCODE$ = ""
		THEN
			FIND #WP_REGLINE.CH%, KEY #1% GE TTYPE$, REGARDLESS
		ELSE
			FIND #WP_REGLINE.CH%, &
				KEY #1% GE TTYPE$ + FROM_ITEMCODE$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get next Order Register record
	!
17020	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Check current record if should be printed
	!
	GOTO ExitTotal IF WP_REGLINE::TTYPE <> TTYPE$

	GOTO ExitTotal IF WP_REGLINE::ITEMCODE > TO_ITEMCODE$ AND &
		(TO_ITEMCODE$ <> "")

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(WP_REGLINE::ITEMCODE, -1%), WLDCRD$) = 0% &
		AND (WLDCRD$ <> "")

	!
	! Check if time to print a total
	!
	IF WP_REGLINE::ITEMCODE <> WP_REGLINE_OLD::ITEMCODE AND &
		(WP_REGLINE_OLD::ITEMCODE <> "")
	THEN
		GOSUB Total
	END IF

	!
	! Set Values
	!
	WP_REGLINE_OLD = WP_REGLINE

        V% = WP_READ_REGLINE(WP_REGLINE_OLD::JOB, WP_REGLINE_OLD::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	TEXT$ = WP_REGLINE_OLD::TTYPE + " " + &
		WP_REGLINE_OLD::ITEMCODE + " " + &
		WP_REGLINE_OLD::DESCR + "  " + &
		WP_REGLINE_OLD::JOB + "    " + &
		WP_REGLINE_OLD::LLINE + " " + &
		FORMAT$(QTY(1%), "#########") + "   " + &
		FORMAT$(QTY(2%), "#########") + " " + &
		FORMAT$(QTY(3%), "#########") + " " + &
		FORMAT$(QTY(0%), "#########") + " " + &
		PRNT_DATE(WP_REGLINE_READ::COMP_DATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTALS(1%) = QTY(1%) + TOTALS(1%)
	TOTALS(2%) = QTY(2%) + TOTALS(2%)
	TOTALS(3%) = QTY(3%) + TOTALS(3%)

	TOTAL_FLAG% = TOTAL_FLAG% + 1%

17030	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #1% GE WP_REGLINE_OLD::TTYPE + &
			WP_REGLINE_OLD::ITEMCODE + &
			WP_REGLINE_OLD::JOB + &
			WP_REGLINE_OLD::LLINE + &
			CHR$(255%), &
			REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Go for next line
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Check if time to print a total
	!
	GOSUB Total

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

 Total:
	TOTAL_BALANCE = TOTALS(1%) - TOTALS(2%) - TOTALS(3%)

	TOTAL_BALANCE = 0.0 IF TOTAL_BALANCE < 0.0

	GOTO SkipTotal IF TOTAL_BALANCE = 0.0

	GOTO SkipTotal IF TOTAL_FLAG% <= 1%

	TEXT$ = WP_REGLINE_OLD::TTYPE + " " + &
		WP_REGLINE_OLD::ITEMCODE + &
		SPACE$(62%) + &
		FORMAT$(TOTALS(1%), "#########") + "   " + &
		FORMAT$(TOTALS(2%), "#########") + " " + &
		FORMAT$(TOTALS(3%), "#########") + " " + &
		FORMAT$(TOTAL_BALANCE, "#########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 SkipTotal:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF TOTAL_FLAG%

	TOTAL_FLAG% = 0%

	TOTALS(I%) = 0.0 FOR I% = 1% TO 3%

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
