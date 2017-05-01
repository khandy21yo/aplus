1	%TITLE "Delayed Jobs"
	%SBTTL "WP_RPRT_JOBDELAY"
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
	! ID:WP0056
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Delayed Jobs\* report contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Job Type	Class
	!	.te
	!	Order Date	Location
	!	.te
	!	Operator	Reference No
	!	.te
	!	Status Flag	Closed Date
	!	.te
	!	Line	Current Line Information
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Delayed Jobs
	!	.x Delayed Jobs>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_JOBDELAY/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBDELAY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBDELAY.OBJ;*
	!
	! AUTHOR:
	!
	!	07/03/91 - Jeff Beard
	!
	! MODIFICATION HISTORY:
	!
	!	09/27/91 - Deborah K. Fries
	!		Cleaned source code
	!		Replaced code that was done in function.
	!
	!	01/20/92 - Dan Perkins
	!		Changed quantity values to integer values.
	!		Added omitted REC_TYPE field to report.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	10/30/92 - Dan Perkins
	!		Removed code to pad Job Number with zeros.
	!
	!	11/17/92 - Kevin Handy
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
	!	05/10/2000 - Kevin Handy
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
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

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

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort>Job Status Summary Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*T\* - Job Type
	!	.te
	!	^*C\* - Class
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
	!	^*(02)From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field allows entry of the item with which the report
	!	is to begin printing.  The "item" must be in agreement
	!	with the entry in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Job Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with which the
	!	report will end printing.  The item must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Job Status Summary Report
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard through the wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Order Status Summary Report
	!
	!--

300	!
	! Open  Subaccount Register file
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
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		KEY_NUM% = 1%
		TITLE$(1%) = "DELAY JOBS BY JOB TYPE"

	CASE "C"
		KEY_NUM% = 2%
		TITLE$(1%) = " DELAY JOBS BY JOB CLASS"

	CASE "J"
		KEY_NUM% = 0%
		TITLE$(1%) = " DELAY JOBS BY JOB NUMBER"

		!
		! Routine to load left justified zeros into FROM_ITEM$
		! and TO_ITEM$ if any job numbers are entered as ranges
		!
		!FROM_ITEM$ = STRING$(10%-LEN(FROM_ITEM$), ASCII("0")) + FROM_ITEM$ IF FROM_ITEM$ <> ""
		!TO_ITEM$ = STRING$(10%-LEN(TO_ITEM$), ASCII("0")) + TO_ITEM$ IF TO_ITEM$ <> ""

	END SELECT

	TITLE$(2%) = " Work In Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "JobNumber  JobDescription               " + &
		"            Type Class JobDate    Status" + &
		" CloseDate  Location Operator     ReferenceNo"

	TITLE$(5%) = "            Line  Rtype  LT  ItemCode        " + &
		"Description                                 "  + &
		"ItemCost        Qty  CompDate    DaysLate"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #KEY_NUM% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #KEY_NUM% GE "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next JOB Register record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF JC_JOB::SUBJECT <> "J"
	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitProgram IF (JC_JOB::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			JC_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitProgram IF (JC_JOB::JOB > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			JC_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (JC_JOB::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			JC_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Build and print the Header crap out now
	!
	TEXT1$ = JC_JOB::JOB + " " + &
		JC_JOB::DESCR + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		JC_JOB::SSTATUS + "      " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + "   " + &
		JC_JOB::REFNO

	HEADER.FLAG% = 0%
	LLINE$ = "   "

 GetNextline:
17370	IF WP_READ_REGLINE(JC_JOB::JOB, LLINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL
	THEN
		GOTO NewOrder
	END IF

	LLINE$ = WP_REGLINE_READ::LLINE

	GOTO GetNextline IF QTY(7%) = 0.0

	IF HEADER.FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		HEADER.FLAG% = 1%
	END IF

	Y% = DATE_DAYCODE(DATE_TODAY)
	X% = DATE_DAYCODE(WP_REGLINE_READ::START_DATE)
	WP_TEMP.DATE% = Y% - X%

	!
	! PRINT OUT LLINE
	!
	TEXT$ = JC_JOB::JOB + "  " + &
		WP_REGLINE_READ::LLINE + "  " + &
		WP_REGLINE_READ::REC_TYPE + "     " + &
		WP_REGLINE_READ::TTYPE + "   " + &
		WP_REGLINE_READ::ITEMCODE + "  " + &
		WP_REGLINE_READ::DESCR + "  " + &
		FORMAT$(WP_REGLINE_READ::COST, "#######.##") + "  " + &
		FORMAT$(QTY(7%), "#########") + "  " + &
		PRNT_DATE(WP_REGLINE_READ::COMP_DATE, 8%) + "      " + &
		FORMAT$(WP_TEMP.DATE%, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetNextLine

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF HEADER.FLAG%

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

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
