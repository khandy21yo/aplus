1	%TITLE "Job Status Detail"
	%SBTTL "WP_RPRT_REGISTER"
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
	! ID:WP0055
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Status Detail\* report contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Type	Class
	!	.te
	!	Order Date	Location
	!	.te
	!	Operator	Reference No
	!	.te
	!	Status Flag	Closed Date
	!	.te
	!	Line	Current Line Info.
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Job Status Detail
	!	.x Job Status Detail>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_REGISTER/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_REGISTER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_REGISTER.OBJ;*
	!
	! AUTHOR:
	!
	!	06/26/91 - Val James "Let's Party and Pig Out" Allen
	!
	! MODIFICATION HISTORY:
	!
	!	09/30/91 - Deborah K. Fries
	!		Cleaned source code
	!		Used function to read file
	!		Improved error trapping
	!
	!	10/30/92 - Dan Perkins
	!		Fixed FROM_ITEM$ and TO_ITEM$ in sort by "J"
	!		so that they would work properly.
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
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add some REGARDLESS clauses
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP	(SB_SUBACCOUNT)	JC_JOB_CDD	JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)		SB_SUBACCOUNT_CDD  SB_SUBACCOUNT

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
	!	The ^*From Item\* field enters the item with which the report
	!	will begin printing.   The item entered must be in agreement
	!	with field (01) Sort by.
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
	!	The ^*To Item\* field specifies the item with
	!	which the report is to begin printing.  The value
	!	must be in agreement field (01) Sort by.
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

	STATUS$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	.x Status>Job Status Summary Report
	!	^*(05) Status\*
	!	.b
	!	.lm +5
	!	The ^*Status\* field selects
	!	the Jobs flagged with a given status code.
	!	Only those jobs with the selected status will be reflected on
	!	the report.
	!	.b
	!	Valid Status Codes Are:
	!	.table 3,25
	!	.te
	!	^*A\* = Active Jobs
	!	.te
	!	^*I\* = Inactive Jobs
	!	.te
	!	^*C\* = Completed Jobs
	!	.end table
	!	A blank setting in this field will print all records in the
	!	register file regardless of status.
	!	.b
	!	Any single status code or any two code combination may be entered.
	!	For example:  AI = Active and Inactive, IC = Inactive and Completed
	!	.lm -5
	!
	! Index:
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

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER DETAIL BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER DETAIL BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER DETAIL BY JOB NUMBER"


	END SELECT

	SELECT STATUS$

	CASE "A"
		TITLE$(2%) = "Active Jobs"

	CASE "I"
		TITLE$(2%) = "Inactive Jobs"

	CASE "C"
		TITLE$(2%) = "Closed Jobs"

	CASE "AI"
		TITLE$(2%) = "Active and Inactive Jobs"

	CASE "IA"
		TITLE$(2%) = "Active and Inactive Jobs"

	CASE "AC"
		TITLE$(2%) = "Active and Completed Jobs"

	CASE "CA"
		TITLE$(2%) = "Active and Completed Jobs"

	CASE "IC"
		TITLE$(2%) = "Completed and Inactive Jobs"

	CASE "CI"
		TITLE$(2%) = "Completed and Inactive Jobs"

	CASE ELSE
		TITLE$(2%) = "All Jobs regardless of Status"

	END SELECT


	!
	! Title
	!
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber  JobDescription               " + &
		"            Type Class   JobDate  Status" + &
		" CloseDate  Location Operator   ReferenceNo"

	TITLE$(6%) = "              Line Rtype LT ItemCode      Line" + &
		"Description                           It" + &
		"emCost       Qty StartDate  CompDate   Batch"

	TITLE$(7%) = "."

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
				KEY #K_NUM% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J" + FROM_ITEM$, &
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

	GOTO GetNextRec IF COMP_ARRAY(STATUS$, JC_JOB::SSTATUS) = 0% &
		AND STATUS$ <> ""

 PrintItNow:
	!
	! Build and print the Header out now
	!
	TEXT$ = JC_JOB::JOB + " " + &
		JC_JOB::DESCR + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		JC_JOB::SSTATUS + "      " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + " " + &
		JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Try to find any line for the header
	!
17310	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #0% GE JC_JOB::JOB, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 155% OR ERR = 11% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 Regline:
	!
	! Read line for header
	!
	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 155% OR ERR = 11% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO NewOrder IF WP_REGLINE::JOB <> JC_JOB::JOB

	!
	! To form the TEXT$ line
	!
	SELECT WP_REGLINE::REC_TYPE

	CASE "01"
		RECTYPE$ = "ORD"
		STARTDATE$ = PRNT_DATE(WP_REGLINE::START_DATE, 8%)

	CASE "02"
		RECTYPE$ = "COM"
		WP_REGLINE::TTYPE = " "
		WP_REGLINE::ITEMCODE = " "
		WP_REGLINE::DESCR = " "
		STARTDATE$ = "          "

	CASE "03"
		RECTYPE$ = "CAN"
		WP_REGLINE::TTYPE = " "
		WP_REGLINE::ITEMCODE = " "
		WP_REGLINE::DESCR = " "
		STARTDATE$ = "          "

	CASE ELSE
		GOTO Regline

	END SELECT

	TEXT$ = JC_JOB::JOB + "    " + &
		WP_REGLINE::LLINE + " " + &
		RECTYPE$ + "   " + &
		WP_REGLINE::TTYPE + "  " + &
		WP_REGLINE::ITEMCODE + &
		WP_REGLINE::DESCR + " " + &
		FORMAT$(WP_REGLINE::COST, "######.##") + " " + &
		FORMAT$(WP_REGLINE::QTY, "######.##") + " " + &
		STARTDATE$ + " " + &
		PRNT_DATE(WP_REGLINE::COMP_DATE, 8%) + " " + &
		WP_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO Regline

  NewOrder:

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

17350	!
	! Try for next Order Register record
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
