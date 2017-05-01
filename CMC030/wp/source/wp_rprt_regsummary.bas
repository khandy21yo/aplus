1	%TITLE "Job Status Summary Report"
	%SBTTL "WP_RPRT_REGSUMMARY"
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
	! ID:WP0050
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Job Status Summary\* report contains the following information:
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
	!	Line	Transaction Type
	!	.te
	!	Item code	Description
	!	.te
	!	Quantity Ordered	Quantity Completed
	!	.te
	!	Quantity Cancelled	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Job Status Summary
	!	.x Job Status Summary>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_REGSUMMARY/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_REGSUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_REGSUMMARY.OBJ;*
	!
	! Author:
	!
	!	06/26/91 - Val James "Let's Party and Pig Out" Allen
	!
	! Modification History:
	!
	!	10/02/91 - Dan Perkins
	!		Cleaned up program code.
	!		Checked error trapping.
	!
	!	01/20/92 - Dan Perkins
	!		Changed quantity values to integer values.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	10/30/92 - Dan Perkins
	!		Fixed FROM_ITEM$ and TO_ITEM$ in sort by "J"
	!		to work properly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/08/96 - Kevin Handy
	!		Reformat Source Code.
	!		Reverse comparison of status.
	!
	!	05/19/97 - Kevin Handy
	!		Reformat Source Code.
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
	! Include codes
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

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
	!	^*(01) Sort by (J,T,C)\*\*
	!	.lm +5
	!	.b
	!	The ^*Sort by\* field determines the order in which the report
	!	will print.
	!	.b
	!	Valid settings are:
	!	.Table 3,25
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*T\* - Job Type
	!	.te
	!	^*C\* - Class
	!	.end table
	!	.b
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
	!	.lm +5
	!	.b
	!	The ^*From Item\* field
	!	enters the item with which be report is to begin printing.
	!	The value in this field must be in agreement with the setting in field (01)
	!	Sort by.
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
	!	.lm +5
	!	.b
	!	The ^*To Item\* field
	!	specifies the item with which the report will end.  The value in
	!	this field must be in agreement with the setting in field (01) Sort by.
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
	!	.lm +5
	!	.b
	!	The ^*Wildcard\* field
	!	selects a designated item or group of items to be printed by
	!	entering a wildcard value.
	!	.b
	!	For further information about wildcard technique, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Status Summary Report
	!
	!--
	STATUS$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Status Select\*
	!	.lm +5
	!	.b
	!	The ^*Status Select\* field
	!	selects the jobs with a given status code.
	!	.b
	!	Valid status codes are:
	!	.table 3,25
	!	.te
	!	A = Active Jobs
	!	.te
	!	I = Inactive Jobs
	!	.te
	!	C = Completed Jobs
	!	.end table
	!	A blank value in this field will cause all records in the register file
	!	to be printed regardless of status.
	!	.b
	!	Any single status code or any combination of two codes may be entered.
	!	For example:  AI = Active and Inactive, IC = Inactive and Completed
	!	.lm -5
	!
	! Index:
	!	.x Status>Select>Job Status Summary Report
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
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB NUMBER"

	END SELECT

	SELECT STATUS$

	CASE "A"
		TITLE$(2%) = "Active Jobs"

	CASE "I"
		TITLE$(2%) = "Inactive Jobs"

	CASE "C"
		TITLE$(2%) = "Closed Jobs"

	CASE "AI", "IA"
		TITLE$(2%) = "Active and Inactive Jobs"

	CASE "AC", "CA"
		TITLE$(2%) = "Active and Completed Jobs"

	CASE "IC", "CI"
		TITLE$(2%) = "Completed and Inactive Jobs"

	CASE ELSE
		TITLE$(2%) = "All Jobs Regardless of Status"

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
		"            Type Class   JobDate  Status"      + &
		" CloseDate  Location Operator   ReferenceNo"

	TITLE$(6%) = "              Line TType ItemCode       Descriptio" + &
		"n                              QtyOrdere"                + &
		"d QtyComplete QtyCancel   Balance"

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

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitProgram IF (JC_JOB::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (JC_JOB::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Check for Status type
	!
	IF STATUS$ <> ""
	THEN
		GOTO GetNextRec IF INSTR(1%, STATUS$, JC_JOB::SSTATUS) = 0%
	END IF

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

	NEXT_LINE$ = "   "

 ReadRegLine:
	GOTO NewOrder IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	TEXT$ = JC_JOB::JOB + "    " + &
		WP_REGLINE_READ::LLINE + " " + &
		WP_REGLINE_READ::TTYPE + "     " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		WP_REGLINE_READ::DESCR + "  " + &
		FORMAT$(QTY(1%), "#########") + "   " + &
		FORMAT$(QTY(2%), "#########") + " " + &
		FORMAT$(QTY(3%), "#########") + " "    + &
		FORMAT$(QTY(0%), "#########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadRegline

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

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
