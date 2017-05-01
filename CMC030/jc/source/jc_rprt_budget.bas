1	%TITLE "Job Costing Budget Report"
	%SBTTL "JC_RPRT_BUDGET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:JC0002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Budget Report\* prints a list containing all
	!	the budgets of the jobs contained in the ^*Budget Maintenance\* file.
	!	This file is a dump for the ^*Budget Maintenance\* file and contains the
	!	following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Job Number
	!	.le
	!	Operation
	!	.le
	!	General Ledger Account Number
	!	.le
	!	Period
	!	.le
	!	Amount
	!	.le
	!	Units
	!	.le
	!	Hours
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Budget Report
	!	.x Report>Budget
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_BUDGET/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_RPRT_BUDGET, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_BUDGET.OBJ;*
	!
	! Author:
	!
	!	03/24/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Removed extra error trapping (check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	12/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.HB"
	MAP	(SB_BUDGET)	SB_BUDGET_CDD	SB_BUDGET

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD SB_SUBACCOUNT

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) Sort by (J,P)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field causes the report to print in
	!	a selected order. The following values are valid:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*J\* = Job Number
	!	.le
	!	^*P\* = Period
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Sort By
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected item.  The value entered must be in agreement
	!	with the value in field (01) Sort by.
	!	.b
	!	A blank field causes the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report to end with the
	!	selected item.  The value entered must be in agreement with the value
	!	in field (01) Sort by.
	!	.b
	!	A blank field causes the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enables the user to print a report including selected
	!	items only using the "wildcarding" technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	SELECT SORT_BY$
	CASE "J"
		SORT_KEY% = 0%
		ADD_TITLE$ = "By Job Number"
	CASE "P"
		SORT_KEY% = 1%
		ADD_TITLE$ = "By Period"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.OPN"
	USE
		FILENAME$ = "SB_BUDGET"
		CONTINUE HelpError
	END WHEN

310	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Job Budget List Sorted " + ADD_TITLE$
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = ""

	!
	! Heading
	!		 1234567890
	!		 12345678 123456789012345678 123456
	!		 ###,###,###.## ###,###,###.## ###,###,###.##
	TITLE$(4%) = "JobNumber  " + &
		"Opertion GLAccountNumber    Period " + &
		"        Amount          Units          Hours"
	TITLE$(5%) = "."

	LYT.LINE$ = "$JobNumber:011,$Opertion:020,$GLAccountNumber:039,$Period:054,VAmount:070,VUnits:086,VHours:092"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_BUDGET.CH%, KEY #SORT_KEY%
		ELSE
			FIND #SB_BUDGET.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "SB_BUDGET"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_BUDGET.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_BUDGET"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	IF SB_BUDGET::SYSTEM <> "JC"
	THEN
		GOTO GetNextRec
	END IF

	SELECT SORT_BY$

	CASE "J"
		GOTO ExitTotal IF (SB_BUDGET::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(SB_BUDGET::SUBACCOUNT, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO ExitTotal IF (SB_BUDGET::PERIOD > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(SB_BUDGET::PERIOD, -1%), WLDCRD$) = 0%

	END SELECT

17200	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ SB_BUDGET::SUBACCOUNT, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	TEXT$ = SB_BUDGET::SUBACCOUNT + " " + &
		SB_BUDGET::OPERATION + " " + &
		SB_BUDGET::ACCOUNT + " " + &
		SB_BUDGET::PERIOD + " " + &
		FORMAT$(SB_BUDGET::AMOUNT, "###,###,###.##") + " " + &
		FORMAT$(SB_BUDGET::UNITS, "###,###,###.##") + " " + &
		FORMAT$(SB_BUDGET::HOURS, "###,###,###.##")

	CALL OUTP_LINE(LYT.LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!

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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report JC_RPRT_BUDGET
	!******************************************************************
	END
