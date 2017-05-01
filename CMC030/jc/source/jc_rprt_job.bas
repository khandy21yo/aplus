1	%TITLE "Job Description"
	%SBTTL "JC_RPRT_JOB"
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
	! ID:JC0001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Description\* prints a report
	!	containing all the job descriptions from the job description file. The
	!	following fields are included:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Job Number
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Class
	!	.le
	!	Open Date
	!	.le
	!	Status
	!	.le
	!	Close Date
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Job Description List
	!	.x Description>Job
	!	.x List>Job Description
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_JOB/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_RPRT_JOB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_JOB.OBJ;*
	!
	! Author:
	!
	!	03/23/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	10/14/92 - Dan Perkins
	!		Fixed FROM_ITEM$ to work properly.  Eliminated
	!		ExitTotal from report.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	12/12/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

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
	!	^*(01) Sort by (C,J,T)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field causes the report to print in
	!	a selected order.
	!	.b
	!	The following values are valid:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*C\* = Job Class
	!	.le
	!	^*J\* = Job Number
	!	.le
	!	^*T\* = Job Type
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
	!	The ^*From Item\* field causes the report to begin with the
	!	selected item.  The value entered must be in agreement with the value
	!	in field (01) Sort by.
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
		ADD_TITLE$ = "BY  JOB  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  JOB  TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  JOB  CLASS"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "JC_JOB"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "JOB  DESCRIPTION  " + ADD_TITLE$
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "JobNumber  Description                    " + &
		"Ty Clas OpenDate   S CloseDate"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #SORT_KEY% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #SORT_KEY% GE "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "JC_JOB"
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
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "JC_JOB"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitProgram IF SB_SUBACCOUNT::SUBJECT <> "J"

	SELECT SORT_BY$

	CASE "J"
		GOTO ExitProgram IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%

	CASE "C"
		GOTO ExitProgram IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " " + &
		LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
		SB_SUBACCOUNT::TTYPE + " " + &
		SB_SUBACCOUNT::CLASS + " " + &
		PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
		SB_SUBACCOUNT::SSTATUS + " " + &
		PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

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
	! End of report JC_RPRT_JOB
	!******************************************************************
	END
