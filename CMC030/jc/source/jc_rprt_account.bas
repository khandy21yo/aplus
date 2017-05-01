1	%TITLE "Job Costing Account Report"
	%SBTTL "JC_RPRT_ACCOUNT"
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
	! ID:JC0003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Costing Account Report\* prints a report
	!	listing all General Ledger Account numbers used in the Job Costing system.
	!	Included in this report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Account Group
	!	.le
	!	General Ledger Account Number
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Job Costing Account
	!	.x Job Costing Account>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_ACCOUNT/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_RPRT_ACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	04/03/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	09/25/92 - Dan Perkins
	!		Rehacked to use SB_ACCOUNT instead of CHARTEX.
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
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Account\*
	!	.b
	!	.lm +5
	!	The ^*From Account\* field causes the printing
	!	to begin with the selected account.
	!	.b
	!	A blank field causes the report to begin with the first account in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.b
	!	.lm +5
	!	The ^*To Account\* field causes the report to end with the
	!	selected account.
	!	.b
	!	A blank field causes the report to end with the last account in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 128%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
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

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ACCOUNT DESCRIPTION LIST BY ACCOUNT"
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Acct Group  GL Account Number   Description"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_ACCOUNT.CH%
		ELSE
			FIND #SB_ACCOUNT.CH%, &
				KEY #0% GE "JC" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO GetNextRec IF SB_ACCOUNT::SYSTEM <> "JC"

	GOTO ExitProgram IF (SB_ACCOUNT::ACCOUNT > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_ARRAY(EDIT$(SB_ACCOUNT::ACCOUNT, 128%), WLDCRD$) = 0%

	IF INSTR(1%, SB_ACCOUNT::ACCOUNT, "?")
	THEN
		GL_CHART_EXAM::DESCR = "Account Mask Overlay"
	ELSE
		!
		! Get the account description
		!
		V% = GL_EXAM_CHART(SB_ACCOUNT::ACCOUNT, GL_CHART_EXAM)
	END IF

	!
	! Print out one line
	!
	TEXT$ = SB_ACCOUNT::ACCTGROUP + "        " + &
		SB_ACCOUNT::ACCOUNT + "  " + &
		GL_CHART_EXAM::DESCR

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
	! End of report JC_RPRT_ACCOUNT
	!******************************************************************
	END
