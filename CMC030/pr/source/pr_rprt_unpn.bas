1	%TITLE "PR Union/Pension Table Report"
	%SBTTL "PR_RPRT_UNPN"
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
	! ID:PR069
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Union Pension Table\* option
	!	prints a list of the contents
	!	of the Union Pension Table.
	!	.b
	!	The list contains the following fields:
	!	.table 3,25
	!	.te
	!	Code
	!	.te
	!	Code Description
	!	.te
	!	Expense Account
	!	.te
	!	Type
	!	.te
	!	Type Description
	!	.te
	!	Paid By
	!	.te
	!	Employee Rate
	!	.te
	!	Employer Rate
	!	.te
	!	Liability Account
	!	.te
	!	Basis
	!	.te
	!	Deduction Code
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Union/Pension Table>Report
	!	.x Report>Union/Pension Table
	!
	! Option:
	!
	! Author:
	!
	!	12/11/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_UNPN
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_UNPN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_UNPN.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/02/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.HB"
	MAP	(PR_UNPN_DESC)	PR_UNPN_DESC_CDD	PR_UNPN_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DEF.HB"
	MAP	(PR_UNPN_DEF)	PR_UNPN_DEF_CDD		PR_UNPN_DEF

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Code\*
	!	.b
	!	.lm +5
	!	The ^*From Code\* field causes the printing
	!	to begin with a particular code.
	!	.b
	!	A blank causes the report to begin with the first code in
	!	file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Code\*
	!	.b
	!	.lm +5
	!	The ^*To Code\* field causes the printing
	!	to end with this particular code.
	!	.b
	!	A blank causes the report to end with the last code in file.
	!	.lm -5
	!
	! Index:
	!
	!--

	K_NUM% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.OPN"
	USE
		FILENAME$ = "PR_UNPN_DESC"
		CONTINUE HelpError
	END WHEN

310	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DEF.OPN"
	USE
		FILENAME$ = "PR_UNPN_DEF"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "UNION/PENSION TABLE REPORT"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = "Code        Description             " + &
		"              Expense Account    "

	TITLE2$ = "Ty Description          PB  EE Rate  ER Rate  " + &
		"Liability Account  Basis  DC     "

	TITLE10$ = STRING$(80%, 61%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_UNPN_DESC.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_UNPN_DESC.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_UNPN_DESC"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_UNPN_DESC.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_UNPN_DESC"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_UNPN_DESC::CODE > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 13%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = " " + PR_UNPN_DESC::CODE + "         " + &
		PR_UNPN_DESC::DESCR + "        " + &
		PR_UNPN_DESC::EX_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 DefTop:
17040	!
	! Def loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_UNPN_DEF.CH%, KEY #0% EQ PR_UNPN_DESC::CODE, REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	DEF_FLAG% = -1%

 DefNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_UNPN_DEF.CH%, REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	GOTO LongEnd IF PR_UNPN_DEF::CODE <> PR_UNPN_DESC::CODE

	IF DEF_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE2$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		DEF_FLAG% = 0%
	END IF

	TEXT$ = PR_UNPN_DEF::DTYPE + " " + &
		LEFT(PR_UNPN_DEF::DESCR, 20%) + " " + &
		PR_UNPN_DEF::PAID_BY + "  " + &
		FORMAT$(PR_UNPN_DEF::EMPE_RATE, "#####.##") + " " + &
		FORMAT$(PR_UNPN_DEF::EMPR_RATE, "#####.##") + "  " + &
		PR_UNPN_DEF::LIA_ACCT + "   " + &
		PR_UNPN_DEF::BASIS + "    " + &
		PR_UNPN_DEF::DED_CODE
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO DefNext

 LongEnd:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE10$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
