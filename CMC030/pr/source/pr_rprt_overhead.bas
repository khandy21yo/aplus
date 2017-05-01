1	%TITLE "PR Overhead Table Report"
	%SBTTL "PR_RPRT_OVERHEAD"
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
	! ID:PR065
	!
	! Abstract:HELP
	!	.p
	!	The ^*Overhead Table\* option
	!	prints a list of the contents
	!	of the Overhead Table. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Overhead Key
	!	.le
	!	Description
	!	.le
	!	Rate
	!	.le
	!	Basis
	!	.le
	!	Premium Account
	!	.le
	!	Subject Account
	!	.le
	!	Overhead Account
	!	.le
	!	Expense Account
	!	.le
	!	Operation
	!	.els
	!
	! Index:
	!	.x Overhead Table>Report
	!	.x Report>Overhead Table
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_OVERHEAD
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_OVERHEAD, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_OVERHEAD.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP	(PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP	(PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF

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
	!	^*(01) From Item\*
	!	.p
	!	The ^*Item\* field causes the printing
	!	to begin from a particular item.
	!	.p
	!	A blank in this field will cause the report to begin with
	!	the first item in the file.
	!
	! Index:
	!	.x From Item>Overhead Table Report
	!	.x Overhead Table>Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with a particular item.
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Overhead Table Report
	!	.x Overhead Table>Report>To Item
	!
	!--


	K_NUM% = 0%


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.OPN"
	USE
		FILENAME$ = "PR_OVERHEAD_DESC"
		CONTINUE HelpError
	END WHEN

310	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.OPN"
	USE
		FILENAME$ = "PR_OVERHEAD_DEF"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "OVERHEAD TABLE REPORT"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = "OH Key          Description"

	TITLE2$ = "    Rate  Basis  Premium Account     " + &
		"Overhead Account    Expense Account"

	TITLE3$ = "                    Subject Account                Operation"

	TITLE10$ = STRING$(80%, 61%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_OVERHEAD_DESC.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_OVERHEAD_DESC.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_OVERHEAD_DESC"
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
		GET #PR_OVERHEAD_DESC.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11
		FILENAME$ = "PR_OVERHEAD_DESC"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_OVERHEAD_DESC::OVH_KEY > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 13%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = PR_OVERHEAD_DESC::OVH_KEY + "          " + &
		PR_OVERHEAD_DESC::DESCR
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = FORMAT$(PR_OVERHEAD_DESC::RATE, "#,###.##") + "    " + &
		PR_OVERHEAD_DESC::BASIS + "    " + &
		PR_OVERHEAD_DESC::PREM_ACCT + "  " + &
		PR_OVERHEAD_DESC::OVRHD_ACCT + "  " + &
		PR_OVERHEAD_DESC::EX_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 DefTop:
17040	!
	! Def loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_OVERHEAD_DEF.CH%, &
			KEY #0% EQ PR_OVERHEAD_DESC::OVH_KEY, &
			REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	DEF_FLAG% = -1%

 DefNext:
	!
	! Get next record
	!
	GET #PR_OVERHEAD_DEF.CH%, REGARDLESS

	GOTO LongEnd IF PR_OVERHEAD_DEF::OVH_KEY <> PR_OVERHEAD_DESC::OVH_KEY

	IF DEF_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE3$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		DEF_FLAG% = 0%
	END IF

	TEXT$ = SPACE$(20%) + &
		PR_OVERHEAD_DEF::SUBJ_ACCT + SPACE$(13%) + &
		PR_OVERHEAD_DEF::OPER
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
