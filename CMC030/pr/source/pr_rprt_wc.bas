1	%TITLE "Workmans Compensation Table"
	%SBTTL "PR_RPRT_WC"
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
	! ID:PR066
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Workmans Compensation Table\* option
	!	prints
	!	the Workmans Compensation Table. This list contains the
	!	following fields:
	!	.table 3,25
	!	.te
	!	Code
	!	.te
	!	Description
	!	.te
	!	Liability Account
	!	.te
	!	Expense Accout
	!	.te
	!	Subject Account
	!	.te
	!	Operation
	!	.te
	!	State
	!	.te
	!	Type
	!	.te
	!	Effective Date
	!	.te
	!	Method
	!	.te
	!	Employer Rate
	!	.te
	!	Employee Rate
	!	.te
	!	Maximum Quarter Hours
	!	.end table
	!
	! Index:
	!	.x Workmans Compensation Table>Report
	!	.x Report>Workmans Compensation Table
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_WC
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_WC, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_WC.OBJ;*
	!
	! Modification history:
	!
	!	01/09/91 - Kevin Handy
	!		Removed PR_WC_DEFINITION file.
	!
	!	10/10/91 - Kevin Handy
	!		Removed commented out code.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/01/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP	(PR_WC_DESCR)		PR_WC_DESCR_CDD		PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP	(PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

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
	!	.b
	!	.lm +5
	!	The ^*Item\* field causes the printing
	!	to begin from this particular
	!	item.
	!	.b
	!	A blank in this field will cause the report to begin with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Workmans Compensation Table Report
	!	.x Workmans Compensation Table>Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end printing with a particular item.
	!	.b
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Workmans Compensation Table
	!	.x Workmans Compensation Table>TO Item
	!
	!--


	K_NUM% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.OPN"
	USE
		FILENAME$ = "PR_WC_DESCR"
		CONTINUE HelpError
	END WHEN

320	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
	USE
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Workmen's Comp Table Report"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = " Code       Description              " + &
		"Liability Account      Expense Account"

	TITLE3$ = " ST  Type  Eff Date    Method       Employer Rate    " + &
		"Employee Rate MaxQtrHours"

	TITLE10$ = STRING$(80%, 61%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_WC_DESCR.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_WC_DESCR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_WC_DESCR"
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
		GET #PR_WC_DESCR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_WC_DESCR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_WC_DESCR::CODE > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 13%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = " " + PR_WC_DESCR::CODE + "     " + &
		PR_WC_DESCR::DESCR + "     " + &
		PR_WC_DESCR::LIA_ACCT + "     " + &
		PR_WC_DESCR::EX_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 DefTop:
17040	!
	! Def loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

 InsurTop:
17080	!
	! Insur loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_WC_INSURANCE.CH%, KEY #0% EQ PR_WC_DESCR::CODE, REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	INSUR_FLAG% = -1%

 InsurNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_WC_INSURANCE.CH%, REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	GOTO LongEnd IF PR_WC_INSURANCE::CODE <> PR_WC_DESCR::CODE

	IF INSUR_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE3$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		INSUR_FLAG% = 0%
	END IF

	TEXT$ = " " + PR_WC_INSURANCE::STATE + "   " + &
		PR_WC_INSURANCE::INS_TYPE + "  " + &
		PRNT_DATE(PR_WC_INSURANCE::EFFDAT, 8%) + "     " + &
		PR_WC_INSURANCE::METHOD + "             " + &
		FORMAT$(PR_WC_INSURANCE::EMPLR_RATE, "#,###.####       ") + &
		FORMAT$(PR_WC_INSURANCE::EMPLE_RATE, "#,###.####") + &
		FORMAT$(PR_WC_INSURANCE::MAXQHOURS, "   #,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO InsurNext

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
