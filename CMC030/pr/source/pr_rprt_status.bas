1	%TITLE "PR Employee Status Dump"
	%SBTTL "PR_RPRT_STATUS"
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
	! ID:PR027
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Status Report\* option
	!	prints a report listing the various types
	!	of withholding tax to which an employee's earnings may be subject and
	!	the possible jurisdictional codes relative to State, city, county
	!	and school district withholding taxes. A Status code and number of
	!	exemptions are indicated for each tax type and jurisdiction.
	!	.b
	!	The report includes the following column headings:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Type of Tax
	!	.te
	!	Jurisdictional Code
	!	.te
	!	Status (i.e. marital status or exempt)
	!	.te
	!	Number of Exemptions
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employee Status Report
	!	.x Employee Status>Report
	!	.x Report>Employee Status
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_STATUS
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_STATUS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_STATUS.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	12/13/90 - Kevin Handy
	!		Added Additional Exemption Code.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/15/95 - Kevin Handy
	!		Added employee name to report for King B.
	!
	!	06/15/95 - Kevin Handy
	!		Added "Wildcard Type" option for King B.
	!
	!	06/15/95 - Kevin Handy
	!		Added "Include Terminated" option for King B.
	!
	!	09/11/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)	PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Employee _#\*
	!	.p
	!	The ^*From Employee _#\* field enters a
	!	particular employee number with which the report will begin
	!	printing. A blank field will cause the report
	!	to begin with the first Employee _# in the file.
	!
	! Index:
	!	.x From Employee Number>Employee Status Report
	!	.x Employee Status Report>From Employee Number
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Employee _#\*
	!	.p
	!	The ^*To Employee _#\* field enters a
	!	particular employee _# with which the report will end
	!	printing. A blank field will cause the
	!	report to end with the last Employee _# in the file.
	!
	! Index:
	!	.x To Employee Number>Employee Status Report
	!	.x Employee Status Report>To Employee Number
	!
	!--

	WILD_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard Type\*
	!	.p
	!
	! Index:
	!	.x Wildcard Type>Employee Status Report
	!	.x Employee Status Report>Wildcard Type
	!
	!--

	INC_TERM$ = LEFT(UTL_REPORTX::OPTDEF(3%), 1%)

	!++
	! Abstract:FLD04
	!	^*(04) Include Terminated\*
	!	.p
	!
	! Index:
	!	.x Include Terminated>Employee Status Report
	!	.x Employee Status Report>Include Terminated
	!
	!--

	K_NUM% = 0%

300	!
	! Open status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

310	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Status Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Employee    Name                    " + &
		"Type     Code      Status       Exemptions"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:30, $StType:039,$Code:048," + &
		"$WithholdStat:057,VExemptions:071,VAddexemptions:77"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_STATUS.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_STATUS.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_STATUS"
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
		GET #PR_EMP_STATUS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_EMP_STATUS::EMPNUM > TO_ITEM$) AND &
		TO_ITEM$ <> ""

17030	!
	! Get employee record
	!
	IF PR_EMP_MASTER::EMPNUM <> PR_EMP_STATUS::EMPNUM
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_EMP_STATUS::EMPNUM, &
				REGARDLESS
		USE
			PR_EMP_MASTER::EMPNAME = ""
			PR_EMP_MASTER::TERMDAY = ""

			CONTINUE 17040 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

17040	!
	! Chech wildcard code
	!
	IF WILD_TYPE$ <> ""
	THEN
		GOTO GetNextRec IF COMP_STRING( &
			PR_EMP_STATUS::STTYPE, WILD_TYPE$) = 0%
	END IF

	!
	! Check termination status
	!
	IF INC_TERM$ = "N"
	THEN
		GOTO GetNextRec IF PR_EMP_MASTER::TERMDAY > "00000000"
	END IF

	!
	! Print out one line
	!
	TEXT$ = PR_EMP_STATUS::EMPNUM + "  " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 20%) + "     " + &
		PR_EMP_STATUS::STTYPE + "       " + &
		PR_EMP_STATUS::CODE + "        " + &
		PR_EMP_STATUS::STSTATUS + "           " + &
		FORMAT$(PR_EMP_STATUS::EXEMPT, "###") + "   " + &
		FORMAT$(PR_EMP_STATUS::EXEMPT, "###")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
