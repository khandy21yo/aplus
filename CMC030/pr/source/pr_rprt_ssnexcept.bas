1	%TITLE "Payroll SSN Exception Report"
	%SBTTL "PR_RPRT_SSNEXCEPT"
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
	! ID:PR055
	!
	! Abstract:HELP
	!	.p
	!	The ^*Social Security Exception report\* prints a
	!	list of all Social Security numbers that are duplicates.
	!	The information includes the following:
	!	.b
	!	.lm +12
	!	.ls 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Social Security Number
	!	.le
	!	Location
	!	.le
	!	Department
	!	.end list
	!
	! Index:
	!	.x Social Security>Report
	!	.x Report>Social Security
	!	.x Report>Exception
	!	.x Report>Exception
	!	.x Social Security>Exception
	!	.x Exception>Social Security
	!
	! Option:
	!
	! Author:
	!
	!	06/20/89 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SSNEXCEPT
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_SSNEXCEPT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SSNEXCEPT.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.  Also,
	!		modified the report to use the "Print SSN" function.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Declare variables and constants
	!
	DECLARE	PR_EMP_MASTER_CDD	PR_EMP_MASTER_OLD
	DECLARE	STRING			LYT_LINE

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

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Employee Social Security Number Exception Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "EmpNum     EmployeeName                   " + &
		"SSNumber      Loc   Dept"
	TITLE$(4%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:041,$SSN:053,$Location:058,$Dept:065"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	RESET #PR_EMP_MASTER.CH%, KEY #3%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PR_EMP_MASTER_OLD = PR_EMP_MASTER
	REC_OK$ = "Y"

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17030	REC_OK$ = "N" IF (LEN(EDIT$(PR_EMP_MASTER::SSN, 2%)) <> 11%) OR &
		(MID(PR_EMP_MASTER::SSN, 4%, 1%) <> "-") OR &
		(MID(PR_EMP_MASTER::SSN, 7%, 1%) <> "-")

17035	IF (PR_EMP_MASTER::SSN = PR_EMP_MASTER_OLD::SSN) AND &
		(EDIT$(PR_EMP_MASTER::SSN, 2%) <> "--")
	THEN
		COUNT% = COUNT% + 1%

		IF COUNT% = 1%
		THEN
			!
			! Print out OLD employee with same SSN as new one.
			!
			TEXT$ = PR_EMP_MASTER_OLD::EMPNUM + " " + &
				PR_EMP_MASTER_OLD::EMPNAME + " " + &
				PRNT_SSN(PR_EMP_MASTER_OLD::SSN, 0%) + " " + &
				PR_EMP_MASTER_OLD::LOCATION + " " + &
				PR_EMP_MASTER_OLD::DEPT

			CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), &
				TEXT$, 0%)
		END IF
	ELSE
			COUNT% = 0%
	END IF

	GOTO GetNextRec IF (EDIT$(PR_EMP_MASTER::SSN, 2%) <> "--") &
		AND (COUNT% = 0%) AND (REC_OK$ = "Y")

	!
	! Print out first employee with same SSN or SSN is blank
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		PRNT_SSN(PR_EMP_MASTER::SSN, 0%) + " " + &
		PR_EMP_MASTER::LOCATION + " " + &
		PR_EMP_MASTER::DEPT

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
