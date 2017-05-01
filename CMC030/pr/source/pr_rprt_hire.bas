1	%TITLE "PR Employee Hire Date Report"
	%SBTTL "PR_RPRT_HIRE"
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
	! ID:PR024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Date of Employment Report\* option
	!	prints a report which lists all employees
	!	in order by date of employment.
	!	.b
	!	The report includes the following column headings:
	!	.table 3,25
	!	.te
	!	Date of Employment
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employment Date>Report
	!	.x Date of Employment>Report
	!	.x Report>Date of Employment
	!	.x Report>Employment Date
	!	.x Hire Date>Report
	!	.x Report>Hire Date
	!	.x Employee Hire Date>Report
	!	.x Report>Employee Hire Date
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_HIRE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_HIRE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_HIRE.OBJ;*
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file as TEMPORARY instead of
	!		trying to remember to delete it.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreasheet or a DIF file.
	!
	!	04/25/91 - Kevin Handy
	!		Modified to put name and number in temp file instead
	!		of an RFA.  That way we don't need two passes through
	!		the employee file.
	!
	!	04/25/91 - Kevin Handy
	!		Modified to use a RECORD instead of a multitude
	!		of variables in the map statement.  Modified
	!		test of from-to into the file creation instead
	!		of in the print loop.
	!
	!	06/17/91 - Kevin Handy
	!		Added "Terminated (Y/N)" field and code to handle
	!		that field.
	!
	!	11/08/91 - Kevin Handy
	!		Added Location Wildcard field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	11/20/97 - Kevin Handy
	!		Clean up source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add column for marital status and employment status
	!
	!	11/05/98 - Kevin Handy
	!		Modified report for FJ to have grade instead of
	!		termination date
	!
	!	08/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add a lot more error trapping code for PR_TEMP and
	!		PR_EMP_STATUS.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)	PR_EMP_STATUS_CDD	PR_EMP_STATUS

	RECORD PR_TEMP_CDD
		STRING HIREDAY = 8%
		STRING EMPNUM = 10%
		STRING EMPNAME = 30%
		STRING TERMDAY = 8%
		STRING MSTATUS = 1%
		INTEGER EXEMPTION
		STRING EMP_GRADE = 2%
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first
	!	item in the file.
	!
	! Index:
	!	.x From Item>Employee Hire Date Report
	!	.x Employee Hire Date Report>From Item
	!
	!--

	TO_ITEM$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* setting causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>Employee Hire Date Report
	!	.x Employee Hire Date Report>To Item
	!
	!--

	TERMINATED$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), -1%), 1%)

	!++
	! Abstract:FLD03
	!	^*(03) Terminated\*
	!	.p
	!	The ^*Terminated\* field is used to determine if you
	!	want to see terminated employees on the report.
	!
	! Index:
	!	.x Terminated>Employee Hire Date Report
	!	.x Employee Hire Date Report>Terminated
	!
	! Datatype:DATE
	! Size:8
	!--

	LOCWILD$ = TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) Location Wildcard\*
	!	.p
	!	The ^*Location Wildcard\* field is used to
	!	display employees belonging to a specific location.
	!	.p
	!	A blank entry in this field will cause all locations
	!	to be printed.
	!
	! Index:
	!	.x Location>Employee Hire Date Report
	!	.x Employee Hire Date Report>Location
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Open Employee Master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Create work file
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Master file.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY PR_TEMP::HIREDAY DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Employee Status
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%, KEY #0%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO 410 &
		IF (TERMINATED$ = "N") AND (PR_EMP_MASTER::TERMDAY > "00000000")

	IF (LOCWILD$ <> "")
	THEN
		GOTO 410 IF COMP_STRING(LOCWILD$, PR_EMP_MASTER::LOCATION) = 0%
	END IF

	IF (PR_EMP_MASTER::HIREDAY >= FROM_ITEM$) AND &
		((PR_EMP_MASTER::HIREDAY <= TO_ITEM$) OR (TO_ITEM$ = ""))
	THEN
		! OK
	ELSE
		GOTO 410
	END IF

420	!
	! Create sort record
	!
	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + "FW", &
			REGARDLESS
	USE
		PR_EMP_STATUS::STSTATUS = ""
		PR_EMP_STATUS::EXEMPT = 0%
		PR_EMP_STATUS::ADDEXEMPT = 0%
	END WHEN

450	IF PR_EMP_MASTER::HIREDAY = "00000000"
	THEN
		PR_TEMP::HIREDAY = ""
	ELSE
		PR_TEMP::HIREDAY = PR_EMP_MASTER::HIREDAY
	END IF

	PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM
	PR_TEMP::EMPNAME = PR_EMP_MASTER::EMPNAME
	PR_TEMP::TERMDAY = PR_EMP_MASTER::TERMDAY
	PR_TEMP::MSTATUS = PR_EMP_STATUS::STSTATUS
	PR_TEMP::EXEMPTION = PR_EMP_STATUS::EXEMPT + PR_EMP_STATUS::ADDEXEMPT
	PR_TEMP::EMP_GRADE = PR_EMP_MASTER::EMP_GRADE

	WHEN ERROR IN
		PUT #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 410

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Date of Employment Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "Employment Date  Employee #    Employee Name" + &
		"                 Status   Grade"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "DHiringDate:019,$EmpNum:033,$EmpName:060,$Status:70,DTermDate:80"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_EMP_MASTER"
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
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

17040	!
	! Print out one line
	!
	TEXT$ = PRNT_DATE(PR_TEMP::HIREDAY, 8%) + SPACE$(7%) + &
		PR_TEMP::EMPNUM + SPACE$(4%) + &
		PR_TEMP::EMPNAME + "  " + &
		FORMAT$(PR_TEMP::MSTATUS + &
		NUM1$(PR_TEMP::EXEMPTION), "'LLL") +  "   " + &
		PR_TEMP::EMP_GRADE

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
	! Close the channel
	!
	CLOSE #PR_TEMP.CH%

17500	!
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
