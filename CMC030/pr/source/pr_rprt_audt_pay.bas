1	%TITLE "Payroll Audit Report"
	%SBTTL "PR_RPRT_AUDT_PAY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR016
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Audit Report\* displays a short summary of the status of the employees
	!	in the payroll at this time. The following information is included in this
	!	report:
	!	.table 3,25
	!	.te
	!	Previous Control
	!	.te
	!	Current Activity
	!	.te
	!	Present Control
	!	.te
	!	Active Employees Not Paid
	!	.te
	!	Number of Active Employees Paid
	!	.te
	!	Number of Terminated/Laid off Employees Paid
	!	.te
	!	Number of Employees Paid
	!	.te
	!	Number of Pays in excess of One
	!	.te
	!	Total Number of Pays
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Payroll audit report>Report
	!	.x Report>Payroll audit report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_PAY/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_PAY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_PAY.OBJ;*
	!
	! Author:
	!
	!	03/02/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove unsolicited input stuff.
	!
	!	05/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/07/2000 - Kevin Handy
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
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"
	MAP	(PR_EMP_DATES)		PR_EMP_DATES_CDD	PR_EMP_DATES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP	(PR_TRN_CHECK)		PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP	(PR_HIS_CHECK)		PR_TRN_CHECK_CDD	PR_HIS_CHECK

	!
	! Record Variables
	!
	RECORD PAY_RECORD
		LONG	PREV_ACTIVE
		LONG	PREV_TERMINATED
		LONG	PREV_LOA
		LONG	NEW_HIRE(2%)
		LONG	NEW_TERMINATED(2%)
		LONG	NEW_LOA(2%)
		LONG	PAID_ACTNO
		LONG	PAID_ACTYES
		LONG	PAID_INACTIVE
		LONG	PAID_EXCESS
	END RECORD

	DECLARE PAY_RECORD GRAND_TOTAL

	%page

	!*******************************************************************
	! FNTESTDATE% - Check range of dates
	!
	! Returns 0 (in range)
	!	-1 (before range)
	!	1 (after range)
	!*******************************************************************

	DEF FNTESTDATE%(FROM_DATE$, TO_DATE$, THIS_DATE$)

		!
		! Before start date?
		!
		IF (THIS_DATE$ < FROM_DATE$)
		THEN
			FNTESTDATE% = -1%
		ELSE
			!
			! After end date
			!
			IF (THIS_DATE$ > TO_DATE$) AND &
				(TO_DATE$ > "00000000")
			THEN
				FNTESTDATE% = 1%
			ELSE
				!
				! Must be in the range
				!
				FNTESTDATE% = 0%
			END IF
		END IF

	END DEF


	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	!
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Folder Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Folder Date\* field specifies the date of the payroll
	!	folder which is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Folder Date>Audit Report
	!	.x Audit Report>Folder Date
	!
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$ = LEFT(BATCH_NO$, 4%)

	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Department\*
	!	.b
	!	.lm +5
	!	The ^*Department\* field refers to the department, area, to which an employee
	!	is assigned.
	!	.lm -5
	!
	! Index:
	!	.x Department>Audit Report
	!	.x Audit Report>Department
	!
	!--

	LAST_BATCH_NO$ = DATE_INVDCODE(DATE_DAYCODE(BATCH_NO$) - 6%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Employee date file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.OPN"
	USE
		FILENAME$ = "PR_EMP_DATES"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
	USE
		CONTINUE 335 IF ERR = 5%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 350

335	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%

350	!

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Audit Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = "Department: " + DEPT$

	TITLE$(4%) = ""

	TITLE$(5%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%, KEY #0%
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
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Skip if wrong department
	!
	IF (DEPT$ <> "")
	THEN
		GOTO 17020 IF COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0%
	END IF

17100	!
	! Figure which column this employee belongs to in the previous
	! week, as well as the current week.
	!
	!	Assume active.
	!	Check the start/termination dates in the employee
	!		record to see if they are terminated there.
	!	Check the employee dates file to see if they are shown
	!		as terminated there.
	!	Check the dates file for leave of absence.
	!
	!	This_week, Last_week codes defined as:
	!		0 - Active
	!		1 - Terminated
	!		2 - LOA
	!		3 - Never been active
	!

	!
	! Check the employee record for being active, terminated.
	!
	SELECT FNTESTDATE%(PR_EMP_MASTER::HIREDAY, &
		PR_EMP_MASTER::TERMDAY, BATCH_NO$)

	CASE 0%
		THIS_WEEK% = 0%
	CASE 1%
		THIS_WEEK% = 1%
	CASE ELSE
		THIS_WEEK% = 3%
	END SELECT

	SELECT FNTESTDATE%(PR_EMP_MASTER::HIREDAY, &
		PR_EMP_MASTER::TERMDAY, LAST_BATCH_NO$)

	CASE 0%
		LAST_WEEK% = 0%
	CASE 1%
		LAST_WEEK% = 1%
	CASE ELSE
		LAST_WEEK% = 3%
	END SELECT

	!
	! Skip active search if is shown as active in both weeks already
	!
	GOTO 17120 IF (THIS_WEEK% = 0%) AND (LAST_WEEK% = 0%)

17110	!
	! Scan through active dates (AC)
	!
	WHEN ERROR IN
		GET #PR_EMP_DATES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM + "AC", &
			REGARDLESS
	USE
		CONTINUE 17120
	END WHEN

	WHILE (PR_EMP_DATES::EMPLOYEE = PR_EMP_MASTER::EMPNUM) AND &
		(PR_EMP_DATES::DATECD = "AC")

		SELECT FNTESTDATE%(PR_EMP_DATES::DATEBEGIN, &
			PR_EMP_DATES::DATEEND, BATCH_NO$)

		CASE 0%
			THIS_WEEK% = 0%
		CASE 1%
			THIS_WEEK% = 1% IF THIS_WEEK% <> 0%
		CASE ELSE
			THIS_WEEK% = 3%
		END SELECT

		SELECT FNTESTDATE%(PR_EMP_DATES::DATEBEGIN, &
			PR_EMP_DATES::DATEEND, LAST_BATCH_NO$)

		CASE 0%
			LAST_WEEK% = 0%
		CASE 1%
			LAST_WEEK% = 1% IF LAST_WEEK% <> 0%
		CASE ELSE
			LAST_WEEK% = 3%
		END SELECT

		GET #PR_EMP_DATES.CH%, REGARDLESS
	NEXT

17120	!
	! Scan through LOA dates (LA)
	!
	WHEN ERROR IN
		GET #PR_EMP_DATES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM + "LA", &
			REGARDLESS
	USE
		CONTINUE 17130
	END WHEN

	WHILE (PR_EMP_DATES::EMPLOYEE = PR_EMP_MASTER::EMPNUM) AND &
		(PR_EMP_DATES::DATECD = "LA")

		IF FNTESTDATE%(PR_EMP_DATES::DATEBEGIN, &
			PR_EMP_DATES::DATEEND, BATCH_NO$) = 0%
		THEN
			THIS_WEEK% = 2% IF THIS_WEEK% = 0%
		END IF

		IF FNTESTDATE%(PR_EMP_DATES::DATEBEGIN, &
			PR_EMP_DATES::DATEEND, LAST_BATCH_NO$) = 0%
		THEN
			LAST_WEEK% = 2% IF LAST_WEEK% = 0%
		END IF

		GET #PR_EMP_DATES.CH%, REGARDLESS
	NEXT

17130	!

17200	!
	! Place into tables at proper points
	!
	SELECT LAST_WEEK% * 10% + THIS_WEEK%

	CASE 00%
		GRAND_TOTAL::PREV_ACTIVE = GRAND_TOTAL::PREV_ACTIVE + 1%

	CASE 01%
		GRAND_TOTAL::PREV_ACTIVE = GRAND_TOTAL::PREV_ACTIVE + 1%
		GRAND_TOTAL::NEW_TERMINATED(1%) = &
			GRAND_TOTAL::NEW_TERMINATED(1%) + 1%
		GRAND_TOTAL::NEW_TERMINATED(0%) = &
			GRAND_TOTAL::NEW_TERMINATED(0%) - 1%

	CASE 02%
		GRAND_TOTAL::PREV_ACTIVE = GRAND_TOTAL::PREV_ACTIVE + 1%
		GRAND_TOTAL::NEW_LOA(0%) = GRAND_TOTAL::NEW_LOA(0%) - 1%
		GRAND_TOTAL::NEW_LOA(2%) = GRAND_TOTAL::NEW_LOA(2%) + 1%

	CASE 10%
		GRAND_TOTAL::PREV_TERMINATED = GRAND_TOTAL::PREV_TERMINATED + 1%
		GRAND_TOTAL::NEW_HIRE(0%) = GRAND_TOTAL::NEW_HIRE(0%) + 1%
		GRAND_TOTAL::NEW_HIRE(1%) = GRAND_TOTAL::NEW_HIRE(1%) - 1%

	CASE 11%
		GRAND_TOTAL::PREV_TERMINATED = GRAND_TOTAL::PREV_TERMINATED + 1%

	CASE 12%
		GRAND_TOTAL::PREV_TERMINATED = GRAND_TOTAL::PREV_TERMINATED + 1%
		GRAND_TOTAL::NEW_LOA(2%) = GRAND_TOTAL::NEW_LOA(2%) + 1%
		GRAND_TOTAL::NEW_LOA(1%) = GRAND_TOTAL::NEW_LOA(1%) - 1%

	CASE 20%
		GRAND_TOTAL::PREV_LOA = GRAND_TOTAL::PREV_LOA + 1%
		GRAND_TOTAL::NEW_LOA(2%) = GRAND_TOTAL::NEW_LOA(2%) - 1%
		GRAND_TOTAL::NEW_LOA(0%) = GRAND_TOTAL::NEW_LOA(0%) + 1%

	CASE 21%
		GRAND_TOTAL::PREV_LOA = GRAND_TOTAL::PREV_LOA + 1%
		GRAND_TOTAL::NEW_LOA(2%) = GRAND_TOTAL::NEW_LOA(2%) - 1%
		GRAND_TOTAL::NEW_LOA(1%) = GRAND_TOTAL::NEW_LOA(1%) + 1%

	CASE 22%
		GRAND_TOTAL::PREV_LOA = GRAND_TOTAL::PREV_LOA + 1%

	CASE 30%
		GRAND_TOTAL::NEW_HIRE(0%) = GRAND_TOTAL::NEW_HIRE(0%) + 1%

	CASE 31%
		GRAND_TOTAL::NEW_TERMINATED(1%) = GRAND_TOTAL::NEW_TERMINATED(1%) + 1%

	CASE 32%
		GRAND_TOTAL::NEW_LOA(2%) = GRAND_TOTAL::NEW_LOA(2%) + 1%

	CASE 33%
		!
		! Won't show on this report
		!
	END SELECT

17300	!
	! Get the check number now
	!
	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, KEY #0% GE PR_EMP_MASTER::EMPNUM, REGARDLESS
	USE
		GRAND_TOTAL::PAID_ACTNO = GRAND_TOTAL::PAID_ACTNO + 1% &
			IF THIS_WEEK% = 0%

		CONTINUE 17310
	END WHEN

	IF USE_HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

	!
	! Not paid?
	!
	IF (PR_TRN_CHECK::EMPNUM <> PR_EMP_MASTER::EMPNUM)
	THEN
		GRAND_TOTAL::PAID_ACTNO = GRAND_TOTAL::PAID_ACTNO + 1% &
			IF THIS_WEEK% = 0%
		GOTO 17400
	END IF

	!
	! Paid, but inactive?
	!
	IF THIS_WEEK% = 0%
	THEN
		GRAND_TOTAL::PAID_ACTYES = &
			GRAND_TOTAL::PAID_ACTYES + 1%
	ELSE
		GRAND_TOTAL::PAID_INACTIVE = &
			GRAND_TOTAL::PAID_INACTIVE + 1%
	END IF

17310	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, REGARDLESS
	USE
		CONTINUE 17400
	END WHEN

	!
	! If history then set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

	IF (PR_TRN_CHECK::EMPNUM = PR_EMP_MASTER::EMPNUM)
	THEN
		GRAND_TOTAL::PAID_EXCESS = GRAND_TOTAL::PAID_EXCESS + 1%

		GOTO 17310
	END IF

17400	!
	! End of report
	!
	GOTO 17020

	%Page

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB PrintGrandTotal

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

 PrintGrandTotal:
	!*******************************************************************
	! Print out the grand totals
	!*******************************************************************

	FMT_STRING1$ = "'LLLLLLLLLLLLLLLLLLLLLLLLLL  "
	FMT_STRING2$ = "<%>###### "

	TEXT$ = &
		FORMAT$("", FMT_STRING1$) + &
		"  Active   Term     LOA   Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("Previous Control", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PREV_ACTIVE, FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_TERMINATED, FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_LOA, FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_ACTIVE + &
			GRAND_TOTAL::PREV_TERMINATED + &
			GRAND_TOTAL::PREV_LOA, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("    Current Activity", FMT_STRING1$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("        Add New Hires", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::NEW_HIRE(0%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_HIRE(1%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_HIRE(2%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_HIRE(0%) + &
			GRAND_TOTAL::NEW_HIRE(1%) + &
			GRAND_TOTAL::NEW_HIRE(2%), FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("        Less Terminations", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::NEW_TERMINATED(0%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_TERMINATED(1%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_TERMINATED(2%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_TERMINATED(0%) + &
			GRAND_TOTAL::NEW_TERMINATED(1%) + &
			GRAND_TOTAL::NEW_TERMINATED(2%), FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("        Less LOA", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::NEW_LOA(0%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_LOA(1%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::NEW_LOA(2%), FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("Present Control", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PREV_ACTIVE + &
			GRAND_TOTAL::NEW_HIRE(0%) + &
			GRAND_TOTAL::NEW_TERMINATED(0%) + &
			GRAND_TOTAL::NEW_LOA(0%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_TERMINATED + &
			GRAND_TOTAL::NEW_HIRE(1%) + &
			GRAND_TOTAL::NEW_TERMINATED(1%) + &
			GRAND_TOTAL::NEW_LOA(1%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_LOA + &
			GRAND_TOTAL::NEW_HIRE(2%) + &
			GRAND_TOTAL::NEW_TERMINATED(2%) + &
			GRAND_TOTAL::NEW_LOA(2%), FMT_STRING2$) + &
		FORMAT$(GRAND_TOTAL::PREV_ACTIVE + &
			GRAND_TOTAL::NEW_HIRE(0%) + &
			GRAND_TOTAL::PREV_TERMINATED + &
			GRAND_TOTAL::NEW_TERMINATED(0%) + &
			GRAND_TOTAL::NEW_LOA(0%) + &
			GRAND_TOTAL::NEW_HIRE(1%) + &
			GRAND_TOTAL::NEW_TERMINATED(1%) + &
			GRAND_TOTAL::PREV_LOA + &
			GRAND_TOTAL::NEW_LOA(1%) + &
			GRAND_TOTAL::NEW_HIRE(2%) + &
			GRAND_TOTAL::NEW_TERMINATED(2%) + &
			GRAND_TOTAL::NEW_LOA(2%), FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Pay distribution
	!
	FMT_STRING1$ = "        'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL    "
	FMT_STRING2$ = "#######"

	TEXT$ = &
		FORMAT$("        * * * Pay Distributation * * *", FMT_STRING1$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("Active Employees Not Paid", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_ACTNO, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("Number of Active Employees Paid", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_ACTYES, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("Number of TERM/LOA Employees Paid", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_INACTIVE, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = &
		FORMAT$("Number of Employees Paid", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_ACTYES + &
			GRAND_TOTAL::PAID_INACTIVE, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("Number of Pays in Excess of One", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_EXCESS, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = &
		FORMAT$("Total Number of Pays", FMT_STRING1$) + &
		FORMAT$(GRAND_TOTAL::PAID_ACTYES + &
			GRAND_TOTAL::PAID_INACTIVE + &
			GRAND_TOTAL::PAID_EXCESS, FMT_STRING2$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN

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

	END
