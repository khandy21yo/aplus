1	%TITLE "PR JWOD Report"
	%SBTTL "PR_RPRT_JWOD"
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
	! ID:PR080
	!
	! Abstract:HELP
	!	.p
	!	The ^*JWOD Report\* prints a listing of each employee and their JWOD related
	!	hours and amounts.  Included in this report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Subaccount
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	JWOD Hours
	!	.le
	!	JWOD Amount
	!	.le
	!	Non-JWOD Hours
	!	.le
	!	Non-JWOD Amount
	!	.le
	!	Total Hours
	!	.le
	!	Total Amount
	!	.els
	!
	! Index:
	!	.x JWOD>Report
	!	.x Report>JWOD
	!
	! Option:
	!
	! Author:
	!
	!	12/14/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_JWOD
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_JWOD, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_JWOD.OBJ;*
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	08/21/89 - Kevin Handy
	!		Fixed title so that FROM DATE xxx TO DATE xxx would
	!		show the dates, and not the from employee/to employee
	!		in date format.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records.
	!
	!	02/06/92 - Kevin Handy
	!		Added (optional) ratio to job totals
	!
	!	05/12/93 - Kevin Handy
	!		Modified to zero hour totals (element 0),
	!		where it was starting to zero with 1.
	!
	!	05/26/93 - Kevin Handy
	!		Modified to suck in JWOD table at time of open
	!		into three strings, then to do lookups in these
	!		strings instead of file lookups.  This should
	!		speed things up tremendiously.
	!
	!	05/26/93 - Kevin Handy
	!		Display file being processed.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to get on PR_JWOD_CROSS_REF.
	!
	!	10/06/93 - Kevin Handy
	!		Changed dimension for data file from 200 to 600.
	!
	!	01/18/95 - Kevin Handy
	!		Added from/to job.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/18/95 - Kevin Handy
	!		Increase dimension for data files from 600 to 1000.
	!		Reformat source closer to 80 columns.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.HB"
	MAP	(PR_JWOD_CROSS_REF)	PR_JWOD_CROSS_REF_CDD	PR_JWOD_CROSS_REF

	MAP (PR_TEMP) &
		PR_TEMP.SUBACC$ = 10%, &
		PR_TEMP.DISABLED$ =  1%, &
		PR_TEMP.EMPNUM$ = 10%, &
		PR_TEMP.EMPNAME$ = 20%, &
		PR_TEMP.JWOD_HRS, &
		PR_TEMP.JWOD_AMT, &
		PR_TEMP.NJWOD_HRS, &
		PR_TEMP.NJWOD_AMT

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	!
	! Dimension statements
	!
	EMP_FILE% = 3000%
	DIM DATA_FILE$(1000%), TOT_EMP$(EMP_FILE%), JWOD_EMP$(EMP_FILE%)

	TOT_EMP$(I%), JWOD_EMP$(I%) = "" FOR I% = 0% TO EMP_FILE%

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!++
	! Abstract:FLD01
	!	^*(01) From Date\*
	!	.p
	!	The ^*From Date\* field enters the date with
	!	which the report will begin printing.  A blank field causes the report to
	!	begin with the first date in the file.
	!
	! Index:
	!	.x From Date
	!
	!--
	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.p
	!	The ^*To Date\* field enters the date which causes the
	!	report to end printing.  A blank field causes the report to end with the last
	!	date in the file.
	!
	! Index:
	!
	!--
	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD03
	!	^*(03) Summary Only (Y/N)\*
	!	.p
	!	The ^*Summary Only\* field prints the entire listing or
	!	only the summary information.  A ^*Y\* entry causes only the summary to be
	!	printed, while a ^*N\* entry prints a fully detailed report.
	!
	! Index:
	!	.x Summary Only
	!
	!--
	SUMMARY_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Use Master Subaccount Number\*
	!	.p
	!	The ^*Use Master Subaccount Number\* decides
	!	if the Master file Subaccount Number will be used or the payroll folder
	!	Subaccount number.  A ^*Y\* answer causes the Master number to be used and a
	!	^*N\* causes the payroll folder number to be used.
	!
	! Index:
	!	.x Use Master Subaccount Number
	!
	!--
	USE.MST.SUBACCT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Non-prod Code Exclude\*
	!
	! Index:
	!
	!--
	NONPROD_CODE_EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Non-prod Code Include\*
	!
	! Index:
	!
	!--
	NONPROD_CODE_INCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Show Ratio\*
	!
	! Index:
	!
	!--
	SHOWRATIO% = (LEFT(UTL_REPORTX::OPTDEF(6%), 1%) = "Y")

	!++
	! Abstract:FLD08
	!	^*(08) From Subaccount\*
	!
	! Index:
	!
	!--
	FROMJOB$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD09
	!	^*(09) To Subaccount\*
	!
	! Index:
	!
	!--
	TOJOB$ = TRM$(UTL_REPORTX::OPTDEF(8%))

	K_NUM% = 0%

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FUNC_FILESCAN(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	GOTO ExitProgram IF DATA_FILE% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open JWOD table, and suck info into strings for quicker lookup
	!
	JWOD_YES$ = "~"
	JWOD_NO$ = "~"
	JWOD_UNKNOWN$ = "~"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.OPN"

		RESET #PR_JWOD_CROSS_REF.CH%
	USE
		FILENAME$ = "PR_JWOD_CROSS_REF"
		CONTINUE HelpError
	END WHEN

312	WHEN ERROR IN
		GET #PR_JWOD_CROSS_REF.CH%, REGARDLESS
	USE
		CONTINUE 320
	END WHEN

	SELECT PR_JWOD_CROSS_REF::FLAG

	CASE "J"
		JWOD_YES$ = JWOD_YES$ + PR_JWOD_CROSS_REF::SUBACCT + "~"

	CASE "N"
		JWOD_NO$ = JWOD_NO$ + PR_JWOD_CROSS_REF::SUBACCT + "~"

	CASE ELSE
		JWOD_UNKNOWN$ = JWOD_UNKNOWN$ + PR_JWOD_CROSS_REF::SUBACCT + "~"

	END SELECT

	GOTO 312

320	!
	! Create work file
	!

350	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Pay file.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY (PR_TEMP.SUBACC$,PR_TEMP.DISABLED$,PR_TEMP.EMPNUM$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	FOR LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, &
			"Creating work file.  Reading Pay file. " + BATCH_NO$, &
			1%)

		USE_HISTORY% = 0%

400		!
		! Open file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 402 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 405

402		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 460 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

405		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 460
		END WHEN

410		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 410 IF PR_TRN_PAY::PTYPE = "A"

		GOTO 410 IF PR_TRN_PAY::SUBACC < FROMJOB$ AND FROMJOB$ <> ""
		GOTO 410 IF PR_TRN_PAY::SUBACC > TOJOB$ AND TOJOB$ <> ""

420		PR_EMP_MASTER::EMPNAME  = STRING$(20%, 63%)
		PR_EMP_MASTER::DISABLED = "Y"
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 440 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

440		PR_TEMP.SUBACC$ = PR_TRN_PAY::SUBACC

		IF USE.MST.SUBACCT$ = "Y"
		THEN
			PR_TEMP.SUBACC$ = PR_EMP_MASTER::SUBACC
		END IF

		GOTO 410 IF PR_TEMP.SUBACC$ = ""

		A_JWOD% = 0%

		!
		! Look up JWOD status
		!
		JWOD_YES% = INSTR(1%, JWOD_YES$, PR_TEMP.SUBACC$)
		JWOD_NO% = INSTR(1%, JWOD_NO$, PR_TEMP.SUBACC$)
		JWOD_UNKNOWN% = INSTR(1%, JWOD_UNKNOWN$, PR_TEMP.SUBACC$)

		IF (JWOD_YES% = 0%) AND (JWOD_NO% = 0%) AND &
			(JWOD_UNKNOWN%) = 0%
		THEN
			GOTO 450
		END IF

		IF JWOD_UNKNOWN% <> 0%
		THEN
			GOTO 410
		END IF

		!
		! Is this record a non productive record.  If so then don't
		! add it to the jwod report
		!
		IF COMP_STRING(PR_TRN_PAY::OPER, NONPROD_CODE_EXCLUDE$) AND TRM$(NONPROD_CODE_EXCLUDE$) <> "" AND &
			COMP_STRING(PR_TRN_PAY::OPER, NONPROD_CODE_INCLUDE$) = 0%
		THEN
			GOTO 410
		END IF

		A_JWOD% = -1% IF JWOD_YES% <> 0%

450		IF PR_EMP_MASTER::DISABLED = "Y"
		THEN
			PR_TEMP.DISABLED$ = "D"
		ELSE
			PR_TEMP.DISABLED$ = "N"
		END IF

		PR_TEMP.EMPNAME$ = LEFT(PR_EMP_MASTER::EMPNAME, 20%)
		PR_TEMP.EMPNUM$ = PR_TRN_PAY::EMPNUM

		GOTO EmpTOT IF PR_TRN_PAY::EMPNUM = TOT_EMP$(X%) OR TOT_EMP$(X%) = "" &
			FOR X% = 0% TO EMP_FILE%

 EmpTOT:
		TOT_EMP$(X%) = PR_TRN_PAY::EMPNUM IF TOT_EMP$(X%) = ""

		GOTO EmpCont IF A_JWOD% = 0%

		GOTO EmpJWOD IF PR_TRN_PAY::EMPNUM = JWOD_EMP$(X%) OR JWOD_EMP$(X%) = "" &
			FOR X% = 0% TO EMP_FILE%

 EmpJWOD:
		JWOD_EMP$(X%) = PR_TRN_PAY::EMPNUM IF JWOD_EMP$(X%) = ""

 EmpCont:
		PR_TEMP.JWOD_HRS, PR_TEMP.JWOD_AMT, PR_TEMP.NJWOD_HRS, &
			PR_TEMP.NJWOD_AMT = 0.0

		IF A_JWOD%
		THEN
			PR_TEMP.JWOD_HRS  = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
			PR_TEMP.JWOD_AMT  = PR_TRN_PAY::GROSS
		ELSE
			PR_TEMP.NJWOD_HRS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
			PR_TEMP.NJWOD_AMT = PR_TRN_PAY::GROSS
		END IF

		PUT #PR_TEMP.CH%

		GOTO 410

460		CLOSE	PR_TMP_PAY.CH%

		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)

	NEXT LOOP%

	CLOSE PR_EMP_MASTER.CH%

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "JWOD REPORT"
	TITLE$(2%) = "From Date " + PRNT_DATE(FROM_BATCH_NO$, 8%) + " to Date " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)

	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = SPACE$(60%) + &
		"JWOD                      NON-JWOD                     TOTAL"
	TITLE$(5%) = "Sub Account   Employee #   NAME                      " + &
		"Hours         Amount        Hours         Amount         " + &
		"Hours         Amount"
	TITLE$(6%) = ""

	TITLE1$ = SPACE$(27%) + "SUBTOTAL DISABLED"
	TITLE2$ = SPACE$(27%) + "SUBTOTAL NON-DISABLED"
	TITLE3$ = SPACE$(27%) + "JOB TOTAL"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_TEMP.CH%, KEY #K_NUM%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	PASS_1%, SUB_COUNT%, JOB_COUNT% = 0%

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

	IF (PR_TEMP.DISABLED$ <> OLD_DISABLED$ OR &
		PR_TEMP.SUBACC$ <> OLD_SUBACC$) AND PASS_1%
	THEN
		GOSUB EmpTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB SubTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PR_TEMP.SUBACC$ <> OLD_SUBACC$
	THEN
		IF PASS_1%
		THEN
			GOSUB JobTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF
		WORK_SUBACC$ = PR_TEMP.SUBACC$
	END IF

	IF SUMMARY_ONLY$ <> "Y"
	THEN
		IF PR_TEMP.EMPNUM$ <> WORK_EMPNUM$ AND PASS_1%
		THEN
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_EMPNUM$ = PR_TEMP.EMPNUM$
		WORK_EMPNAME$ = PR_TEMP.EMPNAME$
		EMP(1%) = EMP(1%) + PR_TEMP.JWOD_HRS
		EMP(2%) = EMP(2%) + PR_TEMP.JWOD_AMT
		EMP(3%) = EMP(3%) + PR_TEMP.NJWOD_HRS
		EMP(4%) = EMP(4%) + PR_TEMP.NJWOD_AMT
		EMP(5%) = EMP(5%) + PR_TEMP.JWOD_HRS+PR_TEMP.NJWOD_HRS
		EMP(6%) = EMP(6%) + PR_TEMP.JWOD_AMT+PR_TEMP.NJWOD_AMT
	END  IF

	SUB_COUNT% = SUB_COUNT% + 1%
	OLD_SUBACC$ = PR_TEMP.SUBACC$
	OLD_DISABLED$ = PR_TEMP.DISABLED$
	PASS_1% = -1%

	SUBT(0%) = SUBT(0%) + PR_TEMP.JWOD_HRS
	SUBT(1%) = SUBT(1%) + PR_TEMP.JWOD_AMT
	SUBT(2%) = SUBT(2%) + PR_TEMP.NJWOD_HRS
	SUBT(3%) = SUBT(3%) + PR_TEMP.NJWOD_AMT
	SUBT(4%) = SUBT(4%) + PR_TEMP.JWOD_HRS + PR_TEMP.NJWOD_HRS
	SUBT(5%) = SUBT(5%) + PR_TEMP.JWOD_AMT + PR_TEMP.NJWOD_AMT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB EmpTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB SubTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB JobTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TITLE$(3%),TITLE$(4%) = ""
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "DIRECT LABOR INFORMATION:", &
		9999%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 32%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "   HOURS:", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "      DIRECT LABOR HOURS PAID" + &
		" TO DISABLED                JWOD               " + &
		FORMAT$(DIST(0%), "###,###,###.##") + "    = a", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"NON-JWOD           " + &
		FORMAT$(DIST(2%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL              " + &
		FORMAT$(DIST(0%) + DIST(2%), "###,###,###.##") + "    = x", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "      DIRECT LABOR HOURS PAID" + &
		" TO NON-DISABLED            JWOD               " + &
		FORMAT$(NONT(0%), "###,###,###.##") + "    = b", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"NON-JWOD           " + &
		FORMAT$(NONT(2%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL              " + &
		FORMAT$(NONT(0%) + NONT(2%), "###,###,###.##") + "    = y", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL HOURS        " + &
		FORMAT$((DIST(0%) + DIST(2%) + NONT(0%) + NONT(2%)), &
		"###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "   WAGES:", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "      DIRECT LABOR WAGES PAID" + &
		" TO DISABLED                JWOD               " + &
		FORMAT$(DIST(1%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"NON-JWOD           " + &
		FORMAT$(DIST(3%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL              " + &
		FORMAT$(DIST(1%) + DIST(3%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "      DIRECT LABOR WAGES PAID" + &
		" TO NON-DISABLED            JWOD               " + &
		FORMAT$(NONT(1%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"NON-JWOD           " + &
		FORMAT$(NONT(3%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL              " + &
		FORMAT$(NONT(1%) + NONT(3%), "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"TOTAL WAGES        " + &
		FORMAT$((DIST(1%) + DIST(3%) + NONT(1%) + NONT(3%)), &
		"###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	AAAA = 0.0
	AAAA = FUNC_ROUND(DIST(1%) / DIST(0%), 2%) IF DIST(0%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "AVERAGE HOURLY WAGE PAID" + &
		" TO DISABLED                     JWOD               " + &
		FORMAT$(AAAA, "###,###,###.##"), 0%)
	BBBB = 0.0
	BBBB = FUNC_ROUND(DIST(3%) / DIST(2%), 2%) IF DIST(2%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"NON-JWOD           " + &
		FORMAT$(BBBB, "###,###,###.##"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CCCC = 0.0
	TEMP = (DIST(0%) + DIST(2%) + NONT(0%) + NONT(2%))
	CCCC = FUNC_ROUND(((DIST(0%) + DIST(2%)) / TEMP) * 100.0, 2%) &
		IF TEMP <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "DIRECT LABOR RATIO (HOURS)" + &
		"                               AGENCY                     " + &
		FORMAT$(CCCC, "###.##%") + "  x/(x+y)", 0%)
	DDDD = 0.0
	TEMP = DIST(0%) + NONT(0%)
	DDDD = FUNC_ROUND((DIST(0%) / TEMP) * 100.0, 2%) IF TEMP <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"JWOD                       " + &
		FORMAT$(DDDD, "###.##%") + "  a/(a+b)", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO WorkForce1 IF TOT_EMP$(I%) = "" FOR I% = 0% TO EMP_FILE%

 WorkForce1:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "WORKFORCE" + SPACE$(48%) + &
		"AGENCY                  " + FORMAT$(I%, "##,###"), 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO WorkForce2 IF JWOD_EMP$(I%) = "" FOR I% = 0% TO EMP_FILE%

 WorkForce2:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SPACE$(57%) + &
		"JWOD                    " + FORMAT$(I%, "##,###"), 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

17500	!
	! Kill the PR temp file
	!
	CLOSE PR_TEMP.CH%

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 SubTotal:
	!
	! Check for the Sub Totals
	!
	IF SUB_COUNT% > 0%
	THEN
		SELECT OLD_DISABLED$
		CASE "D"
			TEMP$ = TITLE1$ + SPACE$(6%)
			DIST(0%) = DIST(0%) + SUBT(0%)
			DIST(1%) = DIST(1%) + SUBT(1%)
			DIST(2%) = DIST(2%) + SUBT(2%)
			DIST(3%) = DIST(3%) + SUBT(3%)
			DIST(4%) = DIST(4%) + SUBT(4%)
			DIST(5%) = DIST(5%) + SUBT(5%)

			DISTX(0%) = DISTX(0%) + SUBT(0%)
			DISTX(1%) = DISTX(1%) + SUBT(1%)
			DISTX(2%) = DISTX(2%) + SUBT(2%)
			DISTX(3%) = DISTX(3%) + SUBT(3%)
			DISTX(4%) = DISTX(4%) + SUBT(4%)
			DISTX(5%) = DISTX(5%) + SUBT(5%)

		CASE ELSE
			TEMP$ = TITLE2$ + SPACE$(2%)
			NONT(0%) = NONT(0%) + SUBT(0%)
			NONT(1%) = NONT(1%) + SUBT(1%)
			NONT(2%) = NONT(2%) + SUBT(2%)
			NONT(3%) = NONT(3%) + SUBT(3%)
			NONT(4%) = NONT(4%) + SUBT(4%)
			NONT(5%) = NONT(5%) + SUBT(5%)

			NONTX(0%) = NONTX(0%) + SUBT(0%)
			NONTX(1%) = NONTX(1%) + SUBT(1%)
			NONTX(2%) = NONTX(2%) + SUBT(2%)
			NONTX(3%) = NONTX(3%) + SUBT(3%)
			NONTX(4%) = NONTX(4%) + SUBT(4%)
			NONTX(5%) = NONTX(5%) + SUBT(5%)

		END SELECT

		IF SUMMARY_ONLY$ <> "Y"
		THEN
			TEXT$ = TEMP$ + &
				FORMAT$(SUBT(0%), "#,###.##") + " " + &
				FORMAT$(SUBT(1%), "###,###,###.##") + "     " + &
				FORMAT$(SUBT(2%), "#,###.##") + " " + &
				FORMAT$(SUBT(3%), "###,###,###.##") + "     " + &
				FORMAT$(SUBT(4%), "##,###.##") + " " + &
				FORMAT$(SUBT(5%), "###,###,###.##")
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END IF

		JOBT(0%) = JOBT(0%) + SUBT(0%)
		JOBT(1%) = JOBT(1%) + SUBT(1%)
		JOBT(2%) = JOBT(2%) + SUBT(2%)
		JOBT(3%) = JOBT(3%) + SUBT(3%)
		JOBT(4%) = JOBT(4%) + SUBT(4%)
		JOBT(5%) = JOBT(5%) + SUBT(5%)

		SUBT(Z%) = 0.0 FOR Z% = 0% TO 5%

		JOB_COUNT% = JOB_COUNT% + SUB_COUNT%
	END IF

	SUB_COUNT% = 0%

	RETURN

 JobTotal:
	!
	! Check for the Job Totals
	!
	IF JOB_COUNT% > 0%
	THEN
		IF SUMMARY_ONLY$ <> "Y"
		THEN
			TEXT$ = TITLE3$ + SPACE$(14%) + &
				FORMAT$(JOBT(0%), "#,###.##") + " " + &
				FORMAT$(JOBT(1%), "###,###,###.##") + "     " + &
				FORMAT$(JOBT(2%), "#,###.##") + " " + &
				FORMAT$(JOBT(3%), "###,###,###.##") + "     " + &
				FORMAT$(JOBT(4%), "##,###.##") + " " + &
				FORMAT$(JOBT(5%), "###,###,###.##")
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END IF

		IF SHOWRATIO%
		THEN
			CCCC = 0.0
			TEMP = (DISTX(0%) + DISTX(2%) + NONTX(0%) + NONTX(2%))
			CCCC = FUNC_ROUND(((DISTX(0%) + DISTX(2%)) / TEMP) * 100.0, 2%) &
				IF TEMP <> 0.0


			DDDD = 0.0
			TEMP = DISTX(0%) + NONTX(0%)
			DDDD = FUNC_ROUND((DISTX(0%) / TEMP) * 100.0, 2%) &
				IF TEMP <> 0.0

			TEXT$ = &
				SPACE$(27%) + "JOB RATIO:   AGENCY =" + &
				FORMAT$(CCCC, "###.##%") + "    JWOD =" + &
				FORMAT$(DDDD, "###.##%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

	END IF

	FOR Z% = 0% TO 5%
		DISTX(Z%) = 0.0
		NONTX(Z%) = 0.0
		JOBT(Z%) = 0.0
	NEXT Z%

	JOB_COUNT% = 0%

	RETURN

 EmpTotal:
	!******************************************************************
	! Print employee totals
	!******************************************************************
	TEST% = 0%
	TEST% = -1% IF EMP(I%) <> 0.0 FOR I% = 1% TO 6%
	GOTO EmpTotal1 IF TEST% = 0%

	TEXT$ = LEFT(WORK_SUBACC$ + SPACE$(10%), 10%) + "    " + &
		WORK_EMPNUM$ + "   " + &
		WORK_EMPNAME$ + "   " + &
		FORMAT$(EMP(1%), "#,###.##") + " " + &
		FORMAT$(EMP(2%), "###,###,###.##") + "     " + &
		FORMAT$(EMP(3%), "#,###.##") + " " + &
		FORMAT$(EMP(4%), "###,###,###.##") + "     " + &
		FORMAT$(EMP(5%), "##,###.##") + " " + &
		FORMAT$(EMP(6%), "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	EMP(I%) = 0.0 FOR I% = 1% TO 6%

	WORK_SUBACC$ = ""

 EmpTotal1:
	RETURN

	%Page

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
