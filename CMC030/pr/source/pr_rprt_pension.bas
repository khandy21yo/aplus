1	%TITLE "Pension Report"
	%SBTTL "PR_RPRT_PENSION"
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR071
	!
	! Abstract:HELP
	!	.p
	!	The ^*Pension Report\* makes a report of all pension calculations
	!	and the recipients and includes them in the following columns:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Social Security Number
	!	.le
	!	Sex
	!	.le
	!	Date of Birth
	!	.le
	!	Date of Hire
	!	.le
	!	Current Salary
	!	.le
	!	Hours Worked
	!	.le
	!	Current Status
	!	.els
	!
	! Index:
	!	.x Pension>Report
	!	.x Report>Pension
	!
	! Option:
	!
	! Author:
	!
	!	08/21/89 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_PENSION
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_PENSION, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_PENSION.OBJ;*
	!
	! Modification history:
	!
	!	09/12/89 - Kevin Handy
	!		Fixed bug where didn't handle TO_ITEM$, and
	!		didn't update record when adding to existing
	!		one in the temp file.
	!
	!	09/19/89 - Kevin Handy
	!		Modified so only used "P" type records.
	!
	!	09/28/89 - Kevin Handy
	!		Added total, and spreadsheet information.
	!
	!	10/11/89 - Kevin Handy
	!		Modified to show auto-payments.
	!
	!	10/12/89 - Kevin Handy
	!		Undid 9/19/89 and 10/11/89 so that this
	!		report will show same numbers as check
	!		register.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
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
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/21/2003 - Kevin Handy
	!		Change a VAL% to a VAL to lose error.
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

	RECORD PR_TEMP_RECORD
		STRING	EMPNUM = 10%
		REAL	SALARY
		REAL	HOURS
	END RECORD

	MAP	(PR_TEMP) PR_TEMP_RECORD	PR_TEMP

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	!
	! Dimension statements
	!
	DIM DATA_FILE$(200%)
	DIM BLANK_EMPLOYEE$(400%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) From Folder\*
	!	.p
	!	The ^*From Folder\* setting causes the printing
	!	to begin with a particular folder.
	!	.p
	!	A blank field will cause the report to start with the first folder in the
	!	file.
	!
	! Index:
	!	.x From Folder>Pension Report
	!	.x Pension Report>From Folder
	!
	!--
	TO_BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) To Folder\*
	!	.p
	!	The ^*To Folder\* setting causes the printing
	!	to end with a particular folder.
	!	.p
	!	A blank field causes the report to end with the last folder in the file.
	!
	! Index:
	!	.x To Folder>Pension Report
	!	.x Pension Report>To Folder
	!
	!--
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first item in the file.
	!
	! Index:
	!	.x From Item>Pension Report
	!	.x Pension Report>From Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* setting causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field will cause the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Pension Report
	!	.x Pension Report>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU,NA,LO,SO)\*
	!	.p
	!	The ^*Sort by\* field causes the
	!	report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Alphabetical (last name first)
	!	.le
	!	LO = Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort By>Pension Report
	!	.x Pension Report>Sort By
	!
	!--
	MIN_HOURS = VAL(UTL_REPORTX::OPTDEF(5%))
	!++
	! Abstract:FLD06
	!	^*(06) Minimum Hours\*
	!	.p
	!	The ^*Minimum Hours\* field refers to the minimum hours required for an
	!	employee to be eligible for the pension plan benefits. Any parameter may be
	!	set by the user.
	!
	! Index:
	!	.x Minimum Hours>Pension Report
	!	.x Pension Report>Minimum Hours
	!
	!--
	HIRED_BEFORE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))
	!++
	! Abstract:FLD07
	!	^*(07) Hired Before\*
	!	.p
	!	The ^*Hired Before\* field is used to set a date parameter. The employee
	!	must be hired by the date entered in this field to be eligible for the benefits
	!	of the Pension Plan.
	!
	! Index:
	!	.x Hired Before>Pension Report
	!	.x Pension Report>Hired Before
	!
	! Datatype:DATE
	! Size:8
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

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

350	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Pay file.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY (PR_TEMP::EMPNUM) NODUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	THIS_EMPNUM$ = "01234567890123456789"	! Impossible value
	THIS_SALARY  = 0.0
	THIS_HOURS   = 0.0

	FOR LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, "Starting " + BATCH_NO$, 1%)

		USE_HISTORY% = 0%

400		!
		! Open file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 402 IF ERR = 5%
			CONTINUE 460 IF ERR = 11%
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
			CONTINUE 460 IF ERR = 11%
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

420		!
		! Did the employee number change
		!
		IF THIS_EMPNUM$ <> PR_TRN_PAY::EMPNUM
		THEN
			GOSUB SetSalary
		END IF

		THIS_SALARY = THIS_SALARY + PR_TRN_PAY::GROSS
		THIS_HOURS  = THIS_HOURS  + PR_TRN_PAY::REG_HR + &
			PR_TRN_PAY::ovt_HR

		GOTO 410

460		CLOSE PR_TMP_PAY.CH%

		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)

	NEXT LOOP%

	!
	! Handle last total
	!
	GOSUB SetSalary

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PENSION REPORT"
	TITLE$(2%) = "From Date " + PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" to Date " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)

	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "                                          " + &
		"Social       S Date       Date"
	TITLE$(5%) = "Employee   Employee                       " + &
		"Security     e of         of                Current    Hours    " + &
		"Current"
	TITLE$(6%) = "Number     Name                           " + &
		"Number       x Birth      Hire               Salary   Worked    " + &
		"Status"
	TITLE$(7%) = "."
	TITLE$(8%) = ""

	LYT_LINE$ = "$EMPNUM:11,$EMPNAME:42,$SSN:55,$SEX:57," + &
		"$BIRTH:68,$HIREDAY:79,VSALARY:94,VHOURS:102,$STATUS:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	BLANK_EMPLOYEE% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	TOTAL_DOLLARS = 0.0
	TOTAL_HOURS = 0.0

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
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check for end of print
	!
	IF TO_ITEM$ <> ""
	THEN
		SELECT SORTBY$

		CASE "NU"
			GOTO ExitTotal IF PR_EMP_MASTER::EMPNUM > TO_ITEM$

		CASE "NA"
			GOTO ExitTotal IF PR_EMP_MASTER::EMPNAME > TO_ITEM$

		CASE "SN"
			GOTO ExitTotal IF PR_EMP_MASTER::SSN > TO_ITEM$

		CASE "LO"
			GOTO ExitTotal IF PR_EMP_MASTER::LOCATION > TO_ITEM$

		CASE ELSE
			GOTO ExitTotal IF PR_EMP_MASTER::SORT > TO_ITEM$

		END SELECT

	END IF

	!
	! Give warnings if there are any blank hire dates
	!
	IF PR_EMP_MASTER::HIREDAY = ""
	THEN
		BLANK_EMPLOYEE% = BLANK_EMPLOYEE% + 1%
		BLANK_EMPLOYEE$(BLANK_EMPLOYEE%) = PR_EMP_MASTER::EMPNUM
	END IF

	!
	! See if employee meets hire date requirements
	!
	IF (PR_EMP_MASTER::HIREDAY >= HIRED_BEFORE$) AND &
		(HIRED_BEFORE$ <> "        ")
	THEN
		GOTO GetNextRec
	END IF

17030	!
	! Try to read master file
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM
	USE
		CONTINUE 17020 IF ERR = 155%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 17020 IF PR_TEMP::SALARY = 0.0
	GOTO 17020 IF PR_TEMP::HOURS < MIN_HOURS

17040	!
	! Set up output text
	!
	TEXT$ = &
		PR_TEMP::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		PR_EMP_MASTER::SSN + " " + &
		PR_EMP_MASTER::SEX + " " + &
		PRNT_DATE(PR_EMP_MASTER::BIRTH, 8%) + " " + &
		PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%) + " " + &
		FORMAT$(PR_TEMP::SALARY, "###,###,###.## ") + &
		FORMAT$(PR_TEMP::HOURS, "######.# ")

	TOTAL_DOLLARS = TOTAL_DOLLARS + PR_TEMP::SALARY
	TOTAL_HOURS = TOTAL_HOURS + PR_TEMP::HOURS

	IF (PR_EMP_MASTER::TERMDAY > "00000000")
	THEN
		TEXT$ = TEXT$ + &
			" T:" + PRNT_DATE(PR_EMP_MASTER::TERMDAY, 8%)
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:

	TEXT$ = &
		"           " + &
		"                               " + &
		"             " + &
		"  " + &
		"           " + &
		"           " + &
		FORMAT$(TOTAL_DOLLARS, "###,###,###.## ") + &
		FORMAT$(TOTAL_HOURS, "######.# ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF BLANK_EMPLOYEE% <> 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"*** Employees with blank starting dates ***", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		FOR LOOP% = 1% TO BLANK_EMPLOYEE%
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ BLANK_EMPLOYEE$(LOOP%), &
				REGARDLESS

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				BLANK_EMPLOYEE$(LOOP%) + " " + &
				PR_EMP_MASTER::EMPNAME, 0%)

		NEXT LOOP%
	END IF

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

18000	!*******************************************************************
	! Subroutine to store salary amount in temp file
	!*******************************************************************
 SetSalary:
	GOTO 18090 IF THIS_SALARY = 0.0

	WHEN ERROR IN
		GET #PR_TEMP.CH%, KEY #0% EQ THIS_EMPNUM$
	USE
		CONTINUE 18010 IF ERR = 155%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	PR_TEMP::SALARY = PR_TEMP::SALARY + THIS_SALARY
	PR_TEMP::HOURS  = PR_TEMP::HOURS  + THIS_HOURS

	UPDATE #PR_TEMP.CH%

	GOTO 18090

18010	PR_TEMP::EMPNUM = THIS_EMPNUM$
	PR_TEMP::SALARY = THIS_SALARY
	PR_TEMP::HOURS  = THIS_HOURS

	PUT #PR_TEMP.CH%

18090	THIS_EMPNUM$ = PR_TRN_PAY::EMPNUM
	THIS_SALARY = 0.0
	THIS_HOURS  = 0.0

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

32767	END
