1	%TITLE "Print Payroll Check Using a Form"
	%SBTTL "PR_FORM_CHECK_KINGB"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.b
	!	^*CHECK - Print Payroll Checks\*
	!	.b
	!	.lm +5
	!	The ^*Print Payroll Checks\* option
	!	prints the payroll checks.
	!	.b
	!	The checks may be printed in employee number, alphabetical or
	!	location order.
	!	.b
	!	^*Note\*:  If any check(s) needs to be reprinted due to an error or perhaps due
	!	to the forms jamming in the printer, access the maintenance option and
	!	blank the check number and check date for the employee(s) whose check(s)
	!	needs to be reprinted, then reaccess the ^*Print Payroll Checks\* routine.
	!	Checks will be printed only for those employees whose check record contains
	!	blanks in the check number field.
	!	.lm -5
	!
	! Index:
	!	.x Check>Reprint
	!	.x Print>Payroll Checks
	!	.x Payroll Checks>Print
	!	.x Checks>Print
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FORM_CHECK_KINGB
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_CHECK_KINGB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_CHECK_KINGB.OBJ;*
	!
	! Author:
	!
	!	12/05/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax package.
	!
	!	04/18/89 - Kevin Handy
	!		Modified to look at check number, no check
	!		date to decide if check needs to be printed.
	!
	!	04/24/89 - Kevin Handy
	!		Added standard rate(s) to usable values.
	!
	!	04/26/89 - Kevin Handy
	!		Fixed problem with TO_ITEM$ when by location
	!		is selected.
	!
	!	04/26/89 - Kevin Handy
	!		Modified to use DATE_STOREDATE to set the check
	!		date in the PR_TRN_CHECK file instead of using
	!		hardcoded left, mid, right.
	!
	!	10/07/89 - Kevin Handy
	!		Modified to store totals in a structure (array)
	!		instead of a double subscripted array.
	!		This will make it much, much easier to handle
	!		KIDK's check format.
	!
	!	10/18/89 - Kevin Handy
	!		Fixed bug that caused a later employee to
	!		receive a previous employee's earnings.
	!		Would also cause things not printed on check
	!		due to space problems not to show up in the
	!		employees check amount.
	!
	!	12/14/89 - Kevin Handy
	!		Fixed bug where was pulling random information
	!		on some checks.
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/06/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	06/19/91 - Craig Tanner
	!		Added section to update UTL_REPORT from master.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to handle "A" earn types in REG_ERNDED.
	!		(Ignore them)
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" types in PR_PAY files.
	!
	!	06/16/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	06/09/93 - Kevin Handy
	!		Don't print zero check.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last param to entr_3choices.
	!
	!	04/26/95 - Kevin Handy
	!		Removed delete of SMG_BLANK display, which is
	!		never created.
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/12/97 - Kevin Handy
	!		Add FH code.
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/99 - Kevin Handy
	!		Increase number of digits in several fields where
	!		possible without changing spacing.
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE parameter to PR_READ_RATE
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/15/2005 - Kevin Handy
	!		Make check number variables integers.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (CHECK_FORM) &
		NET_CHECK, &
		CHECK_DATE$, &
		LAST_CKNUM%

	COMMON (PR_FORM_CHECK_KINGB) &
		DEFAULT_RTYPE$ = 1%, &
		DEFAULT_CODE$ = 2%, &
		DEFAULT_HOUR_RATE, &
		DEFAULT_PIECE_RATE, &
		DEFAULT_FACTOR%, &
		DEFAULT_STDEFF, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%

	!
	! Structures
	!
	RECORD EMP_TOTAL_PAY_STRUCTURE
		STRING	CODE = 2
		REAL	REG_HR
		REAL	OVT_HR
		REAL	PIECE
		REAL	GROSS
		REAL	PREMIUM
		REAL	YTD_GROSS
	END RECORD

	DIM EMP_TOTAL_PAY_STRUCTURE EMP_TOTAL_PAY(30%)
	DECLARE EMP_TOTAL_PAY_STRUCTURE EMP_SUB_PAY

	RECORD EMP_TOTAL_TAX_STRUCTURE
		STRING CODE = 2
		STRING TAX_CODE = 2
		REAL AMOUNT
		REAL YTD_AMOUNT
	END RECORD

	DIM EMP_TOTAL_TAX_STRUCTURE EMP_TOTAL_TAX(30%)
	DECLARE EMP_TOTAL_TAX_STRUCTURE EMP_SUB_TAX

	RECORD EMP_TOTAL_DED_STRUCTURE
		STRING ETYPE = 1
		STRING CODE = 2
		REAL AMOUNT
		REAL YTD_AMOUNT
	END RECORD

	DIM EMP_TOTAL_DED_STRUCTURE EMP_TOTAL_DED(30%)
	DECLARE EMP_TOTAL_DED_STRUCTURE EMP_SUB_DED

	RECORD EMP_TOTAL_NON_STRUCTURE
		STRING DTYPE = 1
		STRING CODE = 2
		REAL AMOUNT
		REAL YTD_AMOUNT
	END RECORD

	DIM EMP_TOTAL_NON_STRUCTURE EMP_TOTAL_NON(30%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	!
	! Dimension
	!
	DIM TEXT$(40%), REVERSE_LIST$(200%), DATE_FILE$(200%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PRCHK"

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)


100	!-----------------------------------------------------------------
	! Get payroll file to print checks for
	!-----------------------------------------------------------------

	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", DATE_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	DATE_FILE% = VAL%(DATE_FILE$(0%))

	IF DATE_FILE%
	THEN
		REVERSE_LIST$(DATE_FILE% - LOOP% + 1%) = &
			MID(DATE_FILE$(LOOP%), 16%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 18%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 12%, 4%) &
			FOR LOOP% = DATE_FILE% TO 1% STEP -1%

		TEMP$ = "Payroll Folder Dates"

		X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), &
			"", 0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
				LEFT(REVERSE_LIST$(X%), 2%) + &
				MID(REVERSE_LIST$(X%), 4%, 2%)
			GOTO 190
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	BATCH_NO$ = DATE_TODAY

120	PR_TRN_PAY_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter Payroll Folder Date (MMDDYYYY) ", &
		BATCH_NO$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			! Good key

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 120
	END SELECT

	PR_TRN_PAY_DATE$ = EDIT$(PR_TRN_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_TRN_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the folder date in (MMDDYYYY) format", 0%)
		GOTO 120
	END IF

	BATCH_NO$ = PR_TRN_PAY_DATE$

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	YYYY$ = LEFT(BATCH_NO$, 4%)

	%Page

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open Payroll Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!*********************************************************************
	! Test to see if payroll has been closed or posted
	!	1 - Updated to Register
	!	2 - Accrued Post
	!	4 - Final Post
	!*********************************************************************

	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%

		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", &
			"ERR", "PR_CLOSED", "ERROR_PR_CLOSED")
		GOTO ExitProgram
	END IF

	IF (PR_TRN_PAY::UPDATE_FLAG AND 4%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", &
			"ERR", "PR_POSTED", "ERROR_PR_POSTED")
		GOTO ExitProgram
	END IF

320	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Std APD file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

360	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

370	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 400 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%Page

400	!
	! Open REPORT file
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	GOTO ExitProgram IF UTL_REPORTX::STAT

	LAST_CKNUM% = VAL%(XLATE(UTL_REPORTX::OPTDEF(0%), &
		STRING$(48%, 0%) + "0123456789"))
	!++
	!
	! Abstract:FLD01
	!	^*(01) Start Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Start Check _#\* field enters the
	!	number of the check with which the printing will begin.
	!	.lm -5
	!
	! Index:
	!
	!--

	BREAK_CKNUM% = VAL%(XLATE(UTL_REPORTX::OPTDEF(1%), &
		STRING$(48%, 0%) + "0123456789"))
	!++
	!
	! Abstract:FLD02
	!	^*(02) Break Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Break Check _#\* field enters the number of
	!	the check subsequent to the number of the last check which is to
	!	be printed when there is a break in the check forms, end of a box, etc.
	!	.b
	!	For example, if ^*1395\* were entered in this field, the printer
	!	would stop after printing check number ^*1394\*. The printer may then
	!	be loaded with additional check forms. After pressing ^*Resume\*,
	!	the Report Setting Screen will reappear, facilitating the resumption
	!	of the check printing routine.
	!	.lm -5
	!
	! Index:
	!	.x Break Check
	!	.x Check Break
	!
	!--

	CHECK_DATE$ = UTL_REPORTX::OPTDEF(2%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field enters the
	!	appropriate date which will be printed on the checks.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) From Item\*
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	!++
	!
	! Abstract:FLD05
	!
	!	^*(05) To item\*
	!
	! Index:
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	!
	! Abstract:FLD06
	!	^*(06) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field determines the order
	!	in which the checks will print.
	!	.b
	!	The valid values and related
	!	sort orders are:
	!	.table 3,25
	!	.te
	!	^*NU\*  - Number
	!	.te
	!	^*NA\*  - Name
	!	.te
	!	^*SO\*  - Sort Order
	!	.te
	!	^*LO\*  - Location
	!	.end table
	!	.lm -5
	!
	! Index:
	!
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

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(9%))
	!++
	! Abstract:FLD10
	!	^*(10) Form Name\*
	!	.b
	!	.lm +5
	!	The ^*Form Name\* field enters the form which will be
	!	used in the printing.
	!	.lm -5
	!
	! Index:
	!	.x Form Name
	!
	!--

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! Load in form
	!
	GOSUB LoadForm

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Notify user that file is being processed
	!
	CALL ENTR_3MESSAGE(SCOPE, "Processing.  Please wait. . .", 1%)

	!
	! Jump over the reset if break check option is used
	!
	IF BREAK_FLAG%
	THEN
		GOTO 17120 IF DO_NOT_ADD_REG% = -1%
		GOTO 17100
	END IF

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
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "NU"
		GOTO ExitProgram IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "NA"
		GOTO ExitProgram IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SN"
		GOTO ExitProgram IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "LO"
		GOTO ExitProgram IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitProgram IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	ERN_LOOP%, TAX_LOOP%, DED_LOOP%, NON_LOOP% = 0%
	DO_NOT_ADD_REG% = 0%

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(PR_EMP_MASTER::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		PR_EMP_MASTER.ADDLINE$(I%) = &
			EDIT$(PR_EMP_MASTER::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(PR_EMP_MASTER::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		PR_EMP_MASTER.ADDLINE$(I%) = &
			EDIT$(PR_EMP_MASTER::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	PR_EMP_MASTER.ADDLINE$(I%) = &
		EDIT$(EDIT$(PR_EMP_MASTER::CITY, 128%) + ", " + &
		PR_EMP_MASTER::STATE + " " + PR_EMP_MASTER::ZIP + " " + &
		PR_EMP_MASTER::COUNTRY, 8% + 16% + 32% + 128%)

	PR_EMP_MASTER.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	!
	! Set test check to null for the next employee record
	!
	TEST_PR_END_DATE$ = ""

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		PR_TRN_PAY::EMPNUM = "" IF ERR = 11%
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM) OR &
		(PR_TRN_PAY::PR_END_DATE <> TEST_PR_END_DATE$ AND &
		TEST_PR_END_DATE$ <> "")

	GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

17120	TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	GOTO 17130 IF EMP_TOTAL_PAY(LOOP%)::CODE = PR_TRN_PAY::CODE &
		FOR LOOP% = 1% TO ERN_LOOP%
	ERN_LOOP%, LOOP% = ERN_LOOP% + 1%
	EMP_TOTAL_PAY(LOOP%)::CODE = PR_TRN_PAY::CODE
	EMP_TOTAL_PAY(LOOP%)::REG_HR = 0.0
	EMP_TOTAL_PAY(LOOP%)::OVT_HR = 0.0
	EMP_TOTAL_PAY(LOOP%)::PIECE = 0.0
	EMP_TOTAL_PAY(LOOP%)::GROSS = 0.0
	EMP_TOTAL_PAY(LOOP%)::PREMIUM = 0.0
	EMP_TOTAL_PAY(LOOP%)::YTD_GROSS = 0.0

17130	EMP_TOTAL_PAY(LOOP%)::REG_HR = &
		FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::REG_HR + &
		PR_TRN_PAY::REG_HR, 2%)
	EMP_TOTAL_PAY(LOOP%)::OVT_HR = &
		FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::OVT_HR + &
		PR_TRN_PAY::OVT_HR, 2%)
	EMP_TOTAL_PAY(LOOP%)::PIECE = FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::PIECE + &
		PR_TRN_PAY::PIECE, 2%)
	EMP_TOTAL_PAY(LOOP%)::GROSS = FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::GROSS + &
		PR_TRN_PAY::GROSS, 2%)
	EMP_TOTAL_PAY(LOOP%)::PREMIUM = &
		FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::PREMIUM + &
		PR_TRN_PAY::OVT_HR * PR_TRN_PAY::HOUR_RATE * &
		PR_TRN_PAY::FACTOR / 100, 2%)
	EMP_TOTAL_PAY(LOOP%)::YTD_GROSS = &
		FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::YTD_GROSS + &
		PR_TRN_PAY::GROSS, 2%) &
		IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) <> 1%

	GOTO 17110

17200	!
	! Get Tax/Ded detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 17300 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) OR &
		(PR_TRN_DED::PR_END_DATE <> TEST_PR_END_DATE$)

17220	GOTO 17250 &
		IF INSTR(1%, "!FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!", &
		PR_TRN_DED::CODE)

	GOTO 17270 IF PR_TRN_DED::DTYPE = "M" OR PR_TRN_DED::DTYPE = "T"

	!
	! Add Deductions
	!
	GOTO 17230 &
		IF EMP_TOTAL_DED(LOOP%)::CODE = PR_TRN_DED::CODE AND &
		EMP_TOTAL_DED(LOOP%)::ETYPE = PR_TRN_DED::DTYPE &
		FOR LOOP% = 1% TO DED_LOOP%

	DED_LOOP%, LOOP% = DED_LOOP% + 1%
	EMP_TOTAL_DED(LOOP%)::ETYPE = PR_TRN_DED::DTYPE
	EMP_TOTAL_DED(LOOP%)::CODE = PR_TRN_DED::CODE
	EMP_TOTAL_DED(LOOP%)::AMOUNT = 0.0
	EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT = 0.0

17230	EMP_TOTAL_DED(LOOP%)::AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_DED(LOOP%)::AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%)
	EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%) &
		IF (PR_TRN_DED::UPDATE_FLAG AND 1%) <> 1%

	GOTO 17290

17250	!
	! Add taxes
	!
	GOTO 17260 IF (EMP_TOTAL_TAX(LOOP%)::CODE = PR_TRN_DED::CODE) &
		AND (EMP_TOTAL_TAX(LOOP%)::TAX_CODE = PR_TRN_DED::TAX_CODE) &
		FOR LOOP% = 1% TO TAX_LOOP%

	TAX_LOOP%, LOOP% = TAX_LOOP% + 1%
	EMP_TOTAL_TAX(LOOP%)::CODE = PR_TRN_DED::CODE
	EMP_TOTAL_TAX(LOOP%)::TAX_CODE = PR_TRN_DED::TAX_CODE
	EMP_TOTAL_TAX(LOOP%)::AMOUNT = 0.0
	EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT = 0.0

17260	EMP_TOTAL_TAX(LOOP%)::AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_TAX(LOOP%)::AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%)
	EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%) &
		IF (PR_TRN_DED::UPDATE_FLAG AND 1%) <> 1%

	GOTO 17290

17270	!
	! Add noncompensation items
	!
	GOTO 17280 IF (EMP_TOTAL_NON(LOOP%)::DTYPE = PR_TRN_DED::DTYPE) AND &
		(EMP_TOTAL_NON(LOOP%)::CODE = PR_TRN_DED::CODE) &
		FOR LOOP% = 1% TO NON_LOOP%
	NON_LOOP%, LOOP% = NON_LOOP% + 1%
	EMP_TOTAL_NON(LOOP%)::DTYPE = PR_TRN_DED::DTYPE
	EMP_TOTAL_NON(LOOP%)::CODE = PR_TRN_DED::CODE

17280	EMP_TOTAL_NON(LOOP%)::AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_NON(LOOP%)::AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%)
	EMP_TOTAL_NON(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_NON(LOOP%)::YTD_AMOUNT + &
		PR_TRN_DED::AMOUNT, 2%) &
		IF (PR_TRN_DED::UPDATE_FLAG AND 1%) <> 1%

17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	!
	! Get the check number now
	!
	PR_TRN_CHECK::EMPNUM		= PR_EMP_MASTER::EMPNUM
	PR_TRN_CHECK::PR_END_DATE	= TEST_PR_END_DATE$
	PR_TRN_CHECK::CHECK		= ""
	PR_TRN_CHECK::CHECK_DATE	= ""
	PR_TRN_CHECK::PAYFREQ		= PR_EMP_MASTER::PAYFREQ

	PR_TRN_CHECK::UPDATE_FLAG	= 0%

	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + TEST_PR_END_DATE$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17305 IF ERR = 155%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 17310

17305	WHEN ERROR IN
		PUT #PR_TRN_CHECK.CH%

		GET #PR_TRN_CHECK.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + TEST_PR_END_DATE$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

17310	IF EDIT$(PR_TRN_CHECK::CHECK, -1%) = ""
	THEN
		GOSUB PrintEmployee
	ELSE
		IF PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM
		THEN
			EMP_TOTAL_PAY(I%)::YTD_GROSS = &
				EMP_TOTAL_PAY(I%)::YTD_GROSS + &
				EMP_TOTAL_PAY(I%)::GROSS FOR I% = 1% TO 30%

			EMP_TOTAL_TAX(I%)::YTD_AMOUNT = &
				EMP_TOTAL_TAX(I%)::YTD_AMOUNT + &
				EMP_TOTAL_TAX(I%)::AMOUNT FOR I% = 1% TO 30%

			EMP_TOTAL_DED(I%)::YTD_AMOUNT = &
				EMP_TOTAL_DED(I%)::YTD_AMOUNT + &
				EMP_TOTAL_DED(I%)::AMOUNT FOR I% = 1% TO 30%
		END IF
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM
	THEN
		!
		! Zero current amounts, but leave YTD
		!
		EMP_TOTAL_PAY(I%)::REG_HR = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_PAY(I%)::OVT_HR = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_PAY(I%)::PIECE = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_PAY(I%)::GROSS = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_PAY(I%)::PREMIUM = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_TAX(I%)::AMOUNT = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_DED(I%)::AMOUNT = 0.0 FOR I% = 1% TO 30%
		EMP_TOTAL_NON(I%)::AMOUNT = 0.0 FOR I% = 1% TO 30%
	END IF

17320	!
	! Check for a break check
	!
	IF (LAST_CKNUM% >= BREAK_CKNUM%) AND (BREAK_CKNUM% > 0.0)
	THEN
		!
		! Erase Display
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		!
		! Change the width
		!
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			80%)

		SCOPE::PRG_ITEM = "BREAK_CHECK"
		CALL ENTR_3MESSAGE(SCOPE, "Break check has been reached", 0%)

		SELECT SCOPE::SCOPE_EXIT

		!
		! An exit key was typed
		!
		CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
			GOTO ExitProgram

		!
		! Return, etc. act as next screen
		!
		CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

		!
		! Case else
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 17320

		END SELECT

		BREAK_FLAG% = -1%

		GOTO ExitProgram
	END IF

	IF PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM
	THEN
		DO_NOT_ADD_REG% = -1%
		GOTO 17120
	END IF

17350	!
	! End of report
	!
	GOTO 17020

	%Page

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE PR_EMP_MASTER.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE


 PrintEmployee:
	! Skip first step
	!
	GOTO 18200 IF DO_NOT_ADD_REG%

	!******************************************************************
	! Add register to current data
	!******************************************************************

18000	!
	! Look up ernded register record
	!
	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 18100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

18010	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 18100 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 18100 IF PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM

	GOTO 18010 IF PR_REG_ERNDED::ETYPE = "A"

	GOTO 18050 IF PR_REG_ERNDED::ETYPE <> "P"

	!
	! Add earnings
	!
	GOTO 18020 IF EMP_TOTAL_PAY(LOOP%)::CODE = PR_REG_ERNDED::CODE &
		FOR LOOP% = 1% TO ERN_LOOP%
	ERN_LOOP%, LOOP% = ERN_LOOP% + 1%
	EMP_TOTAL_PAY(LOOP%)::CODE = PR_REG_ERNDED::CODE
	EMP_TOTAL_PAY(LOOP%)::REG_HR = 0.0
	EMP_TOTAL_PAY(LOOP%)::OVT_HR = 0.0
	EMP_TOTAL_PAY(LOOP%)::PIECE = 0.0
	EMP_TOTAL_PAY(LOOP%)::GROSS = 0.0
	EMP_TOTAL_PAY(LOOP%)::PREMIUM = 0.0
	EMP_TOTAL_PAY(LOOP%)::YTD_GROSS = 0.0

18020	EMP_TOTAL_PAY(LOOP%)::YTD_GROSS = &
		FUNC_ROUND(EMP_TOTAL_PAY(LOOP%)::YTD_GROSS + &
		PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
		FOR I% = 0% TO 3%

	GOTO 18090

18050	GOTO 18070 &
		IF PR_REG_ERNDED::ETYPE <> "D" AND PR_REG_ERNDED::ETYPE <> "F"

	!
	! Add deductions
	!
	GOTO 18060 &
		IF EMP_TOTAL_DED(LOOP%)::CODE = PR_REG_ERNDED::CODE AND &
		EMP_TOTAL_DED(LOOP%)::ETYPE = PR_REG_ERNDED::ETYPE &
		FOR LOOP% = 1% TO DED_LOOP%
	DED_LOOP%, LOOP% = DED_LOOP% + 1%
	EMP_TOTAL_DED(LOOP%)::ETYPE = PR_REG_ERNDED::ETYPE
	EMP_TOTAL_DED(LOOP%)::CODE = PR_REG_ERNDED::CODE
	EMP_TOTAL_DED(LOOP%)::AMOUNT = 0.0
	EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT = 0.0

18060	EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_DED(LOOP%)::YTD_AMOUNT + &
		PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
		FOR I% = 0% TO 3%

	GOTO 18090

18070	!
	! Add noncompensation items
	!
	GOTO 18080 IF (EMP_TOTAL_NON(LOOP%)::DTYPE = PR_REG_ERNDED::ETYPE) AND &
		(EMP_TOTAL_NON(LOOP%)::CODE = PR_REG_ERNDED::CODE) &
		FOR LOOP% = 1% TO NON_LOOP%
	NON_LOOP%, LOOP% = NON_LOOP% + 1%
	EMP_TOTAL_NON(LOOP%)::DTYPE = PR_REG_ERNDED::ETYPE
	EMP_TOTAL_NON(LOOP%)::CODE = PR_REG_ERNDED::CODE
	EMP_TOTAL_NON(LOOP%)::AMOUNT = 0.0
	EMP_TOTAL_NON(LOOP%)::YTD_AMOUNT = 0.0

18080	EMP_TOTAL_NON(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_NON(LOOP%)::YTD_AMOUNT + &
		PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
		FOR I% = 0% TO 3%

18090	!
	! Get next ernded register record
	!
	GOTO 18010

18100	!*******************************************************************
	! Look up taxwh register record
	!*******************************************************************
	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 18200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

18110	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 18200 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 18200 IF PR_EMP_MASTER::EMPNUM <> PR_REG_TAXES::EMPNUM

	!
	! Add taxes
	!
	GOTO 18120 IF (EMP_TOTAL_TAX(LOOP%)::CODE = PR_REG_TAXES::TTYPE) AND &
		(EMP_TOTAL_TAX(LOOP%)::TAX_CODE = PR_REG_TAXES::CODE) &
		FOR LOOP% = 1% TO TAX_LOOP%

	TAX_LOOP%, LOOP% = TAX_LOOP% + 1%
	EMP_TOTAL_TAX(LOOP%)::CODE = PR_REG_TAXES::TTYPE
	EMP_TOTAL_TAX(LOOP%)::TAX_CODE = PR_REG_TAXES::CODE
	EMP_TOTAL_TAX(LOOP%)::AMOUNT = 0.0
	EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT = 0.0

18120	EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT = &
		FUNC_ROUND(EMP_TOTAL_TAX(LOOP%)::YTD_AMOUNT + &
		PR_REG_TAXES::TAX(I%), 2%) &
		FOR I% = 0% TO 3%

	GOTO 18110

18200	!******************************************************************
	! Now we look up the accruals
	!******************************************************************

18300	!********************************************************************
	! Now we are finally ready to print the employee data
	!********************************************************************

	!
	! Pull up default rate(s) for this employee
	!
	CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
		PR_EMP_MASTER::OPER, &
		TEST_PR_END_DATE$, &
		DEFAULT_RTYPE$, &
		DEFAULT_CODE$, &
		DEFAULT_HOUR_RATE, &
		DEFAULT_PIECE_RATE, &
		DEFAULT_FACTOR%, &
		DEFAULT_STDEFF, &
		DEFAULT_EVALDATE$, &
		EFF_DATE$)

	EMP_SUB_PAY::REG_HR = 0.0
	EMP_SUB_PAY::OVT_HR = 0.0
	EMP_SUB_PAY::PIECE = 0.0
	EMP_SUB_PAY::GROSS = 0.0
	EMP_SUB_PAY::YTD_GROSS = 0.0

	EMP_SUB_TAX::AMOUNT = 0.0
	EMP_SUB_TAX::YTD_AMOUNT = 0.0

	EMP_SUB_DED::AMOUNT = 0.0
	EMP_SUB_DED::YTD_AMOUNT = 0.0

	TEXT$(I%) = "" FOR I% = 1% TO 40%

	!
	! Create sub-totals
	!
	FOR I% = 1% TO ERN_LOOP%
		!
		! Earnings
		!
		EMP_SUB_PAY::REG_HR = EMP_SUB_PAY::REG_HR + &
			EMP_TOTAL_PAY(I%)::REG_HR
		EMP_SUB_PAY::OVT_HR = EMP_SUB_PAY::OVT_HR + &
			EMP_TOTAL_PAY(I%)::OVT_HR
		EMP_SUB_PAY::PIECE = EMP_SUB_PAY::PIECE + &
			EMP_TOTAL_PAY(I%)::PIECE
		EMP_SUB_PAY::GROSS = EMP_SUB_PAY::GROSS + &
			EMP_TOTAL_PAY(I%)::GROSS
		EMP_SUB_PAY::PREMIUM = EMP_SUB_PAY::GROSS + &
			EMP_TOTAL_PAY(I%)::PREMIUM
		EMP_SUB_PAY::YTD_GROSS = EMP_SUB_PAY::YTD_GROSS + &
			EMP_TOTAL_PAY(I%)::YTD_GROSS
	NEXT I%

	FOR I% = 1% TO TAX_LOOP%
		!
		! Taxes
		!
		EMP_SUB_TAX::AMOUNT = EMP_SUB_TAX::AMOUNT + &
			EMP_TOTAL_TAX(I%)::AMOUNT
		EMP_SUB_TAX::YTD_AMOUNT = EMP_SUB_TAX::YTD_AMOUNT + &
			EMP_TOTAL_TAX(I%)::YTD_AMOUNT
	NEXT I%

	FOR I% = 1% TO DED_LOOP%
		!
		! Deductions
		!
		EMP_SUB_DED::AMOUNT = EMP_SUB_DED::AMOUNT + &
			EMP_TOTAL_DED(I%)::AMOUNT
		EMP_SUB_DED::YTD_AMOUNT = EMP_SUB_DED::YTD_AMOUNT + &
			EMP_TOTAL_DED(I%)::YTD_AMOUNT
	NEXT I%

	W1% = 49%
	W2% = 97%

	! Print earnings data
	TEXT_LOOP% = 0%
	FOR I% = 1% TO ERN_LOOP%

		IF (EMP_TOTAL_PAY(I%)::REG_HR <> 0.0) OR &
			(EMP_TOTAL_PAY(I%)::GROSS - &
			EMP_TOTAL_PAY(I%)::PREMIUM <> 0.0)
		THEN
			E_TYPE$ = "P"
			CODE$ = EMP_TOTAL_PAY(I%)::CODE
			GOSUB LookUpED
			TEXT_LOOP% = TEXT_LOOP% + 1%
			TEXT$(TEXT_LOOP%) = "      " + &
				LEFT(DESCR$ + SPACE$(21%), 21%) + &
				FORMAT$(EMP_TOTAL_PAY(I%)::REG_HR, "###.##") + &
				FORMAT$(EMP_TOTAL_PAY(I%)::GROSS - &
				EMP_TOTAL_PAY(I%)::PREMIUM, "########.##")
		END IF

		IF (EMP_TOTAL_PAY(I%)::OVT_HR <> 0.0) OR &
			(EMP_TOTAL_PAY(I%)::PREMIUM <> 0.0)
		THEN
			E_TYPE$ = "P"
			CODE$ = EMP_TOTAL_PAY(I%)::CODE
			GOSUB LookUpED
			TEXT_LOOP% = TEXT_LOOP% + 1%
			TEXT$(TEXT_LOOP%) = "      " + &
				"OVERTIME             " + &
				FORMAT$(EMP_TOTAL_PAY(I%)::OVT_HR, "###.##") + &
				FORMAT$(EMP_TOTAL_PAY(I%)::PREMIUM, "########.##")
		END IF

	NEXT I%

	! Print Deduction data
	TEXT_LOOP% = 0%
	FOR I% = 1% TO DED_LOOP%

		TEXT_LOOP% = TEXT_LOOP% + 1%

		TEXT$(TEXT_LOOP%) = LEFT(TEXT$(TEXT_LOOP%) + SPACE$(W1%), W1%)

		E_TYPE$ = EMP_TOTAL_DED(I%)::ETYPE
		CODE$ = EMP_TOTAL_DED(I%)::CODE
		GOSUB LookUpED
		TEXT$(TEXT_LOOP%) = TEXT$(TEXT_LOOP%) + &
			LEFT(DESCR$ + SPACE$(20%), 20%) + &
			FORMAT$(EMP_TOTAL_DED(I%)::AMOUNT, "#####.##") + &
			FORMAT$(EMP_TOTAL_DED(I%)::YTD_AMOUNT, "########.##")

	NEXT I%

	! Print Tax data
	TEXT_LOOP% = 0%
	FOR I% = 1% TO TAX_LOOP%

		TEXT_LOOP% = TEXT_LOOP% + 1%
		TEXT$(TEXT_LOOP%) = LEFT(TEXT$(TEXT_LOOP%) + SPACE$(W2%), W2%)

		E_TYPE$ = EMP_TOTAL_TAX(I%)::CODE
		CODE$ = EMP_TOTAL_TAX(I%)::TAX_CODE

		SELECT E_TYPE$
		CASE "FI"
			DESCR$ = "FICA"
		CASE "FH"
			DESCR$ = "FICA (HI)"
		CASE "FW"
			DESCR$ = "FED/WH"
		CASE "SW"
			DESCR$ = "STATE/WH " + CODE$
		CASE "SX"
			DESCR$ = "OST " + CODE$
		CASE "CW"
			DESCR$ = "CITY/WH " + CODE$
		CASE "DW"
			DESCR$ = "COUNTY/WH " + CODE$
		CASE "EW"
			DESCR$ = "SCHOOL/WH " + CODE$
		CASE "SU"
			DESCR$ = "STATE/UN " + CODE$
		CASE "SI"
			DESCR$ = "WC/LI " + CODE$
		CASE ELSE
			DESCR$ = "?????????????????????????????"
		END SELECT

		TEXT$(TEXT_LOOP%) = TEXT$(TEXT_LOOP%) + &
			LEFT(DESCR$ + SPACE$(12%), 12%) + &
			FORMAT$(EMP_TOTAL_TAX(I%)::AMOUNT, "#####.##") + &
			FORMAT$(EMP_TOTAL_TAX(I%)::YTD_AMOUNT, "########.##")
	NEXT I%

	NET_CHECK = FUNC_ROUND(EMP_SUB_PAY::GROSS - EMP_SUB_TAX::AMOUNT - &
		EMP_SUB_DED::AMOUNT, 2%)

	TEXT_LOOP% = TEXT_LOOP% + 1%
	TEXT$(I%) = LEFT(TEXT$(I%) + SPACE$(132%), 132%) &
		FOR I% = 8% TO 14%

	TEXT$(8%) = LEFT(TEXT$(8%), W2%) + &
		"          " + &
		FORMAT$(EMP_SUB_TAX::AMOUNT, "#######.##") + &
		FORMAT$(EMP_SUB_TAX::YTD_AMOUNT, "########.##")
	TEXT$(11%) = LEFT(TEXT$(11%), W2%) + &
		CHECK_DATE$ + "        " + &
		NUM1$(LAST_CKNUM%)
	TEXT$(12%) = "                      " + &
		FORMAT$(EMP_SUB_PAY::REG_HR + EMP_SUB_PAY::OVT_HR, "###.##") + &
		"  " + &
		FORMAT$(EMP_SUB_PAY::GROSS, "##########.##") + &
		"                " + &
		FORMAT$(EMP_SUB_DED::AMOUNT, "##########.##") + &
		FORMAT$(EMP_SUB_DED::YTD_AMOUNT, "##########.##")
	TEXT$(13%) = "                              " + &
		FORMAT$(EMP_SUB_PAY::YTD_GROSS, "##########.##")
	TEXT$(14%) = LEFT(TEXT$(14%), W2%) + &
		"                      " + &
		FORMAT$(EMP_SUB_PAY::GROSS - &
			EMP_SUB_TAX::AMOUNT - &
			EMP_SUB_DED::AMOUNT, "#####.##")


	!
	! If this is a negative, or zero check - don't print it
	!
	IF NET_CHECK <= 0.0
	THEN
		GOTO 18390
	END IF

	!
	! Print the check if a top check
	!
	IF FRM_CHECK% < FRM_STUB%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUP%, &
			FORM_GROUP(), &
			0%)
	END IF

	!
	! Print the top of the stub
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_STUB%, &
		FORM_TEXT$, &
		FORM_GROUP%, &
		FORM_GROUP(), &
		0%)

	!
	! Print the detail on the stub
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$(I%), 0%) &
		FOR I% = 1% TO FORM_GROUP(FRM_LENGTH%)::NUMBER

	!
	! Print the check if a bottom check
	!
	IF FRM_CHECK% > FRM_STUB%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUP%, &
			FORM_GROUP(), &
			0%)
	END IF

18380	PR_TRN_CHECK::CHECK = NUM1$(LAST_CKNUM%)
	PR_TRN_CHECK::CHECK_DATE = DATE_STOREDATE(CHECK_DATE$)

	WHEN ERROR IN
		UPDATE #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Increment check number
	!
	LAST_CKNUM% = LAST_CKNUM% + 1%

18390	RETURN

	%Page


 LookUpED:
	!********************************************************************
	! Look up ernded description
	!********************************************************************
	DESCR$ = "?????????????????????????????????"

18900	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, KEY #0% EQ E_TYPE$ + CODE$, REGARDLESS
	USE
		CONTINUE 18990 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	DESCR$ = PR_ERNDED_DEF::DESCR

18990	RETURN

	%Page

 LoadForm:
	!*******************************************************************
	! Initilize Check form
	!*******************************************************************

	!
	! Get form from the PR form library
	!
	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Check form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_CHECK% = 0%
	FRM_STUB% = 0%
	FRM_LENGTH% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-CHECK"
			FRM_CHECK% = I%

		CASE "FRM-STUB"
			FRM_STUB% = I%

		CASE "FRM-LENGTH"
			FRM_LENGTH% = I%

		END SELECT

	NEXT I%

	RETURN

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	!
	! Print the check if a top check
	!
	IF FRM_CHECK% < FRM_STUB%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUP%, &
			FORM_GROUP(), &
			1%)
	END IF

	!
	! Print the top of the stub
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_STUB%, &
		FORM_TEXT$, &
		FORM_GROUP%, &
		FORM_GROUP(), &
		1%)

	TEXT$(I%) = "" FOR I% = 1% TO 40%

	!
	! Print the detail on the stub
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$(I%), 0%) &
		FOR I% = 1% TO FORM_GROUP(FRM_LENGTH%)::NUMBER

	!
	! Print the check if a bottom check
	!
	IF FRM_CHECK% > FRM_STUB%
	THEN
		SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_CHECK%, &
			FORM_TEXT$, &
			FORM_GROUP%, &
			FORM_GROUP(), &
			1%)
	END IF

	!
	! Increment check number
	!
	LAST_CKNUM% = LAST_CKNUM% + 1%

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	!
	! Untrapped error
	!
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	MAP (CHECK_FORM) &
		NET_CHECK, &
		CHECK_DATE$, &
		LAST_CKNUM%

	COMMON (PR_FORM_CHECK_KINGB) &
		DEFAULT_RTYPE$ = 1%, &
		DEFAULT_CODE$ = 2%, &
		DEFAULT_HOUR_RATE, &
		DEFAULT_PIECE_RATE, &
		DEFAULT_FACTOR%, &
		DEFAULT_STDEFF, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "?????????????????????????????????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!************************************************************
	! Fields for the PR_EMP_MASTER file
	!************************************************************

	CASE "PR_EMP_MASTER::EMPNUM"
		TEXTVALUE$ = PR_EMP_MASTER::EMPNUM

	CASE "PR_EMP_MASTER::EMPNAME"
		TEXTVALUE$ = PR_EMP_MASTER::EMPNAME

	CASE "PR_EMP_MASTER::ADD1"
		TEXTVALUE$ = PR_EMP_MASTER::ADD1

	CASE "PR_EMP_MASTER::ADD2"
		TEXTVALUE$ = PR_EMP_MASTER::ADD2

	CASE "PR_EMP_MASTER::CITY"
		TEXTVALUE$ = PR_EMP_MASTER::CITY

	CASE "PR_EMP_MASTER::STATE"
		TEXTVALUE$ = PR_EMP_MASTER::STATE

	CASE "PR_EMP_MASTER::ZIP"
		TEXTVALUE$ = PR_EMP_MASTER::ZIP

	CASE "PR_EMP_MASTER::COUNTRY"
		TEXTVALUE$ = PR_EMP_MASTER::COUNTRY

	CASE "PR_EMP_MASTER.ADDLINE1"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(1%)

	CASE "PR_EMP_MASTER.ADDLINE2"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(2%)

	CASE "PR_EMP_MASTER.ADDLINE3"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(3%)

	CASE "PR_EMP_MASTER::PHONE"
		TEXTVALUE$ = PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)

	CASE "PR_EMP_MASTER::SSN"
		TEXTVALUE$ = PRNT_SSN(PR_EMP_MASTER::SSN, 0%)

	CASE "PR_EMP_MASTER::SORT"
		TEXTVALUE$ = PR_EMP_MASTER::SORT

	CASE "PR_EMP_MASTER::SUBACC"
		TEXTVALUE$ = PR_EMP_MASTER::SUBACC

	CASE "PR_EMP_MASTER::ACCT"
		TEXTVALUE$ = PR_EMP_MASTER::ACCT

	CASE "PR_EMP_MASTER::TRADE"
		TEXTVALUE$ = PR_EMP_MASTER::TRADE

	CASE "PR_EMP_MASTER::OPER"
		TEXTVALUE$ = PR_EMP_MASTER::OPER

	CASE "PR_EMP_MASTER::UNION"
		TEXTVALUE$ = PR_EMP_MASTER::UNION

	CASE "PR_EMP_MASTER::LOCATION"
		TEXTVALUE$ = PR_EMP_MASTER::LOCATION

	CASE "PR_EMP_MASTER::DEPT"
		TEXTVALUE$ = PR_EMP_MASTER::DEPT

	CASE "PR_EMP_MASTER::WORK_CENTER"
		TEXTVALUE$ = PR_EMP_MASTER::WORK_CENTER

	CASE "PR_EMP_MASTER::EMP_SKILL"
		TEXTVALUE$ = PR_EMP_MASTER::EMP_SKILL

	CASE "PR_EMP_MASTER::EMP_GRADE"
		TEXTVALUE$ = PR_EMP_MASTER::EMP_GRADE

	CASE "PR_EMP_MASTER::DISABLED"
		TEXTVALUE$ = PR_EMP_MASTER::DISABLED

	CASE "PR_EMP_MASTER::PAYFREQ"
		REALVALUE = PR_EMP_MASTER::PAYFREQ

	CASE "PR_EMP_MASTER::SUI_SW"
		TEXTVALUE$ = PR_EMP_MASTER::SUI_SW

	CASE "PR_EMP_MASTER::TAX_PKG"
		TEXTVALUE$ = PR_EMP_MASTER::TAX_PKG

	CASE "PR_EMP_MASTER::W2_1099"
		TEXTVALUE$ = PR_EMP_MASTER::W2_1099

	CASE "PR_EMP_MASTER::BIRTH"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::BIRTH, 8%)

	CASE "PR_EMP_MASTER::HIREDAY"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%)

	CASE "PR_EMP_MASTER::TERMDAY"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::TERMDAY, 8%)

	CASE "PR_EMP_MASTER::SEX"
		TEXTVALUE$ = PR_EMP_MASTER::SEX

	CASE "PR_EMP_MASTER::RACE"
		TEXTVALUE$ = PR_EMP_MASTER::RACE

	CASE "PR_EMP_MASTER::USCIT"
		TEXTVALUE$ = PR_EMP_MASTER::USCIT

	CASE "PR_EMP_MASTER::WRKPERMIT"
		TEXTVALUE$ = PR_EMP_MASTER::WRKPERMIT

	CASE "PR_EMP_MASTER::HOMCNTRY"
		TEXTVALUE$ = PR_EMP_MASTER::HOMCNTRY

	CASE "PR_EMP_MASTER::ACTIVE_FLAG"
		TEXTVALUE$ = PR_EMP_MASTER::ACTIVE_FLAG

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "NET_CHECK"
		REALVALUE = NET_CHECK

	CASE "CHECK_NUMBER"
		REALVALUE = LAST_CKNUM%
		TEXTVALUE$ = NUM1$(LAST_CKNUM%)

	CASE "CHECK_DATE"
		TEXTVALUE$ = CHECK_DATE$

	CASE "RTYPE"
		TEXTVALUE$ = DEFAULT_RTYPE$

	CASE "CODE"
		TEXTVALUE$ = DEFAULT_CODE$

	CASE "HOUR_RATE"
		REALVALUE = DEFAULT_HOUR_RATE

	CASE "PIECE_RATE"
		REALVALUE = PIECE_RATE

	CASE "FACTOR"
		REALVALUE = DEFAULT_FACTOR%

	CASE "STDEFF"
		REALVALUE = DEFAULT_STDEFF

	END SELECT

	END SUB
	!+-+-+
	!++
	! Abstract:FLD08
	!	^*(08) Transmitter\*
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD07
	!	^*(07) Benefits Excluded\*
	!	.b
	!	.lm +5
	!	The ^*Benefits Excluded\* field allows for benefits ^~not\~ to be printed
	!	on the checks.
	!	.b
	!	This field does not require an entry.
	!
	! Index:
	!
	!--
