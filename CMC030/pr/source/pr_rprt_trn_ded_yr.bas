1	%TITLE "Payroll Deduction Report"
	%SBTTL "PR_RPRT_TRN_DED_YR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PR004
	!
	! Abstract:HELP
	!	.p
	!	The ^*Deductions Register\* report option
	!	lists all non tax payroll
	!	deductions which were made for that payroll.
	!	.p
	!	The report is listed in deduction code order. The report can be
	!	printed with or without a page break after each deduction code.
	!	.p
	!	It is recommended that the final version for each
	!	payroll folder be filed permanently.
	!	.p
	!	The fields include:
	!	.b
	!	.lm +15
	!	.list 0,"o"
	!	.le
	!	Transaction Type
	!	.le
	!	Code
	!	.le
	!	Description of Deduction
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Payroll End Date
	!	.le
	!	Current Amount of Deduction (per employee)
	!	.le
	!	Year-to-Date Amount of Deduction (per employee)
	!	.els
	!
	! Index:
	!	.x Deductions Register>Report
	!	.x Report>Deductions Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_DED_YR/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_DED_YR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_DED_YR.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax table.
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	08/21/90 - J. Shad Rydalch
	!		Copied from PR_RPRT_TRN_DED and modified so that
	!		more than one folder could be printed at a time.
	!
	!	11/06/90 - Kevin Handy
	!		Modified error trapping so that it will not jump
	!		over 330 when an error occurs on 320, also
	!		over 360 when an error occurs on 330.
	!		Added code to open temporary file (don't know
	!		how it could have worked without it).
	!
	!	11/08/90 - Val Allen
	!		Modified someone's efforts at modification
	!		for dates (start to ending) etc.
	!
	!	11/14/90 - Kevin Handy
	!		More modifications in an attempt to make this
	!		pig work.  It was still bombing out.  Modifications
	!		include moving opens out of loop, closing TRN file
	!		before starting next loop, fixing error trapping, ...
	!
	!	11/20/90 - Kevin Handy
	!		Fixed several bugs that caused it to skip over
	!		several deductions that should have been included.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/28/91 - Kevin Handy
	!		Added dimension for DATA_FILE$() so that it could
	!		handle more than 10 files.
	!
	!	05/06/91 - Kevin Handy
	!		Fixed bug that did not allow Tax to be a D type.
	!		Created variable TAX_TYPE_TABLE$ unstead of hardcoding
	!		table into various INSTR calls.
	!
	!	05/17/91 - Kevin Handy
	!		Ripped out YTD column, since it didn't work, doesn't
	!		make sense in this context, and was too much work
	!		to try to make what was there work intelligently.
	!
	!	03/24/92 - Kevin Handy
	!		Added column "Account" that is pulled from the
	!		employees "User Defined" field.
	!
	!	03/25/92 - Kevin Handy
	!		Added "Wildcard" option to be able to choose the
	!		deductions to show up on the-report, instead of
	!		getting all of them.
	!
	!	07/07/92 - Kevin Handy
	!		Added From/To Location fields.
	!
	!	09/21/92 - Kevin Handy
	!		Modified to use record for PR_TEMP file.
	!
	!	10/25/92 - Kevin Handy
	!		Increased dim for data files from 100 to 200.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/10/94 - Kevin Handy
	!		Increased dimension for DATA_FILE$ from 200 to 1000.
	!
	!	02/11/94 - Kevin Handy
	!		Added wildcard for employee number.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH Code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	11/04/97 - Kevin Handy
	!		Fix handling of from/to item.
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'F' final deduction code
	!
	!	08/06/98 - Kevin Handy
	!		Increase number of journal files from 1000 to 2000.
	!		(DWI)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/02/99 - Kevin Handy
	!		Allow to summarize by employee number
	!
	!	01/20/2000 - Kevin Handy
	!		Don't put out a date when summarizing
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	03/10/2004 - Kevin Handy
	!		Add sort options for BY NAME(NA) and BY ALPHA(AL)
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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	RECORD PR_TEMP_CDD
		STRING	SORTS = 20%
		STRING	DTYPE = 1%
		STRING	CODE = 2%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		STRING	PR_END_DATE = 8%
		REAL	AMOUNT
		WORD	UPDATE_FLAG
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	DIM DATA_FILE$(2000%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel for the TEMP file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	%PAGE

 Init:
	TAX_TYPE_TABLE$ = "!FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01)Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field enters the
	!	starting date for which the report is to print.
	!	.p
	!	This field requires an entry. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Start Payroll Date>Deductions Register
	!	.x Deductions Register>Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02)Ending Payroll Date\*
	!	.p
	!	The ^*Ending Payroll Date\* field enters the
	!	ending date for which the report is to print.
	!	.p
	!	This field requires an entry. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Ending Payroll Date>Deductions Register
	!	.x Deductions Register>Ending Payroll Date
	!
	!--

	PAGE_IT$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Page After Each Deduction\*
	!	.p
	!	The ^*Page After Each Deduction\* field causes a page
	!	break to occur after printing each deduction.
	!	.p
	!	To cause the page break to occur, a ^*Y\* must be entered, Otherwise enter
	!	a ^*N\*.
	!
	! Index:
	!	.x Page After>Deductions Register
	!	.x Deductions Register>Page After
	!
	!--

	WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	Used to select (using wildcards) the deduction types
	!	to appear.
	!
	! Index:
	!	.x Wildcard>Deductions Register
	!	.x Deductions Register>Wildcard
	!
	! Datatype:TEXT
	! Size:15
	!--

	WILDEMP$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(04) Wildcard Employee\*
	!	.p
	!	Used to select (using wildcards) the employees
	!	to appear.
	!
	! Index:
	!	.x Wildcard>Deductions Register
	!	.x Deductions Register>Wildcard
	!
	! Datatype:TEXT
	! Size:15
	!--

	FROMLOC$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) From Item\*
	!	.b
	!	From Location.
	!
	! Index:
	!
	!--

	TOLOC$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) To Item\*
	!	.b
	!	To Location.
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Sort By\*
	!	.LM +5
	!	.B
	!	The ^*Sort By\* field
	!	enters a code to designate the manner in which the report is to be
	!	sorted.  Valid codes are:
	!	.b
	!	.table 5,25
	!	.tE
	!	Code Description
	!	.te
	!	.te
	!	---- -----------
	!	.te
	!	DC   Deduction Code
	!	.te
	!	LO   Location
	!	.te
	!	AL   Alpha Sort
	!	.te
	!	NA   Employee Name
	!	.end table
	!	.LM -5
	!
	! Index:
	!
	!--

	SUMMARIZE$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Summarize Employees\*
	!	.LM +5
	!	Determines if the report should summarize entries for each
	!	employee.
	!	.LM -5
	!
	! Index:
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Employee Master File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		CONTINUE 17070 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Ernded Definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

340	!
	! Employee STD Ernded definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

360	!
	! Temporary file
	!
	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP::SORTS, &
				PR_TEMP::DTYPE, &
				PR_TEMP::CODE, &
				PR_TEMP::EMPNUM, &
				PR_TEMP::PR_END_DATE &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	%PAGE

	!*******************************************************************
	! Loop through all folders
	!*******************************************************************

	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		YYYY$ = LEFT(BATCH_NO$, 4%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

380		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			CONTINUE 390 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		PR_TMP_DED.CH% = PR_TRN_DED.CH%
		USE_HISTORY% = 0%

		GOTO 400

390		!
		! Open pay history folder if journal not there
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			CONTINUE 430 IF ERR = 5%
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

		PR_TMP_DED.CH% = PR_HIS_DED.CH%
		USE_HISTORY% = -1%

400		!
		! Create work file
		!
		WHEN ERROR IN
			RESET #PR_TMP_DED.CH%, KEY #0%
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

 !		CALL ENTR_3MESSAGE(SCOPE, "Creating work file by Deduction type.  Working. . .", 1%)

		!
		! Set up to trap interrupt
		!
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG% = 0%

410		WHEN ERROR IN
			GET #PR_TMP_DED.CH%, REGARDLESS
		USE
			CONTINUE 430 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		!
		! Set journal map equal to history map if use history is set
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_DED = PR_HIS_DED
		END IF

		!
		! Skip over unwanted employees
		!
		IF (WILDEMP$ <> "")
		THEN
			IF (COMP_STRING(PR_TRN_DED::EMPNUM, WILDEMP$) = 0%)
			THEN
				GOTO 410
			END IF
		END IF

		!
		! Handle any special junk in RRR_FLAG%
		!
		SELECT RRR_FLAG%

		!
		! Repaint screen
		!
		CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
			SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Help
		!
		CASE SMG$K_TRM_HELP
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
			CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
				SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Interupt
		!
		CASE SMG$K_TRM_F6, SMG$K_TRM_F20
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			UTL_REPORTX::STAT = -1
			GOTO ExitProgram

		END SELECT

		RRR_FLAG% = 0%

420		!
		! Test to see if deduction and not tax
		!
		GOTO 410 UNLESS ((PR_TRN_DED::DTYPE = "D") OR &
			(PR_TRN_DED::DTYPE = "C") OR &
			(PR_TRN_DED::DTYPE = "F")) AND &
			(INSTR(1%,TAX_TYPE_TABLE$, PR_TRN_DED::CODE) = 0%)

		IF (WILDCARD$ = "") OR &
			(COMP_STRING(PR_TRN_DED::CODE, WILDCARD$) <> 0%)
		THEN
			GOSUB GetMaster

			!
			! Fill in Sortby field
			!
			SELECT SORTBY$
			CASE "LO"
				PR_TEMP::SORTS	= PR_EMP_MASTER::LOCATION
			CASE "NA"
				PR_TEMP::SORTS	= PR_TRN_DED::CODE + &
					PR_EMP_MASTER::EMPNAME
			CASE "AL"
				PR_TEMP::SORTS	= PR_TRN_DED::CODE + &
					PR_EMP_MASTER::SORT
			CASE ELSE
				PR_TEMP::SORTS	= PR_TRN_DED::CODE
			END SELECT

			IF (PR_TEMP::SORTS >= FROMLOC$) AND &
				((TOLOC$ = "") OR (TRM$(PR_TEMP::SORTS) <= TOLOC$))
			THEN
				PR_TEMP::DTYPE	= PR_TRN_DED::DTYPE
				PR_TEMP::CODE	= PR_TRN_DED::CODE
				PR_TEMP::EMPNUM	= PR_TRN_DED::EMPNUM
				PR_TEMP::EMPNAME= PR_EMP_MASTER::EMPNAME
				PR_TEMP::PR_END_DATE = PR_TRN_DED::PR_END_DATE
				PR_TEMP::AMOUNT	= PR_TRN_DED::AMOUNT
				PR_TEMP::UPDATE_FLAG = PR_TRN_DED::UPDATE_FLAG

				WHEN ERROR IN
					PUT #PR_TEMP.CH%
				USE
					FILENAME$ = "PR_TEMP"
					CONTINUE HelpError
				END WHEN
			END IF
		END IF

		GOTO 410

430		CLOSE PR_TMP_DED.CH%

	NEXT PR_LOOP%

	%PAGE

	!*******************************************************************
	! Now that we have generated the temp file, print it out.
	!*******************************************************************

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Deduction Report"
	TITLE$(2%) = "For Payroll Folders Dated from: " &
		+ PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		"  to: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = "Sorted by Deduction"
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Typ Code CodeDescription               EmpNum     " + &
		"EmpName                  Date         Current      Account"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EType:001,$Code:007,$CodeDescr:036,$EmpNum:049," + &
		"$EmpName:072,DPrDate:081,VCurrent:095,$Account:120"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	TEST_EMPNUM$ = ""

	WHEN ERROR IN
		RESET #PR_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TEMP"
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

	IF (SORTBY$ = "LO") AND (THISLOC$ <> PR_TEMP::SORTS)
	THEN
		GOSUB EmployeeTotal IF TEST_EMPNUM$ <> ""
		GOSUB CodeTotal
		GOSUB LocationTotal
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 3000%)
	END IF

	GOTO 17060 IF TEST_CODE$ = PR_TEMP::DTYPE + PR_TEMP::CODE

	IF TEST_CODE$ <> ""
	THEN
		GOSUB EmployeeTotal IF TEST_EMPNUM$ <> ""

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB CodeTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

17040	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, &
			KEY #0% EQ PR_TEMP::DTYPE + PR_TEMP::CODE, &
			REGARDLESS
	USE
		PR_ERNDED_DEF::ETYPE	= PR_TEMP::DTYPE
		PR_ERNDED_DEF::CODE	= PR_TEMP::CODE
		PR_ERNDED_DEF::DESCR	= "?????????????????????????????????????"

		CONTINUE 17050 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	TEST_EMPNUM$ = ""

17050	TEXT$ = PR_ERNDED_DEF::ETYPE + "    " + &
		PR_ERNDED_DEF::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR + SPACE$(27%), 27%) + "  "

17060	IF SUMMARIZE$ = "Y"
	THEN
		GOTO 17100 IF TEST_EMPNUM$ = PR_TEMP::EMPNUM
	ELSE
		GOTO 17100 &
			IF TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE
	END IF

17070	!
	! Switch Employees
	!
	IF EMP_TOTAL <> 0.0
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	!
	! Look up user defined account
	!
	TEST_ACCOUNT$ = ""

	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_TEMP::EMPNUM + PR_TEMP::DTYPE + &
			PR_TEMP::CODE, &
			REGARDLESS
	USE
		CONTINUE 17075
	END WHEN

	TEST_ACCOUNT$ = PR_EMP_STD_ERNDED::USERDEF

17075	IF SUMMARIZE$ = "Y"
	THEN
		PR_TEMP::PR_END_DATE = ""
	END IF

	TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
		PR_TEMP::EMPNUM + " " + &
		LEFT(PR_TEMP::EMPNAME, 22%) + " " + &
		PRNT_DATE(PR_TEMP::PR_END_DATE, 6%)

17100	!
	! Add employee deduction total
	!
	TEST_CODE$ = PR_TEMP::DTYPE + PR_TEMP::CODE
	IF SUMMARIZE$ = "Y"
	THEN
		TEST_EMPNUM$ = PR_TEMP::EMPNUM
	ELSE
		TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE
	END IF

	EMP_TOTAL = EMP_TOTAL + PR_TEMP::AMOUNT
	LOC_TOTAL = LOC_TOTAL + PR_TEMP::AMOUNT

	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	IF TEST_CODE$ <> ""
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB CodeTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF SORTBY$ = "LO"
	THEN
		GOSUB LocationTotal
	END IF

	TEXT$ = SPACE$(39%) + "Grand Total"
	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(GRAND_TOTAL, "  #,###,###.##     ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
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

 EmployeeTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************

18110	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(EMP_TOTAL, "  #,###,###.##     ") + TEST_ACCOUNT$

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CODE_TOTAL = CODE_TOTAL + EMP_TOTAL

	EMP_TOTAL = 0.0

	TEXT$ = ""
	TEST_ACCOUNT$ = ""

	RETURN

	%PAGE

 CodeTotal:
	!******************************************************************
	! Print Code total
	!******************************************************************

	IF (TEST_CODE$ <> "") OR (CODE_TOTAL <> 0.0)
	THEN
		TEXT1$ = SPACE$(39%) + "Total for Deduction " + &
			RIGHT(TEST_CODE$, 2%)
		TEXT1$ = LEFT(TEXT1$ + SPACE$(81%), 81%) + &
			FORMAT$(CODE_TOTAL, "  #,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GRAND_TOTAL = GRAND_TOTAL + CODE_TOTAL

	CODE_TOTAL = 0.0

	IF PAGE_IT$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	END IF

	TEXT$ = ""

	RETURN

	%PAGE

	!*******************************************************************
	! Page for a new location
	!*******************************************************************

 LocationTotal:
	IF THISLOC$ <> ""
	THEN

		TEXT1$ = "      Total for location " + THISLOC$
		TEXT1$ = LEFT(TEXT1$ + SPACE$(81%), 81%) + &
			FORMAT$(LOC_TOTAL, "  #,###,###.##     ")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT1$, 0%)

		TITLE$(3%) = "For Location " + LEFT(PR_TEMP::SORTS, 4%)
	END IF

	!
	! SET UP FOR NEXT PASS
	!
	THISLOC$ = LEFT(PR_TEMP::SORTS, 4%)
	TEST_CODE$ = ""
	TITLE$(3%) = "For Location " + THISLOC$
	LOC_TOTAL = 0.0

	RETURN

	%Page

 GetMaster:
	!*******************************************************************
	! Find employee master file record
	!*******************************************************************

18300	GOTO 18390 IF PR_EMP_MASTER::EMPNUM = PR_TRN_DED::EMPNUM

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_DED::EMPNUM, &
			REGARDLESS
	USE
		PR_EMP_MASTER::EMPNAME = "????????????????????????????????????"

		CONTINUE 18390
	END WHEN

18390	RETURN

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
