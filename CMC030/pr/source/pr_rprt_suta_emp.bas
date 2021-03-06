1	%TITLE "Payroll SUTA Detail Report"
	%SBTTL "PR_RPRT_SUTA_EMP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988, 1989 BY
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
	! ID:PR100
	!
	! Abstract:HELP
	!	.p
	!	The ^*SUTA Detail Report\* option
	!	prints quarterly
	!	State Unemployment Tax Report(s) for each State to which the
	!	employer is liable. Included in this report are the following fields:
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
	!	Year to Date Wages
	!	.le
	!	Quarter to Date Wages
	!	.le
	!	Quarter to Date Excess
	!	.le
	!	Quarter to Date SUTA Wages
	!	.le
	!	Quarter to Date SUTA Withholding
	!	.le
	!	Year to Date Weeks Worked
	!	.le
	!	Quarter to Date Weeks Worked
	!	.els
	!
	! Index:
	!	.x SUTA Detail Report
	!	.x Report>SUTA>Detail
	!	.x Report>State Unemployment Taxes>Detail
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SUTA_EMP/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_SUTA_EMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SUTA_EMP.OBJ;*
	!
	! Author:
	!
	!	07/10/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/27/90 - Kevin Handy
	!		Completely rewrote section that calculates SU wages.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filenames in the error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF.CH and PR_REG_ERNDED.CH
	!		from PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED file which is no longer
	!		used in this program.
	!
	!	07/14/91 - Kevin Handy
	!		Removed duplicate line 1.
	!
	!	11/05/91 - Kevin Handy
	!		Modified so that SUI_MAX of 0.0 means unlimited,
	!		not a limit of 0.0
	!
	!	03/24/92 - Kevin Handy
	!		Modified to use new "SUTA Account" number instead
	!		of "State ID Number".
	!
	!	10/27/92 - Kevin Handy
	!		Split up SORTBY selects so didn't generate title
	!		every time pulled in an employee.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/27/95 - Kevin Handy
	!		Lose check for 1099 flag, so we don't have any magic
	!		happening.
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
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
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	04/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/17/2000 - Kevin Handy
	!		Fix DP sorting problem
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

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"
	MAP	(PR_EMP_DATES)		PR_EMP_DATES_CDD	PR_EMP_DATES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)		PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	RECORD PR_TEMP_CDD
		STRING	STCODE = 2%
		STRING	SORTKEY = 20%
		STRING	EMPNUM = 10%
		REAL	WAGES(3%)
		REAL	TAXES(3%)
		WORD	WKWRK(3%)
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION PR_FUNC_READTAXES
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension arrays and tables
	!
	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	%PAGE

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
		IF THIS_DATE$ < FROM_DATE$
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

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = LEFT(UTL_REPORTX::OPTDEF(0%), 2%)
	!++
	! Abstract:FLD01
	!	^*(01) From State\*
	!	.p
	!	The ^*From State\* setting causes the printing
	!	to begin with a particular state.
	!	.p
	!	A blank field will cause the report to start with the first state in the file.
	!	.p
	!	The field will accommodate a two (2) digit postal code.
	!
	! Index:
	!	.x From State>SUTA Detail Report
	!	.x SUTA Detail Report>From State
	!
	!--

	TO_ITEM$ = LEFT$(UTL_REPORTX::OPTDEF(1%), 2%)
	!++
	! Abstract:FLD02
	!	^*(02) To State\*
	!	.p
	!	The ^*To State\* setting causes the printing
	!	to end with a particular state.
	!	.p
	!	A blank field will cause the report to end with the last state in the file.
	!	.p
	!	The field will accommodate a two (2) digit postal code.
	!
	! Index:
	!	.x To State>SUTA Detail Report
	!	.x SUTA Detail Report>To State
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,NA,AL,DP,LO)\*
	!	.p
	!	The ^*Sort\* field indicates how the
	!	report will be sorted.
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
	!	AL = Alphabetical (last name first)
	!	.le
	!	LO = Location
	!	.le
	!	DP = Department
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort>SUTA Detail Report
	!	.x SUTA Detail Report>Sort
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) Year\*
	!	.p
	!	The ^*Year\* field enters the year for which this report is
	!	to print.
	!	.p
	!	The format for entry is YYYY and the field must be entered.
	!
	! Index:
	!	.x Year>SUTA Detail Report
	!	.x SUTA Detail Report>Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))
	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the accounting quarter for which
	!	this report will print.
	!	.p
	!	This field requires an entry and will only accommodate a one (1) digit number.
	!
	! Index:
	!	.x Quarter>SUTA Detail Report
	!	.x SUTA Detail Report>Quarter
	!
	!--

	MONTH_1$ = YYYY$ + RIGHT(NUM1$((QTR% * 3%) +  98%), 2%) + "12"
	MONTH_2$ = YYYY$ + RIGHT(NUM1$((QTR% * 3%) +  99%), 2%) + "12"
	MONTH_3$ = YYYY$ + RIGHT(NUM1$((QTR% * 3%) + 100%), 2%) + "12"

	!
	! Set sort key
	!
	SELECT SORTBY$

	CASE "NU"
		SB_TEXT$ = "By Employee Number"

	CASE "NA"
		SB_TEXT$ = "By Employee Name"

	CASE "SN"
		SB_TEXT$ = "By Social Security Number"

	CASE "DP"
		SB_TEXT$ = "By Department Number"

	CASE ELSE
		SB_TEXT$ = "By Alpha Key"

	END SELECT

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Open Employee Master File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_"  + YYYY$
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Tax Package file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

360	!
	! Open Employee Date History file
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.OPN"

370	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP::STCODE, &
				PR_TEMP::SORTKEY, &
				PR_TEMP::EMPNUM &
			)	DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE, &
			TEMPORARY
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file by state.  Reading register file", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

410	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE 490 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Set sort key
	!
	SELECT SORTBY$

	CASE "NU"
		SORTKEY$ = PR_EMP_MASTER::EMPNUM

	CASE "NA"
		SORTKEY$ = PR_EMP_MASTER::EMPNAME

	CASE "SN"
		SORTKEY$ = PR_EMP_MASTER::SSN

	CASE "DP"
		SORTKEY$ = PR_EMP_MASTER::LOCATION + &
			PR_EMP_MASTER::DEPT + &
			PR_EMP_MASTER::WORK_CENTER

	CASE ELSE
		SORTKEY$ = PR_EMP_MASTER::SORT

	END SELECT

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

	!
	! Routine to write record
	!
	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

460	FOR LOOP% = 1% TO PR_TAXES%

		IF PR_TAXES(LOOP%)::TTYPE = "SU"
		THEN
			PR_TEMP::STCODE = PR_TAXES(LOOP%)::CODE
			PR_TEMP::SORTKEY = SORTKEY$
			PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM
			PR_TEMP::WAGES(I%) = PR_TAXES(LOOP%)::REPORTABLE(I%) &
				FOR I% = 0% TO 3%
			PR_TEMP::WKWRK(I%) = PR_TAXES(LOOP%)::WKWRK(I%) &
				FOR I% = 0% TO 3%

			PUT #PR_TEMP.CH%
		END IF

	NEXT LOOP%

	GOTO 410

490	!
	! Write last record and print report
	!

	%Page

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "State Unemployment Tax Act Report - " + &
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of  " + YYYY$
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "EmpNum     EmpName                    SSN         " + &
		"     YTD Wages    QTD Wages   QTD Excess  QTD SUTA Wg  " + &
		"QTD SUTA WH  YTD WW  QTD WW"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:037,$SSN:049,VYTDWages:064," + &
		"VQTDWages:077,VQTDExcess:090,VQTDSUTAWg:103," + &
		"VQTDSUTAWithHeld:116,VYTDWW:124,VQTDWW:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_LOC$ = ""

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TEMP.CH%, KEY #0%
		ELSE
			FIND #PR_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
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

	GOTO ExitTotal IF (PR_TEMP::STCODE > TO_ITEM$) AND TO_ITEM$ <> ""

	IF TEST_STCODE$ <> PR_TEMP::STCODE
	THEN
		GOSUB STProfile
		STATE_TOTAL(I%) = 0.0 FOR I% = 1% TO 7%
		F_MONTH_1%, F_MONTH_2%, F_MONTH_3% = 0%
		M_MONTH_1%, M_MONTH_2%, M_MONTH_3% = 0%
		LISTED_TOTAL% = 0%
	END IF

	TEST_STCODE$ = PR_TEMP::STCODE

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 7%

	EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + PR_TEMP::WAGES(I%), 2%) &
		FOR I% = 0% TO QTR% - 1%

	EMP_TOTAL(2%) = FUNC_ROUND(PR_TEMP::WAGES(QTR% - 1%), 2%)

	EMP_TOTAL(5%) = FUNC_ROUND(PR_TEMP::TAXES(QTR% - 1%), 2%)

	EMP_TOTAL(6%) = EMP_TOTAL(6%) + PR_TEMP::WKWRK(I%) &
		FOR I% = 0% TO QTR% - 1%

	EMP_TOTAL(7%) = PR_TEMP::WKWRK(QTR% - 1%)

	PR_EMP_MASTER::EMPNUM	= PR_TEMP::EMPNUM
	PR_EMP_MASTER::EMPNAME	= "???????????????????????????????"
	PR_EMP_MASTER::SSN	= "???-??-????"
	PR_EMP_MASTER::HIREDAY	= "00000000"

17060	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TEMP::EMPNUM, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Print department totals?
	!
	IF SORTBY$ = "DP" AND &
		((THIS_LOC$ <> PR_EMP_MASTER::LOCATION + &
		PR_EMP_MASTER::DEPT + &
		PR_EMP_MASTER::WORK_CENTER) OR (THIS_LOC$ == ""))
	THEN
		GOSUB LocationTotal
		GOSUB LocationLoad
	END IF

17100	!
	! Print total for employee
	!
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		IF (SUI_MAX = 0.0)
		THEN
			EMP_TOTAL(3%) = 0.0
		ELSE
			EMP_TOTAL(3%) = EMP_TOTAL(1%) - SUI_MAX
			EMP_TOTAL(3%) = 0.0 IF EMP_TOTAL(3%) < 0.0
			EMP_TOTAL(3%) = EMP_TOTAL(2%) &
				IF EMP_TOTAL(3%) >= EMP_TOTAL(2%)
		END IF
		EMP_TOTAL(4%) = EMP_TOTAL(2%) - EMP_TOTAL(3%)

		GOSUB Check_Emp_Dates

		STATE_TOTAL(LOOP%) = STATE_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 7%

		SUB_TOTAL(LOOP%) = SUB_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 7%

		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 26%) + " " + &
			LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 11%), 11%) + "   " + &
			FORMAT$(EMP_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(3%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(4%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(EMP_TOTAL(6%), "###,### ") + &
			FORMAT$(EMP_TOTAL(7%), "###,###")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		LISTED_TOTAL% = LISTED_TOTAL% + 1%

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

 LocationTotal:
	!*******************************************************************
	! Handle end of location
	!*******************************************************************

	IF NOT (THIS_LOC$ == "")
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEMP$ = "      Location Total" + SPACE$(45%)
		TEXT$ = LEFT(TEMP$, 52%) + &
			FORMAT$(SUB_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(3%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(4%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(SUB_TOTAL(6%), "###,### ") + &
			FORMAT$(SUB_TOTAL(7%), "###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	SUI_EMPR = SUB_TOTAL(4%) * SUI_PCT
	TEXT$ = "           SUI Tax Liability   = QTD SUI Wages X " + &
		"SUI Percent = " + &
		FORMAT$(SUB_TOTAL(4%), "##,###,###.##") + &
		" X " + &
		FORMAT$(SUI_PCT, ".#####") + &
		" = " + &
		FORMAT$(SUI_EMPR, "##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           SUI Tax Withholding =" + SPACE$(56%) + &
		FORMAT$(SUB_TOTAL(5%), "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		SPACE$(88%) + "=============", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           SUI Total           =" + SPACE$(56%) + &
		FORMAT$(SUB_TOTAL(5%) + SUI_EMPR, "##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	TITLE$(3%) = "Loc: " + PR_EMP_MASTER::LOCATION + &
		" Dep: " + PR_EMP_MASTER::DEPT + &
		" W/C: " + PR_EMP_MASTER::WORK_CENTER

	IF NOT (THIS_LOC$ == "")
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	END IF

	RETURN

 LocationLoad:

	THIS_LOC$ = PR_EMP_MASTER::LOCATION + &
		PR_EMP_MASTER::DEPT + &
		PR_EMP_MASTER::WORK_CENTER

	SUB_TOTAL(I%) = 0.0 FOR I% = 1% TO 6%

	RETURN

 ExitTotal:
	!
	! Handle end of report
	!
	IF SORTBY$ = "DP"
	THEN
		GOSUB LocationTotal
		GOSUB LocationLoad
	END IF


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEMP$ = "           State Total" + SPACE$(52%)
	TEXT$ = LEFT(TEMP$, 52%) + &
		FORMAT$(STATE_TOTAL(1%), "#,###,###.## ") + &
		FORMAT$(STATE_TOTAL(2%), "#,###,###.## ") + &
		FORMAT$(STATE_TOTAL(3%), "#,###,###.## ") + &
		FORMAT$(STATE_TOTAL(4%), "#,###,###.## ") + &
		FORMAT$(STATE_TOTAL(5%), "#,###,###.## ") + &
		FORMAT$(STATE_TOTAL(6%), "###,### ") + &
		FORMAT$(STATE_TOTAL(7%), "###,###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           Total Number of Employees Listed = " + &
		FORMAT$(LISTED_TOTAL%, "###,###,###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	SUI_EMPR = STATE_TOTAL(4%) * SUI_PCT
	TEXT$ = "           SUI Tax Liability   = QTD SUI Wages X " + &
		"SUI Percent = " + &
		FORMAT$(STATE_TOTAL(4%), "##,###,###.##") + &
		" X " + &
		FORMAT$(SUI_PCT, ".#####") + &
		" = " + &
		FORMAT$(SUI_EMPR, "##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           SUI Tax Withholding =" + SPACE$(56%) + &
		FORMAT$(STATE_TOTAL(5%), "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		SPACE$(88%) + "=============", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           SUI Total           =" + SPACE$(56%) + &
		FORMAT$(STATE_TOTAL(5%) + SUI_EMPR, "##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = "           ACTIVE EMPLOYEES ON THE 12TH DAY OF EACH MONTH"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           Month 1:  Males = " + &
		FORMAT$(M_MONTH_1%, "#,###,###") + &
		"   Females = " + &
		FORMAT$(F_MONTH_1%, "#,###,###") + &
		"   Total = " + &
		FORMAT$(M_MONTH_1% + F_MONTH_1%, "##,###,###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           Month 2:  Males = " + &
		FORMAT$(M_MONTH_2%, "#,###,###") + &
		"   Females = " + &
		FORMAT$(F_MONTH_2%, "#,###,###") + &
		"   Total = " + &
		FORMAT$(M_MONTH_2% + F_MONTH_2%, "##,###,###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           Month 3:  Males = " + &
		FORMAT$(M_MONTH_3%, "#,###,###") + &
		"   Females = " + &
		FORMAT$(F_MONTH_3%, "#,###,###") + &
		"   Total = " + &
		FORMAT$(M_MONTH_3% + F_MONTH_3%, "##,###,###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + STATE_TOTAL(I%) FOR I% = 1% TO 7%

	TEMP$ = "           Grand Total" + SPACE$(52%)
	TEXT$ = LEFT(TEMP$, 52%) + &
		FORMAT$(GRAND_TOTAL(1%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "#,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "###,### ") + &
		FORMAT$(GRAND_TOTAL(7%), "###,###")

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

 STProfile:
	!******************************************************************
	! Look up state profile record
	!******************************************************************

	TEMP_SUI_PCT = SUI_PCT

	CODE$ = PR_TEMP::STCODE
	REPNO$ = "??????????????"
	SUI_PCT, SUI_MAX = 0.0

18100	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + CODE$, REGARDLESS
	USE
		CONTINUE 18190 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	REPNO$ = PR_TAX_PROFILE_S::SUTANO
	SUI_PCT = PR_TAX_PROFILE_S::SUI_PCT / 100.0
	SUI_MAX = PR_TAX_PROFILE_S::SUI_MAX

18190	TITLE$(5%) = "State: " + CODE$ + &
		" Employer Tax # " + REPNO$ + "  SUTA Percent: " + &
		NUM1$(SUI_PCT * 100.0) + "%   SUTA Limit: " + &
		NUM1$(SUI_MAX)

	IF TEST_STCODE$ <> ""
	THEN
		IF SORTBY$ = "DP"
		THEN
			GOSUB LocationTotal
			GOSUB LocationLoad
		END IF


		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEMP$ = "           State Total" + SPACE$(52%)
		TEXT$ = LEFT(TEMP$, 52%) + &
			FORMAT$(STATE_TOTAL(1%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(2%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(3%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(4%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(5%), "#,###,###.## ") + &
			FORMAT$(STATE_TOTAL(6%), "###,### ") + &
			FORMAT$(STATE_TOTAL(7%), "###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "           Total Number of Employees Listed = " + &
			FORMAT$(LISTED_TOTAL%, "###,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		SUI_EMPR = STATE_TOTAL(4%) * TEMP_SUI_PCT
		TEXT$ = "           SUI Tax Liability   = QTD SUI Wages X " + &
			"SUI Percent = " + &
			FORMAT$(STATE_TOTAL(4%), "##,###,###.##") + &
			" X " + &
			FORMAT$(TEMP_SUI_PCT, ".#####") + &
			" = " + &
			FORMAT$(SUI_EMPR, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT


		TEXT$ = "           SUI Tax Withholding =" + SPACE$(56%) + &
			FORMAT$(STATE_TOTAL(5%), "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			SPACE$(88%) + "=============", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT


		TEXT$ = "           SUI Total           =" + SPACE$(56%) + &
			FORMAT$(STATE_TOTAL(5%) + SUI_EMPR, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "           Month 1:  Males = " + &
			FORMAT$(M_MONTH_1%, "#,###,###") + &
			"   Females = " + &
			FORMAT$(F_MONTH_1%, "#,###,###") + &
			"   Total = " + &
			FORMAT$(M_MONTH_1% + F_MONTH_1%, "##,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "           Month 2:  Males = " + &
			FORMAT$(M_MONTH_2%, "#,###,###") + &
			"   Females = " + &
			FORMAT$(F_MONTH_2%, "#,###,###") + &
			"   Total = " + &
			FORMAT$(M_MONTH_2% + F_MONTH_2%, "##,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "           Month 3:  Males = " + &
			FORMAT$(M_MONTH_3%, "#,###,###") + &
			"   Females = " + &
			FORMAT$(F_MONTH_3%, "#,###,###") + &
			"   Total = " + &
			FORMAT$(M_MONTH_3% + F_MONTH_3%, "##,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + STATE_TOTAL(I%) &
			FOR I% = 1% TO 7%
	END IF

	RETURN

 Emp_Dates:
18192	!*******************************************************************
	! Emp_Dates - Check Employee dates
	!
	! Returns 0 (in range)
	!	-1 (before range)
	!	1 (after range)
	!	2 (record not found)
	!*******************************************************************

		EMPDATES%, THE_FLAG% = 2	! Assume out of range

		!
		! Scan through employee dates for given code
		!
		WHEN ERROR IN
			GET #PR_EMP_DATES.CH%, KEY #0% GE EMPNUM$ + CODE$, &
				REGARDLESS
		USE
			CONTINUE 18194 IF ERR = 155%
			FILENAME$ = "PR_EMP_DATES"
			CONTINUE HelpError
		END WHEN

		WHILE (PR_EMP_DATES::EMPLOYEE = EMPNUM$) AND &
			(PR_EMP_DATES::DATECD = CODE$)

			EMPDATES%, THE_FLAG% = FNTESTDATE%( &
				PR_EMP_DATES::DATEBEGIN, &
				PR_EMP_DATES::DATEEND, THE_DATE$)

			GOTO 18194 IF THE_FLAG% = 0%

			WHEN ERROR IN
				GET #PR_EMP_DATES.CH%, REGARDLESS
			USE
				CONTINUE 18194 IF ERR = 11%
				FILENAME$ = "PR_EMP_DATES"
				CONTINUE HelpError
			END WHEN
		NEXT

18194	RETURN

 Check_Emp_Dates:
18200	!
	! Check to see if the employee was active during the time in
	!	question.
	!
	!	Check the start/termination dates in the employee
	!		record to see if they are terminated there.
	!	Check the employee dates file to see if they are shown
	!		as terminated there.
	!
	!	Codes defined as:
	!		0 - Active
	!		? - Inactive
	!
	! Check the employee record for being active for each month of
	!	the quarter.
	!
	EMPNUM$ = PR_EMP_MASTER::EMPNUM
	CODE$ = "AC"

	SELECT FNTESTDATE%(PR_EMP_MASTER::HIREDAY, &
		PR_EMP_MASTER::TERMDAY, MONTH_1$)

	CASE 0%
		M_MONTH_1% = M_MONTH_1% + 1% IF PR_EMP_MASTER::SEX = "M"
		F_MONTH_1% = F_MONTH_1% + 1% IF PR_EMP_MASTER::SEX = "F"
	CASE ELSE
		THE_DATE$ = MONTH_1$
		GOSUB Emp_Dates
		IF EMPDATES% = 0%
		THEN
			M_MONTH_1% = M_MONTH_1% + 1% &
				IF PR_EMP_MASTER::SEX = "M"
			F_MONTH_1% = F_MONTH_1% + 1% &
				IF PR_EMP_MASTER::SEX = "F"
		END IF
	END SELECT

	SELECT FNTESTDATE%(PR_EMP_MASTER::HIREDAY, &
		PR_EMP_MASTER::TERMDAY, MONTH_2$)

	CASE 0%
		M_MONTH_2% = M_MONTH_2% + 1% IF PR_EMP_MASTER::SEX = "M"
		F_MONTH_2% = F_MONTH_2% + 1% IF PR_EMP_MASTER::SEX = "F"

	CASE ELSE
		THE_DATE$ = MONTH_2$
		GOSUB Emp_Dates
		IF EMPDATES% = 0%
		THEN
			M_MONTH_2% = M_MONTH_2% + 1% &
				IF PR_EMP_MASTER::SEX = "M"
			F_MONTH_2% = F_MONTH_2% + 1% &
				IF PR_EMP_MASTER::SEX = "F"
		END IF

	END SELECT

	SELECT FNTESTDATE%(PR_EMP_MASTER::HIREDAY, &
		PR_EMP_MASTER::TERMDAY, MONTH_3$)

	CASE 0%
		M_MONTH_3% = M_MONTH_3% + 1% IF PR_EMP_MASTER::SEX = "M"
		F_MONTH_3% = F_MONTH_3% + 1% IF PR_EMP_MASTER::SEX = "F"
	CASE ELSE
		THE_DATE$ = MONTH_3$
		GOSUB Emp_Dates
		IF EMPDATES% = 0%
		THEN
			M_MONTH_3% = M_MONTH_3% + 1% &
				IF PR_EMP_MASTER::SEX = "M"
			F_MONTH_3% = F_MONTH_3% + 1% &
				IF PR_EMP_MASTER::SEX = "F"
		END IF
	END SELECT

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
