1	%TITLE "Payroll SHORT SUTA Report"
	%SBTTL "PR_RPRT_SUTASHORT"
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
	! ID:PR042
	!
	! Abstract:HELP
	!	.p
	!	The ^*SUTA Report\* option
	!	prints quarterly
	!	State Unemployment Tax Report(s) for each State to which the
	!	employer is liable. This report includes the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Social Security Number
	!	.le
	!	Quarter to Date Wages
	!	.els
	!
	! Index:
	!	.x Report>SUTASHORT
	!	.x Report>State Unemployment Taxes
	!	.x State Unemployment Tax>Report
	!	.x SUTASHORT>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SUTASHORT/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_SUTASHORT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SUTASHORT.OBJ;*
	!
	! Author:
	!
	!	09/19/91 - JEFF BEARD
	!		copied from rprt_suta
	!
	! Modification history:
	!
	!	11/05/91 - Kevin Handy
	!		Modified so that SUI_MAX of 0.0 means unlimited
	!		instead of a limit of 0.0
	!
	!	01/03/92 - Kevin Handy
	!		Removed commented out code.
	!		Modified to print only non-zero employees.
	!		Simplified to only use/calculate one number instead
	!		of several arrays.
	!
	!	03/24/92 - Kevin Handy
	!		Modified to use new "SUTA Account" field instead
	!		of "State ID Number".
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/94 - Kevin Handy
	!		Modified to handle SUI limit.
	!
	!	04/18/94 - Kevin Handy
	!		They changed their minds about 04/12/94 change.
	!
	!	04/19/94 - Kevin Handy
	!		Modifications to handle SUTA taxable different
	!		than state. Pulled much code from PR_RPRT_SUTA.BAS
	!		and replaced what was here before, leaving only the
	!		printing code the same.
	!
	!	04/26/94 - Kevin Handy
	!		Modified to display FEDERAL and STATE id numbers
	!		in addition to the SUTA id number.
	!
	!	05/17/94 - Kevin Handy
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
	!		Remove test for 1099 flag, so we don't have any
	!		magic happening.
	!
	!	09/11/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'F' final deduction code
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
	!		Fix problems when sorting by "DP"
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

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
		WORD	WKWRK(3%)
	END RECORD

	DECLARE PR_TEMP_CDD PR_TEMP_BUFFER(52%)
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

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel number for the TEMP file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	%PAGE

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From State\*
	!	.p
	!	The ^*From State\* enters the state with
	!	which the report will begin printing. A blank
	!	field will cause the report to begin with the first state in the
	!	file.
	!	.p
	!	The field will accommodate a two digit state postal code.
	!
	! Index:
	!	.x From State>SUTA Report
	!	.x SUTA Report>From State
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To State\*
	!	.p
	!	The ^*To State\* field entesr a code for the
	!	state with which the report will end. A blank field will cause the
	!	report to end with the last state in the file.
	!	.p
	!	The field will contain a two digit state postal code.
	!
	! Index:
	!	.x To State>SUTA Report
	!	.x SUTA Report>To State
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

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

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU, SN, NA, DP, AL)\*
	!	.p
	!	The ^*Sort\* field enters a code which will
	!	cause the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	SN = Social Security Number
	!	.le
	!	NA = Name
	!	.le
	!	AL = Alphabetical (last name first)
	!	.le
	!	DP = Department
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field and only the above codes
	!	are valid.
	!
	! Index:
	!	.x Sort>SUTA Report
	!	.x SUTA Report>Sort
	!
	!--


	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Year (YYYY)\*
	!	.p
	!	The ^*Year\* field enters the year for which
	!	this report is to be printed.
	!	.p
	!	This field requires an entry and the format for entry is YYYY.
	!
	! Index:
	!	.x Year>SUTA Report
	!	.x SUTA Report>Year
	!
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report will print.
	!	.p
	!	This field requires an entry. The field will accommodate
	!	a one digit number.
	!
	! Index:
	!	.x Quarter>SUTA Report
	!	.x SUTA Report>Quarter
	!
	!--

	WILDLOC$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Location\*
	!
	! Index:
	!	.x Location>SUTA Report
	!	.x SUTA Report>Location
	!
	! Datatype:TEXT
	! Size:15
	! Required:N
	!--


	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Employee Master File
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
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

335	!
	! Open earnings/deduction file in case we need it
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	FED_IDNO$ = PR_TAX_PROFILE_F::REPNO

342	!
	! Open ERNDED_DEF file
	!
	ADJUST_NEGATIVE$ = ""
	ADJUST_POSITIVE$ = ""

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"

		RESET #PR_ERNDED_DEF.CH%
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

344	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE 350 IF ERR = 11%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	IF (PR_ERNDED_DEF::REPORTABLE_SWH <> PR_ERNDED_DEF::REPORTABLE_SUI)
	THEN
		IF PR_ERNDED_DEF::REPORTABLE_SWH = "N"
		THEN
			ADJUST_NEGATIVE$ = ADJUST_NEGATIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		ELSE
			ADJUST_POSITIVE$ = ADJUST_POSITIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		END IF
	END IF

	GOTO 344

350	!
	! Open Tax Package file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

360	OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
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

400	CALL ENTR_3MESSAGE(scope, &
		"Creating work file by state.  Reading register file", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	!*******************************************************************
	! Loop through file, generating temp file information
	!*******************************************************************

	RRR_FLAG% = 0%

	WHEN ERROR IN
		RESET #PR_REG_TAXES.CH%
	USE
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	THIS_EMP$ = "!!!!!!!!!!!"
	PR_TEMP_BUFFER% = 0%

410	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	GOTO 410 IF PR_REG_TAXES::TTYPE <> "SW"

	GOSUB PutBuffer IF PR_REG_TAXES::EMPNUM <> THIS_EMP$

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
		CALL HELP_3MESSAGE(SCOPE, SCOPE::PRG_COMPANY, &
			SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
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

420	IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM
	THEN
		SORTKEY$ = "????????????"
		PR_EMP_MASTER::SUI_SW = "  "
		PR_EMP_MASTER::EMPNUM = PR_REG_TAXES::EMPNUM

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_REG_TAXES::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 480 IF ERR = 155%
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
	END IF

480	PR_TEMP_BUFFER% = PR_TEMP_BUFFER% + 1%

	IF PR_EMP_MASTER::SUI_SW <> "  "
	THEN
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = PR_EMP_MASTER::SUI_SW
	ELSE
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = PR_REG_TAXES::CODE
	END IF

	PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::SORTKEY	= SORTKEY$
	PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::EMPNUM		= PR_REG_TAXES::EMPNUM
	FOR I% = 0% TO 3%
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(I%) = &
			PR_REG_TAXES::REPORTABLE(I%)
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WKWRK(I%) = &
			PR_REG_TAXES::WKWRK(I%)
	NEXT I%

	GOTO 410

 PutBuffer:
500	!*******************************************************************
	! Output all records buffered
	!*******************************************************************

	IF (ADJUST_POSITIVE$ <> "") OR (ADJUST_NEGATIVE$ <> "")
	THEN
		GOSUB CalculateAdjustment IF PR_TEMP_BUFFER%
	END IF

	FOR I% = 1% TO PR_TEMP_BUFFER%

		PR_TEMP = PR_TEMP_BUFFER(I%)
		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN
	NEXT I%

	THIS_EMP$ = PR_REG_TAXES::EMPNUM + ""
	PR_TEMP_BUFFER% = 0%

	RETURN

	%PAGE

 CalculateAdjustment:
600	!*******************************************************************
	! Calculate any adjustment for this employee
	!*******************************************************************

	ADJUSTMENT(I%) = 0.0 FOR I% = 0% TO 3%
	ADJUSTMENT% = 0%

	!
	! Load up any adjustments
	!
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, KEY #0% EQ THIS_EMP$, REGARDLESS
	USE
		CONTINUE 620
	END WHEN

610	WHILE PR_REG_ERNDED::EMPNUM = THIS_EMP$

		IF PR_REG_ERNDED::ETYPE = "O"
		THEN
			ETYPE$ = "P"
		ELSE
			ETYPE$ = PR_REG_ERNDED::ETYPE
		END IF

		!
		! Positive adjustments
		!
		IF INSTR(1%, ADJUST_POSITIVE$, ETYPE$ + PR_REG_ERNDED::CODE)
		THEN
			IF ETYPE$ = "D" OR ETYPE$ = "F"
			THEN
				XSIGN = -1.0
			ELSE
				XSIGN = 1.0
			END IF

			ADJUSTMENT(I%) = FUNC_ROUND(ADJUSTMENT(I%) + &
				XSIGN * PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
			ADJUSTMENT% = -1%
		END IF

		!
		! Negative adjustments
		!
		IF INSTR(1%, ADJUST_NEGATIVE$, ETYPE$ + PR_REG_ERNDED::CODE)
		THEN
			IF ETYPE$ = "D" OR ETYPE$ = "F"
			THEN
				XSIGN = 1.0
			ELSE
				XSIGN = -1.0
			END IF


			ADJUSTMENT(I%) = FUNC_ROUND(ADJUSTMENT(I%) + &
				XSIGN * PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
			ADJUSTMENT% = -1%
		END IF

		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%, REGARDLESS
		USE
			CONTINUE 620
		END WHEN
	NEXT

620	!
	! Add adjustments into totals
	!
	RETURN IF ADJUSTMENT% = 0%

	FOR QTR1% = 0% TO 3%
		!
		! Calculate rate to spread out with
		!
		TOTAL = 0.0
		TOTAL = TOTAL + PR_TEMP_BUFFER(I%)::WAGES(QTR1%) &
			FOR I% = 1% TO PR_TEMP_BUFFER%

		IF TOTAL = 0.0
		THEN
			FACTOR = 0.0
		ELSE
			FACTOR = ADJUSTMENT(QTR1%) / TOTAL
		END IF

		!
		! Spread it out for all but last state
		!
		FOR I% = 1% TO PR_TEMP_BUFFER% - 1%
			CALC = FUNC_ROUND(PR_TEMP_BUFFER(I%)::WAGES(QTR1%) * &
				FACTOR, 2%)
			PR_TEMP_BUFFER(I%)::WAGES(QTR1%) = &
				PR_TEMP_BUFFER(I%)::WAGES(QTR1%) + &
				CALC
			ADJUSTMENT(QTR1%) = ADJUSTMENT(QTR1%) - CALC
		NEXT I%

		!
		! Slap anything left over in last state
		!
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(QTR1%) = &
			PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(QTR1%) + &
			ADJUSTMENT(QTR1%)

	NEXT QTR1%

	RETURN

	%PAGE

 ReportTitle:
	!
	! Output any remaining data
	!
	GOSUB PutBuffer

	!
	! Set up titles
	!
	TITLE$(1%) = "State Unemployment Tax Act Report - " + &
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Federal id # " + FED_IDNO$
	TITLE$(6%) = "."
	TITLE$(7%) = "."
	!		         1         2         3         4         5
	!		12345678901234567890123456789012345678901234567890
	TITLE$(8%) = "social security number  EmpName                   " + &
		"             QTD Wages"
	TITLE$(9%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$SSN:023,$EmpName:063,VQTDWages:073"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_LOC$ = ""

	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_TEMP.CH%, KEY #0%
	ELSE
		FIND #PR_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
	END IF

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
		STATE_TOTAL = 0.0
	END IF

	TEST_STCODE$ = PR_TEMP::STCODE

	!
	! Calculate SUTA wages for this quarter
	!
	EMP_TOTAL = FUNC_ROUND(PR_TEMP::WAGES(QTR% - 1%), 2%)

	EMP_FULLTOTAL = 0.0
	EMP_FULLTOTAL = FUNC_ROUND(EMP_FULLTOTAL + &
		PR_TEMP::WAGES(I%), 2%) &
		FOR I% = 0% TO QTR% - 1%

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
 !	IF EMP_TOTAL <> 0.0 AND PR_EMP_MASTER::W2_1099 <> "Y"
	IF EMP_TOTAL <> 0.0
	THEN
		STATE_TOTAL = STATE_TOTAL + EMP_TOTAL

		SUB_TOTAL = SUB_TOTAL + EMP_TOTAL

		TEXT$ = LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 11%), 11%) + &
			"             " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 26%) + "         " + &
			FORMAT$(EMP_TOTAL, "##,###,###.## ")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
		TEXT$ = LEFT(TEMP$, 61%) + &
			FORMAT$(SUB_TOTAL, "##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

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

	SUB_TOTAL = 0.0

	RETURN


 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB STProfile

	TEMP$ = "           Grand Total" + SPACE$(61%)
	TEXT$ = LEFT(TEMP$, 61%) + &
		FORMAT$(GRAND_TOTAL, "##,###,###.## ")

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

	CODE$ = PR_TEMP::STCODE
	SUTA_IDNO$ = "??????????????"
	SUI_PCT = 0.0
	SUI_MAX = 0.0

18100	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + CODE$, REGARDLESS
	USE
		CONTINUE 18190 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	SUTA_IDNO$ = PR_TAX_PROFILE_S::SUTANO
	STATE_IDNO$ = PR_TAX_PROFILE_S::REPNO
	SUI_PCT = PR_TAX_PROFILE_S::SUI_PCT/100.
	SUI_MAX = PR_TAX_PROFILE_S::SUI_MAX

18190	TITLE$(5%) = "Federal id # " + FED_IDNO$ + &
		"  State id # " + STATE_IDNO$
	TITLE$(6%) = "State: " + CODE$ + &
		" SUTA id # " + SUTA_IDNO$ + "  SUTA Percent: " + &
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

		TEMP$ = "           State Total" + SPACE$(61%)
		TEXT$ = LEFT(TEMP$, 61%) + &
			FORMAT$(STATE_TOTAL, "##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GRAND_TOTAL = GRAND_TOTAL + STATE_TOTAL
	END IF

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
