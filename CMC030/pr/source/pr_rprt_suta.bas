1	%TITLE "Payroll SUTA Report"
	%SBTTL "PR_RPRT_SUTA"
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
	!	Employee Name
	!	.le
	!	Social Security Number
	!	.le
	!	Start Date
	!	.le
	!	Year to Date Wages
	!	.le
	!	Quarter to Date Wages
	!	.le
	!	Quarter to Date Excess
	!	.le
	!	Quarter to Date SUI Wages
	!	.le
	!	Year to Date Weeks Worked
	!	.le
	!	Quarter to Date Weeks Worked
	!	.els
	!
	! Index:
	!	.x Report>SUTA
	!	.x Report>State Unemployment Taxes
	!	.x State Unemployment Tax>Report
	!	.x SUTA>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_SUTA/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_SUTA, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_SUTA.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/21/89 - Kevin Handy
	!		Fixed problem in reading tax file where it would
	!		not use the state code defined in that record,
	!		but replace it with the master file value.
	!
	!	06/22/89 - Kevin Handy
	!		Modified so that temporary file is opened
	!		"temporary" so it is automatically deleted
	!		when program finishes.
	!
	!	06/22/89 - Kevin Handy
	!		Added BUFFERS clause to temporary file open
	!		to speed up creation.
	!
	!	01/27/90 - Kevin Handy
	!		Fixed error trapping format.
	!
	!	01/27/90 - Kevin Handy
	!		Completely rewrote section that calculates SU wages.
	!
	!	06/04/90 - Kevin Handy
	!		Fixed up strange code in the FACTOR section.
	!
	!	06/06/90 - Kevin Handy
	!		Fixed bug where wasn't zeroing employee amounts
	!		after printing an employee.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	07/19/90 - Kevin Handy
	!		Modified titles so that it will print properly,
	!		instead of losing titles.
	!
	!	01/02/91 - Kevin Handy
	!		Modified for changes in PR_REG_TAXES file.  Major
	!		simplification of code as a result.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	02/12/91 - Kevin Handy
	!		Added code to use PR_EMP_MASTER::SUI_SW code
	!		as the state code if it is not blank.
	!		Overrides anything in the SW code.
	!
	!	03/29/91 - Kevin Handy
	!		Fixed so that an undefined employee number
	!		won't crash program.
	!
	!	04/25/91 - Kevin Handy
	!		Fixed Final total to it calls STProfile:
	!		instead of having a (bad) copy of the code contained
	!		therein.
	!
	!	11/05/91 - Kevin Handy
	!		Fixed so that SUI_MAX of 0 means unlimited, not
	!		a limit of 0.0
	!
	!	03/24/92 - Kevin Handy
	!		Modified to use "SUTA Account" field instead of
	!		"State ID Number"
	!
	!	06/23/92 - Kevin Handy
	!		Fixed bug in location totals where it wasn't using
	!		the right percentages (0.0 instead of 0.005).
	!
	!	06/23/92 - Kevin Handy
	!		Added wildcard location.
	!
	!	07/02/92 - Kevin Handy
	!		Fixed bugs in printing state lines in the header.
	!		Wouldn't reliably shift states, or print first
	!		state.
	!
	!	03/15/93 - Kevin Handy
	!		Started modifications to handle SUTA reportable
	!		different than the STATE reportable.
	!		- Make PR_TEMP use record instead of map.
	!		- Lookup problem items in PR_ERNDED_DEF.
	!		- Buffer PR_TEMP records before writing them out.
	!		- Open ERNDED file.
	!		- Search ERNDED file, distribute deltas.
	!
	!	04/05/93 - Kevin Handy
	!		Fix bug where was subtracting deductions instead
	!		of adding them, and using QTR% as a loop variable
	!		which messed up which quarter they wanted to look
	!		at. Spelling error in var ADJUST_NEGITIVE.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added regardless to several get sttements.
	!
	!	10/25/94 - Kevin Handy
	!		Fix SUTA taxable calculation for positive-negative.
	!		Must look at the code as well as the taxable flag
	!		to decide if needs added or subtracted.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/27/95 - Kevin Handy
	!		Lose check for 1099 flag, so we don't have any
	!		magic happening.
	!
	!	07/30/96 - Kevin Handy
	!		Reformat source code.
	!		Use PR_REG_TAXES::CODE instead of
	!		PR_EMP_MASTER::SUI_SW (LL)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
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
	!		Fix the info thrown into SORTKEY when sorting
	!		by department so it matches the tests for "DP"
	!		later in the code.
	!
	!	04/17/2000 - Kevin Handy
	!		use WHEN ERROR IN
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
	!	The ^*From State\* field enters the state with
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
	!	The ^*To State\* field enters a code for the
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
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

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
		IF (PR_ERNDED_DEF::REPORTABLE_SWH = "N" XOR &
			(PR_ERNDED_DEF::ETYPE <> "D" AND &
			PR_ERNDED_DEF::ETYPE <> "F"))
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

400	CALL ENTR_3MESSAGE(SCOPE, &
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

	IF PR_REG_TAXES::CODE = "  "
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
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + &
		" Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = SB_TEXT$
	TITLE$(4%) = "."
	TITLE$(5%) = "."

	!
	! Column headings
	!
	TITLE$(6%) = ""
	TITLE$(7%) = "EmpNum     EmpName                    SSN         " + &
		"StrtDate       YTD Wages     QTD Wages    QTD Excess    " + &
		"QTD SUI Wg  YTD WW  QTD WW"
	TITLE$(8%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:037,$SSN:049,DStartDate:058," + &
		"VYTDWages:074,VQTDWages:088,VQTDExcess:102,VQTDSUIWg:116," + &
		"VYTDWW:124,VQTDWW:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_LOC$ = ""

	TEST_STCODE$ = ""

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

	GOTO ExitTotal IF (PR_TEMP::STCODE > TO_ITEM$) AND (TO_ITEM$ <> "")

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 6%

	EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + PR_TEMP::WAGES(I%), 2%) &
		FOR I% = 0% TO QTR% - 1%

	EMP_TOTAL(2%) = FUNC_ROUND(PR_TEMP::WAGES(QTR% - 1%), 2%)

	EMP_TOTAL(5%) = EMP_TOTAL(5%) + PR_TEMP::WKWRK(I%) &
		FOR I% = 0% TO QTR% - 1%

	EMP_TOTAL(6%) = PR_TEMP::WKWRK(QTR% - 1%)

	PR_EMP_MASTER::EMPNUM	= PR_TEMP::EMPNUM
	PR_EMP_MASTER::EMPNAME	= "???????????????????????????????"
	PR_EMP_MASTER::SSN	= "???-??-????"
	PR_EMP_MASTER::HIREDAY	= "00000000"
	PR_EMP_MASTER::LOCATION	= ""

17060	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TEMP::EMPNUM, REGARDLESS
	USE
		CONTINUE 17070 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17070	!
	! Check location wildcard
	!
	IF WILDLOC$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(PR_EMP_MASTER::LOCATION, WILDLOC$) = 0%
	END IF

	!
	! Check for department number change
	!
	IF TEST_STCODE$ <> PR_TEMP::STCODE
	THEN
		GOSUB STProfile
		STATE_TOTAL(I%) = 0.0 FOR I% = 1% TO 6%
	END IF

	!
	! Print department totals?
	!
	IF SORTBY$ = "DP" AND &
		((THIS_LOC$ <> PR_EMP_MASTER::LOCATION + &
		PR_EMP_MASTER::DEPT + &
		PR_EMP_MASTER::WORK_CENTER) OR (THIS_LOC$ == ""))
	THEN
		TEMP_SUI_PCT = SUI_PCT
		GOSUB LocationTotal
		GOSUB LocationLoad
	END IF

17100	!
	! Print total for employee
	!
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 6%

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

		STATE_TOTAL(LOOP%) = STATE_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 6%

		SUB_TOTAL(LOOP%) = SUB_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 6%

		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 26%) + " " + &
			LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 11%), 11%) + " " + &
			PRNT_DATE(PR_EMP_MASTER::HIREDAY, 10%) + "   " + &
			FORMAT$(EMP_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(4%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(5%), "###,### ") + &
			FORMAT$(EMP_TOTAL(6%), "###,###")

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
			FORMAT$(SUB_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(SUB_TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(SUB_TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(SUB_TOTAL(4%), "##,###,###.## ") + &
			FORMAT$(SUB_TOTAL(5%), "###,### ") + &
			FORMAT$(SUB_TOTAL(6%), "###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = "           SUI Tax Liability = QTD SUI Wages X " + &
			"SUI Percent = " + &
			FORMAT$(SUB_TOTAL(4%), "##,###,###.##") + &
			" X " + &
			FORMAT$(TEMP_SUI_PCT, ".#####") + &
			" = " + &
			FORMAT$(SUB_TOTAL(4%) * TEMP_SUI_PCT, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TITLE$(4%) = "Loc: " + PR_EMP_MASTER::LOCATION + &
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
	GOSUB STProfile

	TEMP$ = "           Grand Total" + SPACE$(61%)
	TEXT$ = LEFT(TEMP$, 61%) + &
		FORMAT$(GRAND_TOTAL(1%), "##,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "##,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "##,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "##,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "###,### ") + &
		FORMAT$(GRAND_TOTAL(6%), "###,###")

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
	SUI_PCT = 0.0
	SUI_MAX = 0.0

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

		TEMP$ = "           State Total" + SPACE$(61%)
		TEXT$ = LEFT(TEMP$, 61%) + &
			FORMAT$(STATE_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(STATE_TOTAL(2%), "##,###,###.## ") + &
			FORMAT$(STATE_TOTAL(3%), "##,###,###.## ") + &
			FORMAT$(STATE_TOTAL(4%), "##,###,###.## ") + &
			FORMAT$(STATE_TOTAL(5%), "###,### ") + &
			FORMAT$(STATE_TOTAL(6%), "###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "           SUI Tax Liability = QTD SUI Wages X " + &
			"SUI Percent = " + &
			FORMAT$(STATE_TOTAL(4%), "##,###,###.##") + &
			" X " + &
			FORMAT$(TEMP_SUI_PCT, ".#####") + &
			" = " + &
			FORMAT$(STATE_TOTAL(4%) * TEMP_SUI_PCT, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)

		GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + &
			STATE_TOTAL(I%) FOR I% = 1% TO 6%
	END IF

	TEST_STCODE$ = PR_TEMP::STCODE

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
