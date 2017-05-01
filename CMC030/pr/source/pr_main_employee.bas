1	%TITLE "Employee Master File Maintenance"
	%SBTTL "PR_MAIN_EMPLOYEE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMPLOYEE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988, 1989, 1990, 1991 BY
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
	! Abstract:HELP
	!	.p
	!	The ^*Maintain Employee File\* option
	!	maintains information relative to each employee.  The file for each
	!	employee consists of a two (2) page personal information record, a
	!	marital/tax status record, a pay rate(s) record and, where applicable,
	!	one record for each standard payment, payroll deduction or payroll
	!	benefit accrual.
	!
	! Index:
	!	.x Maintain>Employee Master File
	!	.x Maintain>Employee Master File
	!	.x Employee Master File>Maintain
	!	.x Employee Master File>Maintain
	!	.x Employee>Master File
	!	.x Master File>Employee
	!
	! Option:
	!
	!	PR_MAIN_EMPLOYEE$STATUS
	!	PR_MAIN_EMPLOYEE$RATE
	!	PR_MAIN_EMPLOYEE$PAY-ERNDED
	!	PR_MAIN_EMPLOYEE$ACCRUAL
	!	PR_MAIN_EMPLOYEE$CHRONICLE
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMPLOYEE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMPLOYEE
	!	$ DELETE PR_MAIN_EMPLOYEE.OBJ;*
	!
	! Author:
	!
	!	09/16/87 - Kevin Handy
	!
	! Modification history:
	!
	!	12/01/87 - Robert Peterson
	!		Added default location and work center to file
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	02/07/89 - Frank F. Starman
	!		Added Location and Department List of Choices
	!
	!	03/27/90 - Kevin Handy
	!		Adjusted view screen so that all 10 characters
	!		of employee number were visible.
	!
	!	10/25/90 - Kevin Handy
	!		Added rate type 'X' (eXcess).
	!
	!	11/07/90 - Val Allen
	!		added field for active flag (y/n)
	!
	!	03/26/91 - Kevin Handy
	!		Spell checked source code.
	!
	!	04/08/91 - Kevin Handy
	!		Added PR_READ_DATES information on the bottom of
	!		the screen.
	!
	!	05/28/91 - Kevin Handy
	!		Fixed bug in validating WC code.  Called with
	!		"PR_MAIN_WC_DESC" instead of "PR_MAIN_WC_DESCR".
	!
	!	06/20/91 - Craig Tanner
	!		Added code on CASE 10% (Sort) to reverse last and first
	!		names.
	!
	!	07/03/91 - Craig Tanner
	!		Added list of choises to country feild.
	!
	!	12/26/91 - Kevin Handy
	!		Added ACCRUAL journal.
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/12/92 - Kevin Handy
	!		Modified TESTENTRY of operation field to look
	!		at OPER throughout, at not partially at location.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Fix format parameter to entr_3phone.
	!
	!	06/27/95 - Kevin Handy
	!		Removed 1099 entry field.
	!
	!	08/27/96 - Kevin Handy
	!		Added "N" for "Native American".
	!		Reformat source code.
	!
	!	09/12/96 - Kevin Handy
	!		Added SKILLS table lookup.
	!
	!	11/13/96 - Kevin Handy
	!		Added "A" for "Asian" in race field.
	!
	!	01/31/96 - Kevin Handy
	!		Moved work center (29) fiels over two characters
	!		so it doesn't cover up it's title.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	09/23/97 - Kevin Handy
	!		Lose lots of commented out code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	03/20/2000 - Kevin Handy
	!		Don't allow entry of zero pay frequency
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER
	MAP (PR_EMP_MASTER2) PR_EMP_MASTER_CDD PR_EMP_MASTER_OLD, PR_EMP_MASTER2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP	(PR_TAX_PKG)	PR_TAX_PKG_CDD		PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP	(PR_OPER)	PR_OPER_CDD		PR_OPER

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.HB"
	MAP	(PR_UNPN_DESC)	PR_UNPN_DESC_CDD	PR_UNPN_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP	(PR_WC_DESCR)	PR_WC_DESCR_CDD		PR_WC_DESCR

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP	(UTL_DEPARTMENT) UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.HB"
	MAP (PR_SKILLS)		PR_SKILLS_CDD		PR_SKILLS

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMPLOYEE) &
		PR_EMP_MASTER.CH%, &
		PR_EMP_MASTER.READONLY%

	COM (TT_PR_EMPLOYEE) &
		SEXTITLE$ = 20%, &
		SEX$(2%) = 20%, &
		RACETITLE$ = 20%, &
		RACE$(9%) = 20%, &
		ECTITLE$ = 20%, &
		EC$(6%) = 20% &

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS
	DECLARE PR_EMP_DATES_CDD CURRENT_DATES

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Payroll Employee Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_EMPLOYEE"
		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 38%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 20%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 39%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Employee-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Name"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::KNAME(2%) = "Alpha-sort"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 10%

		SMG_WINDOW::KNAME(3%) = "Soc-sec-num"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 11%

		SMG_WINDOW::KNAME(4%) = "Location"
			SMG_WINDOW::KFIELD(4%, 0%) = 3%
			SMG_WINDOW::KFIELD(4%, 1%) = 27%
			SMG_WINDOW::KFIELD(4%, 2%) = 28%
			SMG_WINDOW::KFIELD(4%, 3%) = 29%

		!
		! Sex table
		!
		SEXTITLE$ = "Sex Description"
		SEX$(0%) = "2"
		SEX$(1%) = "M   Male"
		SEX$(2%) = "F   Female"

		!
		! Race table
		!
		RACETITLE$ = "Code Description"
		RACE$(0%) = "9"
		RACE$(1%) = "     N/A"
		RACE$(2%) = "B    Black"
		RACE$(3%) = "H    Hispanic"
		RACE$(4%) = "O    Oriental"
		RACE$(5%) = "I    Indian"
		RACE$(6%) = "W    White"
		RACE$(7%) = "N    Native American"
		RACE$(8%) = "X    Other"
		RACE$(9%) = "A    Asian"

		!
		! List of Default rate types
		!
		ECTITLE$ = "Type Description"
		EC$(0%) = "5"
		EC$(1%) = "H    Hourly"
		EC$(2%) = "S    Salary"
		EC$(3%) = "P    Piece"
		EC$(4%) = "M    Mileage"
		EC$(5%) = "X    eXcess"
		EC$(6%) = ""

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_MASTER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_MASTER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMPLOYEE = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_MASTER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
		USE
			PR_MAIN_EMPLOYEE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_MASTER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_MASTER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%

		WHEN ERROR IN
			RESET #PR_EMP_MASTER.CH%
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SELECT MLOOP

		!
		! Main screen
		!
		CASE 0%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	1,  1,	"(01) Emp #", &
				2,  1,	"(02) Name", &
				4,  1,	"(03) Address", &
				5,  1,	"(04) Address", &
				6,  1,	"(05) City", &
				7,  1,	"(06) State", &
				8,  1,	"(07) Zip code", &
				9,  1,	"(08) Country", &
				11,  1,	"(09) Phone", &
				13,  1,	"(10) Sort", &
				1, 40,	"(11) Soc Sec Num", &
				4, 40,	"(12) Birth date", &
				5, 40,	"(13) Start date", &
				6, 40,	"(14) Term. Date", &
				7, 40,	"(15) Re-hire", &
				11, 40,	"(16) Sex", &
				12, 40,	"(17) Race", &
				13, 40,	"(18) US Citizen", &
				14, 40,	"(19) Work Permit", &
				15, 40,	"(20) Home country", &
				0,  0,	""

			RESTORE

			READ XPOS, YPOS, XSTR$
			I% = 0%
			WHILE (XPOS <> 0%)
				I% = I% + 1%
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! 1st page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	1,  1,	"(21) Pay freq", &
				3,  1,	"(22) Subacct", &
				4,  1,	"(23) Account", &
				6,  1,	"(24) Trade", &
				7,  1,	"(25) Oper", &
				8,  1,	"(26) Unn/Pen", &
				9,  1,	"(27) Location", &
				10,  1,	"(28) Dept", &
				11,  1,	"(29) Work Center", &
				12,  1,	"(30) Emp Skill", &
				13,  1,	"(31) Emp Grade", &
				14,  1,	"(32) Disabled", &
				2, 40,	"(33) State unempl", &
				3, 40,	"(34) Tax Package", &
				6, 40,	"(35) Rate Type", &
				7, 40,	"(36) Pay Code", &
				8, 40,	"(37) Workmen Comp", &
				9, 40,	"(38) Active? (Y/N)", &
				0,  0,	""

			RESTORE
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			READ XPOS, YPOS, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)
			WHILE (XPOS <> 0%)
				I% = I% + 1%
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS, YPOS) &
					IF SMG_WINDOW::HFLAG(I%) < 2%
				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

		END SELECT


	!
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Status raTe paY-ernded accrUal chronicLe"

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Employee status
		!
		CASE "STATUS"
	!++
	! Abstract:STATUS
	!	^*Status\*
	!	.p
	!	The ^*Status\* function
	!	accesses the Employee Status file where an employee's marital
	!	status and number of exemptions are maintained.
	!
	! Index:
	!	.x Status
	!
	!--
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STATUS.ID, "")

		!
		! Employee rate
		!
		CASE "RATE"
	!++
	! Abstract:RATE
	!	^*raTe\*
	!	.p
	!	The ^*raTe\* function
	!	accesses to the field where an employee's pay rate(s) are recorded. All rates
	!	may be recorded and all are date sensitive.
	!
	! Index:
	!	.x Rate>Pay
	!	.x Pay Rate
	!
	!--
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_RATE.ID, "")

		!
		! Employee ernded
		!
		CASE "PAY-ERNDED"
	!++
	! Abstract:PAY-ERNDED
	!	^*paY-ernded\*
	!	.p
	!	The ^*paY-ernded\* function
	!	accesses to the file where standard benefit accruals, payments, and
	!	deductions are recorded.
	!
	! Index:
	!	.x Standard Accruals
	!	.x Standard Pay
	!	.x Standard Deductions
	!
	!--
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STD_ERNDED.ID, "")

		!
		! Employee ernded
		!
		CASE "ACCRUAL"
	!++
	! Abstract:ACCRUAL
	!	^*accrUal\*
	!	.p
	!	The ^*accrUal\* function
	!	accesses the screens where various types of fringe benefit accruals
	!	may be recorded.
	!	.p
	!	An unlimited number of user defined types of accruals can be recorded.
	!	.p
	!	If the user's company policy dictates that a certain accrual type (vacation,
	!	for example) is to be accrued, but not made available to an employee until
	!	some predetermined time, (employment anniversary or beginning of a calendar
	!	year, for example) the "unavailable" accrual can be tracked and transferred
	!	to the "available" accrued field at whatever date policy dictates.  An
	!	accrual can also be immediately available.
	!	.p
	!	Any accrual is date sensitive.
	!	.p
	!	An accrual calculation can be nullified if an employee works less than a
	!	predetermined number of hours during a payroll period.  An accrual
	!	calculation can also be nullified if the total accrued or unused number
	!	of hours reaches a predetermined maximum.  The accrued hours are, of course,
	!	decremented when an employee uses some of them, such as taking vacation.
	!	.p
	!	Accrual rates can be expressed up to four (4) decimal points and applied as
	!	a flat rate or on a per hour basis.
	!	.p
	!	The frequency of an accrual is controlled.  For example, if an employee is
	!	paid on a weekly basis, the accrual could occur in coincidence with every
	!	regular payroll period (every week), but not occur on a special payroll
	!	period such as a bonus payroll.
	!
	! Index:
	!	.x Accruals
	!	.x Fringe Benefit>Accruals
	!
	!--
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL.ID, "")

		!
		! Diary
		!
		CASE "CHRONICLE"
	!++
	! Abstract:CHRONICLE
	!	^*chronicLe\*
	!	.b
	!	.lm +5
	!	The ^*chronicLe\* function
	!	accesses the employee work history. Such events as hire date,
	!	termination date, leave of absence, vacation, etc., can be recorded in this
	!	report.
	!	.lm -5
	!
	! Index:
	!	.x Chronicle
	!
	!--
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_DATES.ID, "")

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Erase record
		!
		CASE "Add"
			!
			! Erase any line items under the header
			!
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STATUS.ID, "A")
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_RATE.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_EMP_MASTER_OLD::EMPNUM <> PR_EMP_MASTER::EMPNUM
			THEN
				TEMP$ = PR_EMP_MASTER::EMPNUM + ""
				PR_EMP_MASTER = PR_EMP_MASTER_OLD
				PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STATUS.ID, "C" + TEMP$)
				PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_RATE.ID, "C" + TEMP$)
				PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STD_ERNDED.ID, "C" + TEMP$)
				PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL.ID, "C" + TEMP$)
				PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_DATES.ID, "C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STATUS.ID, "E")
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_RATE.ID, "E")
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_STD_ERNDED.ID, "E")
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL.ID, "E")
			PR_MAIN_EMPLOYEE = MAIN_JOURNAL(PR_MAIN_EMP_DATES.ID, "E")

		END SELECT


	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP1$,TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Employee _#	10 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Employee _#\* field accommodates up to ten (10) alphanumeric
	!	characters, providing a field large enough to use Social Security
	!	numbers as employee numbers if desired. The format is user defined.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Number
	!	.x Number>Employee
	!
	!--

			PR_EMP_MASTER::EMPNUM  = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;15", TEMP$, &
				PR_EMP_MASTER::EMPNUM, MFLAG, "'E", MVALUE)

			CALL PR_READ_DATES(PR_EMP_MASTER::EMPNUM, &
				"AC", DATE_TODAY, 1%, CURRENT_DATES)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Chronicle AC Start: " + &
				PRNT_DATE(CURRENT_DATES::DATEBEGIN, 8%) + &
				"  End: " + &
				PRNT_DATE(CURRENT_DATES::DATEEND, 8%) + &
				"  Descr: " + &
				CURRENT_DATES::DESCR, &
				18%, 1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.ts 55
	!	^*(02) Name	30 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field
	!	accommodates up to thirty (30) characters. The employee name
	!	should be entered first name first. (The Sort field is provided
	!	in order that alphabetical sorts can be accomplished.)
	!	.b
	!	The name entered in this field will print on payroll checks,
	!	quarterly payroll reports and W-2's.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Name
	!	.x Name>Employee
	!
	!--

			PR_EMP_MASTER::EMPNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;15", TEMP$, &
				PR_EMP_MASTER::EMPNAME, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Address	20 Characters\*
	!	.b
	!	.lm +5
	!	The first ^*Address\* line accommodates up to twenty (20)
	!	alphanumeric characters. If only one address line is needed for
	!	an employee, it is recommended that this field be left blank and
	!	the address be entered in the next address line.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Street Address
	!	.x Employee>Address
	!	.x Street Address>Employee
	!	.x Address>Employee
	!
	!--

			PR_EMP_MASTER::ADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;15", TEMP$, &
				PR_EMP_MASTER::ADD1, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Address	30 Characters\*
	!	.b
	!	.lm +5
	!	The second street ^*Address\* field will accommodate up to thirty
	!	(30) alphanumeric characters. It is recommended when there is
	!	only a one line address for an employee, the address be entered
	!	in this field.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Address
	!	.x Address>Employee
	!
	!--

			PR_EMP_MASTER::ADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;15", TEMP$, &
				PR_EMP_MASTER::ADD2, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) City\*
	!	.p
	!	The ^*City\* address field will accommodate up to sixteen (16)
	!	alphanumeric characters.
	!
	! Index:
	!	.x Employee>City
	!	.x City>Employee
	!
	!--

			PR_EMP_MASTER::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;15", TEMP$, &
				PR_EMP_MASTER::CITY, MFLAG, &
				"'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) State\*
	!	.p
	!	The ^*State\* address field is a two (2) character alphanumeric
	!	field and should be equal to the two character Post Office code for
	!	the respective State.
	!
	! Index:
	!	.x Employee>State
	!	.x State>Employee
	!
	!--

			PR_EMP_MASTER::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;15", TEMP$, &
				PR_EMP_MASTER::STATE, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Zip Code\*
	!	.p
	!	The ^*Zip code\* field will accommodate up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Employee>Zip Code
	!	.x Zip Code>Employee
	!
	!--

			PR_EMP_MASTER::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;15", TEMP$, &
				PR_EMP_MASTER::ZIP, MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Country\*
	!	.p
	!	The ^*Country\* field may be used if an employee's address is in a
	!	foreign country, otherwise, it is recommended the field be left blank.
	!	The field will accommodate up to six (6) alphanumeric characters.
	!
	! Index:
	!	.x Employee>Country
	!	.x Country>Employee
	!
	!--

			PR_EMP_MASTER::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;15", TEMP$, &
				LEFT$(PR_EMP_MASTER::COUNTRY, 2%), MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					LEFT$(PR_EMP_MASTER::COUNTRY, 2%)) = 1%
				THEN
					PR_EMP_MASTER::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO E0Loop
			END IF

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Phone\*
	!	.p
	!	The ^*Phone\* number field will accommodate up to ten (10) characters.
	!	No editing characters, i.e. ( ) and -, are to be entered. If only
	!	seven numbers are entered, the system will assume that the number
	!	contains no area code and will properly edit the number.
	!	.p
	!	This field may be left blank.
	!
	! Index:
	!	.x Employee>Telephone Number
	!	.x Telephone Number>Employee
	!	.x Employee>Phone
	!	.x Phone>Employee
	!
	!--

			PR_EMP_MASTER::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;15", TEMP$, &
				PR_EMP_MASTER::PHONE, MFLAG, 0%, MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Sort\*
	!	.p
	!	The ^*Sort\* (Also called ^*Alpha Sort\*)
	!	field is designed to contain the employee name with the
	!	last name first. Since the field contains fourteen (14) characters
	!	only, it is recommended that special characters, such as a comma after
	!	the last name, not be used.
	!
	! Index:
	!	.x Employee>Name>Alpha Sort
	!	.x Employee>Alpha Sort
	!	.x Alpha Sort>Employee
	!	.x Name>Employee>Alpha Sort
	!	.x Employee>Name>Sort
	!	.x Employee>Sort
	!	.x Sort>Employee
	!	.x Name>Employee>Sort
	!
	!--

			IF (TEMP1$ = "Add") AND ((MFLAG AND 1%) = 0%)
			THEN
				DUMMY$ = EDIT$(PR_EMP_MASTER::EMPNAME, 188%)
				RGT_HLF$ = DUMMY$
				V% = INSTR(1%, DUMMY$, " ")

				WHILE V% <> 0%
					X% = X% + V%
					RGT_HLF$ = EDIT$(RIGHT$(RGT_HLF$, V%), 188%)
					V% = INSTR(1%, RGT_HLF$, " ")
				NEXT

				PR_EMP_MASTER::SORT = RGT_HLF$ + " " + LEFT$(DUMMY$, X%)

			END IF

			PR_EMP_MASTER::SORT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;15", TEMP$, &
				PR_EMP_MASTER::SORT, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Social Security Number\*
	!	.p
	!	The employee's nine (9) digit social security number is be entered
	!	in the ^*Social Security Number\* field without entering the associated
	!	dashes.
	!
	! Index:
	!	.x Employee>Social Security Number
	!	.x Social Security Number>Employee
	!	.x Employee>SSN
	!	.x SSN>Employee
	!
	!--

			PR_EMP_MASTER::SSN = ENTR_3SSN(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;58", TEMP$, &
				PR_EMP_MASTER::SSN, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Birthdate\*
	!	.p
	!	The ^*Birthdate\* field is to be entered in MM/DD/YYYY format.
	!	.p
	!	This field may be left blank if the user has no specific need
	!	for the information.
	!
	! Index:
	!	.x Employee>Birthdate
	!	.x Birthdate>Employee
	!
	!--

			PR_EMP_MASTER::BIRTH = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;58", TEMP$, &
				PR_EMP_MASTER::BIRTH, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Start Date\*
	!	.p
	!	The employee's ^*Start date\* or ^*Date of Employment\* is to be entered
	!	in MM/DD/YYYY format.
	!
	! Index:
	!	.x Employee>Date of Employment
	!	.x Employee>Start Date
	!	.x Date of Employment>Employee
	!	.x Start Date>Employee
	!
	!--

			PR_EMP_MASTER::HIREDAY = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;58", TEMP$, &
				PR_EMP_MASTER::HIREDAY, MFLAG, "'E", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Termination Date\*
	!	.p
	!	The employee's ^*Termination Date\* is to be entered in MM/DD/YYYY
	!	format. Records with termination dates entered will not be printed
	!	on employee time transmittals.
	!
	! Index:
	!	.x Employee>Termination Date
	!	.x Termination Date>Employee
	!
	!--

			PR_EMP_MASTER::TERMDAY = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;58", TEMP$, &
				PR_EMP_MASTER::TERMDAY, MFLAG, "'E", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Re_-hire\*
	!	.p
	!	The ^*Re_-hire\* field records when an employee
	!	is terminated, whether or not that individual would be considered for
	!	re-employment. A "Y" flag indicates re-hire eligibility. An "N" flag
	!	indicates a terminated employee is not eligible to be re-hired.
	!
	! Index:
	!	.x Employee>Re-hire
	!	.x Re-hire>Employee
	!
	!--

			PR_EMP_MASTER::REHIRE_FLAG = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;58", TEMP$, &
				PR_EMP_MASTER::REHIRE_FLAG, MFLAG, &
				"'E", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Sex\*
	!	.p
	!	The ^*Sex\* field is a one (1) character field. Valid characters are
	!	"M" or "F". If the user has no specific need for this information,
	!	such as EEO reports, this field may be blank.
	!
	! Index:
	!	.x Employee>Sex
	!	.x Sex>Employee
	!
	!--

			PR_EMP_MASTER::SEX = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;58", TEMP$, &
				PR_EMP_MASTER::SEX, MFLAG OR 16%, "'", MVALUE, &
				SEX$(), SEXTITLE$, "004"), -1%)

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Race\*
	!	.p
	!	The ^*Race\* field is a one character
	!	user defined code. If the user
	!	has no specific requirement for
	!	recording race, such as the EEO report,
	!	this field may be left
	!	blank.
	!	.p
	!	This field is normally used for filling out
	!	EEO reports.
	!
	! Index:
	!	.x Employee>Race
	!	.x Race>Employee
	!
	!--

			PR_EMP_MASTER::RACE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;58", TEMP$, &
				PR_EMP_MASTER::RACE, MFLAG, "'", MVALUE, &
				RACE$(), RACETITLE$, "005"), -1%)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) US Citizen\*
	!	.p
	!	The ^*US Citizen\* field indicates whether or
	!	not an employee is a citizen of the United States. A "Y" = Yes; an
	!	"N" = No.
	!
	! Index:
	!	.x Employee>Citizenship
	!	.x Citizenship>Employee
	!	.x US Citizen>Employee
	!	.x Employee>US Citizen
	!
	!--

			PR_EMP_MASTER::USCIT = "Y" IF PR_EMP_MASTER::USCIT = ""

			PR_EMP_MASTER::USCIT = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;58", TEMP$, &
				PR_EMP_MASTER::USCIT, MFLAG, "'E", MVALUE)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) Work Permit\*
	!	.p
	!	The ^*Work Permit\* field records an employee's
	!	work permit number in the event the employee is not a U.S. citizen.
	!	The field will accommodate up to fifteen (15) characters.
	!
	! Index:
	!	.x Employee>Work Permit
	!	.x Work Permit>Employee
	!
	!--

			PR_EMP_MASTER::WRKPERMIT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;58", TEMP$, &
				PR_EMP_MASTER::WRKPERMIT, MFLAG, "'E", MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	^*(20) Home country\*
	!	.p
	!	The ^*Home country\* field records the country
	!	of citizenship in the event an employee is not a U.S. citizen. The
	!	field will accommodate six (6) characters.
	!
	! Index:
	!	.x Employee>Home Country
	!	.x Home Country>Employee
	!
	!--

			PR_EMP_MASTER::HOMCNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;58", TEMP$, &
				PR_EMP_MASTER::HOMCNTRY, MFLAG, "'E", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(21) Pay Frequency\*
	!	.p
	!	The ^*Pay Frequency\* field is a two (2) character numeric field and
	!	must contain the number of regular payroll periods for which the
	!	employee will be paid, provided he(she) works a full calendar year.
	!	.b
	!	.lm +10
	!	.LIST 0,"*"
	!	.LE
	!	Weekly payroll = 52
	!	.LE
	!	Bi-weekly payroll = 26
	!	.LE
	!	Semi-monthly = 24
	!	.LE
	!	Monthly = 12
	!	.ELS
	!
	! Index:
	!	.x Employee>Pay Frequency
	!	.x Pay Frequency>Employee
	!	.x Employee>Frequency
	!	.x Frequency>Employee
	!
	!--

			PR_EMP_MASTER::PAYFREQ = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::LWINDOW, "1;16", TEMP$, &
				PR_EMP_MASTER::PAYFREQ * 1.0, MFLAG, &
				"##", MVALUE)

		CASE 22%

	!++
	! Abstract:FLD022
	!	^*(22) Subaccount\*
	!	.p
	!	The ^*Subaccount\* field records a default subaccount
	!	number in the event an employee generally works on the same job,
	!	process or other category which utilizes the subaccount field. When
	!	entering time worked by an employee, the subaccount field in the
	!	time entry routine will default to any value in this field. (The
	!	default value may be overridden during the time entry routine.)
	!	.p
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Employee>Subaccount
	!	.x Subaccount>Employee
	!
	!--

			PR_EMP_MASTER::SUBACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "3;16", TEMP$, &
				PR_EMP_MASTER::SUBACC, MFLAG, "'E", MVALUE)

		CASE 23%

	!++
	! Abstract:FLD023
	!	^*(23) Account\*
	!	.p
	!	The ^*Account\* field records a default General
	!	Ledger account number in the event an employee's time worked and
	!	earnings are generally charged to the same account number. When
	!	entering the time worked by an employee, the account to be charged
	!	will default to the value in this field. (The default may be overridden
	!	during the time entry routine.)
	!	.p
	!	The field will accommodate up to eighteen (18) characters, and
	!	the value must be equal to an account number which has been established
	!	in the General Ledger Chart of Accounts.
	!
	! Index:
	!	.x Employee>Default>GL Account Number
	!	.x Employee>GL Account Number
	!	.x Employee>Account Number
	!	.x GL Account>Employee
	!	.x Account>Employee
	!
	!--

			PR_EMP_MASTER::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "4;16", TEMP$, &
				PR_EMP_MASTER::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_EMP_MASTER::ACCT = GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 24%

	!++
	! Abstract:FLD024
	!	^*(24) Trade\*
	!	.p
	!	The ^*Trade\* field is a six (6) character user defined field.
	!
	! Index:
	!	.x Employee>Trade
	!	.x Trade>Employee
	!
	!--

			PR_EMP_MASTER::TRADE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "6;16", TEMP$, &
				PR_EMP_MASTER::TRADE, MFLAG, "'E", MVALUE)

		CASE 25%

	!++
	! Abstract:FLD025
	!	^*(25) Operation\*
	!	.p
	!	The ^*Operation\* field records a default
	!	operation in the event an employee's time worked and earnings are
	!	generally charged to the same operation. When entering the time
	!	worked by an employee, the operation to be charged will default to
	!	the value in this field. (The default may be overridden during the
	!	time entry routine.)
	!	.p
	!	The field will accommodate up to eight (8) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Employee>Operation
	!	.x Operation>Employee
	!
	!--

			PR_EMP_MASTER::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "7;16", TEMP$, &
				PR_EMP_MASTER::OPER, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_OPER.ID, "VX ") = 1%)
				THEN
					PR_EMP_MASTER::OPER = PR_OPER::OPER
				END IF
				GOTO E0Loop
			END IF

		CASE 26%

	!++
	! Abstract:FLD026
	!	^*(26) Union\*
	!	.p
	!	The ^*Union\* field records a two (2) character
	!	user defined union code representing the union to which an employee
	!	belongs.
	!	.p
	!	This field is not applicable, it may be left blank.
	!
	! Index:
	!	.x Employee>Union
	!	.x Union>Employee
	!
	!--

			PR_EMP_MASTER::UNION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "8;16", TEMP$, &
				PR_EMP_MASTER::UNION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_UNPN_DESC.ID, "VX ") = 1%)
				THEN
					PR_EMP_MASTER::UNION = &
						PR_UNPN_DESC::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 27%

	!++
	! Abstract:FLD027
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field is a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_MASTER::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "9;16", TEMP$, &
				PR_EMP_MASTER::LOCATION, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX ") = 1%
				THEN
					PR_EMP_MASTER::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO E0Loop
			END IF

		CASE 28%

	!++
	! Abstract:FLD028
	!	^*(28) Department\*
	!	.p
	!	The ^*Department\* field records a department
	!	to which an employee is assigned.
	!	.p
	!	The field is a six (6) character alphanumeric field. It may be
	!	blank.
	!	.p
	!	The value of this field is defaulted during time entry routines.
	!	The default may be overwritten.
	!
	! Index:
	!	.x Employee>Department
	!	.x Department>Employee
	!
	!--

			PR_EMP_MASTER::DEPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "10;16", TEMP$, &
				PR_EMP_MASTER::DEPT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, "V0" + &
					PR_EMP_MASTER::LOCATION) = 1%
				THEN
					PR_EMP_MASTER::DEPT = &
						UTL_DEPARTMENT::DEPT_NUM
				END IF
				GOTO E0Loop
			END IF

		CASE 29%

	!++
	! Abstract:FLD029
	!	^*(29) Work Center\*
	!	.p
	!	The ^*Work Center\* field
	!	enters a physical or functional center where an employee
	!	normally works. The field will accommodate up to four (4) characters.
	!	The field may be blank.
	!	.p
	!	This field will be defaulted when time is entered.
	!	The defaulted may be overwritten during time entry.
	!
	! Index:
	!	.x Employee>Work Center
	!	.x Work Center>Employee
	!
	!--

			PR_EMP_MASTER::WORK_CENTER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "11;18", TEMP$, &
				PR_EMP_MASTER::WORK_CENTER, MFLAG, &
				"'E", MVALUE)

		CASE 30%

	!++
	! Abstract:FLD030
	!	^*(30) Employee Skill\*
	!	.p
	!	The ^*Employee Skill\* field enters a six
	!	(6) character user defined code which identifies a particular job
	!	skill, i.e. electrician, plumber, assembler, etc.
	!	.p
	!	An important function of the Employee Skill field is to provide
	!	information used to create EEO reports.
	!	.p
	!	Pressing <List-Choices> will list valid skills, as defined in
	!	the skills table.
	!
	! Index:
	!	.x Employee>Skill
	!	.x Skill>Employee
	!
	!--

			PR_EMP_MASTER::EMP_SKILL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "12;16", TEMP$, &
				PR_EMP_MASTER::EMP_SKILL, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PR_MAIN_SKILLS.ID, "V0" + &
					PR_EMP_MASTER::EMP_SKILL) = 1%
				THEN
					PR_EMP_MASTER::EMP_SKILL = &
						PR_SKILLS::SKILL
				END IF
				GOTO E0Loop
			END IF

		CASE 31%

	!++
	! Abstract:FLD031
	!	^*(31) Employee Grade\*
	!	.p
	!	The ^*Employee Grade\* field enters a user defined
	!	two (2) character code which identifies an employee's grade level, i.e.
	!	an apprentice or a journeyman, or any other user defined grade level
	!	designation.
	!
	! Index:
	!	.x Employee>Grade
	!	.x Grade>Employee
	!
	!--

			PR_EMP_MASTER::EMP_GRADE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "13;16", TEMP$, &
				PR_EMP_MASTER::EMP_GRADE, MFLAG, "'E", MVALUE)

		CASE 32%

	!++
	! Abstract:FLD032
	!	^*(32) Disabled\*
	!	.p
	!	The ^*Disabled\* field
	!	enters a "Yes" or "No" flag indicating whether an employee has a
	!	disability.
	!
	! Index:
	!	.x Employee>Disability Flag
	!	.x Disability Flag>Employee
	!
	!--

			PR_EMP_MASTER::DISABLED = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::LWINDOW, "14;16", TEMP$, &
				PR_EMP_MASTER::DISABLED, MFLAG, "'E", MVALUE)

		CASE 33%

	!++
	! Abstract:FLD033
	!	^*(33) State Unemployment\*
	!	.p
	!	The ^*State Unemployment\* field must contain the two (2) character
	!	Post Office code for the State where the employee's earnings are
	!	subject to State Unemployment (SUTA) tax.
	!
	! Index:
	!	.x Employee>State Unemployment Code
	!	.X Employee>SUTA Code
	!	.x State Unemployment Code>Employee
	!	.x SUTA Code>Employee
	!
	!--

			PR_EMP_MASTER::SUI_SW = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "2;60", TEMP$, &
				PR_EMP_MASTER::SUI_SW, MFLAG, "'E", MVALUE)

		CASE 34%

	!++
	! Abstract:FLD034
	!	^*(34) Tax Package\*
	!	.p
	!	The ^*Tax Package\* field
	!	enters a default tax package number or mnemonic designation
	!	referring to the Tax Package Table.
	!	.p
	!	Theoretically, an employee's earnings could be subject to State
	!	withholding taxes, city withholding taxes, county withholding taxes
	!	and school district withholding taxes singly or any combination
	!	thereof. In addition, while one employee's earnings could be subject
	!	to tax withholding in one combination of jurisdictions, another
	!	employee's earnings might be subject to tax withholding in another
	!	combination of jurisdictions, including different states,
	!	cities, etc.
	!	.p
	!	The Tax Package default may be overwritten during the time entry
	!	routine in the event some or all of an employee's earnings may be
	!	subject to jurisdictions different from the default.
	!
	! Index:
	!	.x Employee>Tax Package
	!	.x Tax Package>Employee
	!
	!--

			PR_EMP_MASTER::TAX_PKG = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "3;60", TEMP$, &
				PR_EMP_MASTER::TAX_PKG, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_TAX_PKG.ID, "V0 ") = 1%)
				THEN
					PR_EMP_MASTER::TAX_PKG = &
						PR_TAX_PKG::TAX_PKG
				END IF
				GOTO E0Loop
			END IF

		CASE 35%

	!++
	! Abstract:FLD035
	!	^*(35) Rate Type\*
	!	.p
	!	The ^*Rate Type\* field
	!	enters a default rate type. Valid rate types are:
	!	.b
	!	.lm +20
	!	.list 0,"o"
	!	.le
	!	H = Hourly
	!	.le
	!	S = Salary
	!	.le
	!	P = Piece
	!	.le
	!	M = Mileage
	!	.LE
	!	X = eXcess (Maximum of Hour/Piece)
	!	.els
	!	.lm -20
	!	.p
	!	The purpose of this field is to provide a default rate type in
	!	the event an employee works at an operation for which there is no record
	!	in the employee's rate file. In that event, the employee would be paid
	!	the rate in the operation table for the subject operation relative to
	!	the default rate type in the employee's masterfile.
	!
	! Index:
	!	.x Rate Type>Employee
	!	.x Employee>Rate Type
	!
	!--

			PR_EMP_MASTER::RATE_TYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;60", TEMP$, &
				PR_EMP_MASTER::RATE_TYPE, MFLAG, "'", MVALUE, &
				EC$(), ECTITLE$, "005"), -1%)

		CASE 36%

	!++
	! Abstract:FLD036
	!	^*(36) Pay Code\*
	!	.p
	!	The ^*Pay Code\* field
	!	enters a default pay code. Valid pay codes are those
	!	established by the user in the Pay, Accrual and Deduction Code
	!	Definition Table. Valid codes can be listed by pressing ^*<List
	!	Choices>\* while the cursor is located at the Pay Code field.
	!	.p
	!	The purpose of this field is to provide a default pay code in
	!	the event an employee works at an operation for which there is no
	!	record in the employee's rate file. In that event, the employee
	!	would be paid the rate in the operation table for the subject
	!	operation relative to the default rate type in the employee's
	!	masterfile. The pay code for such earnings would default to the
	!	pay code in the employee's masterfile.
	!
	! Index:
	!	.x Pay Code>Employee
	!	.x Employee>Pay Code
	!
	!--

			PR_EMP_MASTER::RATE_CDE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "7;60", TEMP$, &
				PR_EMP_MASTER::RATE_CDE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, "V0P") = 1%)
				THEN
					PR_EMP_MASTER::RATE_CDE = &
						PR_ERNDED_DEF::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 37%

	!++
	! Abstract:FLD037
	!	^*(37) Workmens Comp\*
	!	.p
	!	The ^*Workmens Comp\* field enters a default
	!	workmens compensation code for an employee. A valid value in this
	!	field will cause the system to override any other logic for determining
	!	the handling of workmens compensation or related employee withholding
	!	and/or employer liability.
	!
	! Index:
	!	.x Employee>Workmens Compensation
	!	.x Workmens Compensation>Employee
	!
	!--

			PR_EMP_MASTER::WC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "8;60", TEMP$, &
				PR_EMP_MASTER::WC, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_WC_DESCR.ID, "VX ") = 1%)
				THEN
					PR_EMP_MASTER::WC = PR_WC_DESCR::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 38%

	!++
	! Abstract:FLD038
	!	^*(38) Active Flag\*
	!	.p
	!	The ^*Active Flag\* field indicates whether or
	!	not an employee is currently active A "Y" = Yes; an "N" = No.
	!
	! Index:
	!	.x Employee>Active Flag
	!	.x Active Flag>Employee
	!
	!--

			PR_EMP_MASTER::ACTIVE_FLAG = "Y" IF PR_EMP_MASTER::ACTIVE_FLAG = ""

			PR_EMP_MASTER::ACTIVE_FLAG = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::LWINDOW, "9;59", TEMP$, &
				PR_EMP_MASTER::ACTIVE_FLAG, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_EMPLOYEE = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_EMP_MASTER::EMPNUM = ""
			THEN
				PR_MAIN_EMPLOYEE = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank Employee # not allowed", 1%)
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #PR_EMP_MASTER.CH%, &
							KEY #0% EQ PR_EMP_MASTER::EMPNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					PR_MAIN_EMPLOYEE = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				ELSE
					IF EDIT$(MVALUE, -1%) = "CHANGE" AND &
						PR_EMP_MASTER::ACTIVE_FLAG = "Y"
					THEN
						PR_MAIN_EMPLOYEE = 1%

						CALL ENTR_3MESSAGE(SCOPE, "Employee is " + &
							"active.  Change of empl # not allowed", 0%)
					END IF
				END IF
			END IF

		CASE 11%

20310			IF (MVALUE = "ADD") AND NOT (PR_EMP_MASTER::SSN = "")
			THEN
				WHEN ERROR IN
					FIND #PR_EMP_MASTER.CH%, &
						KEY #3% EQ PR_EMP_MASTER::SSN + "", &
						REGARDLESS
				USE
					CONTINUE 32767
				END WHEN

				CALL ENTR_3MESSAGE(SCOPE, &
					"WARNING: Soc. Sec. Number already in use!", &
					0%)
			END IF

		CASE 21%
			IF PR_EMP_MASTER::PAYFREQ = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Pay Frequency Must Not Be Zero", &
					0%)
				PR_MAIN_EMPLOYEE = 1%
			END IF

		CASE 23%
			!
			! Is the input defined?
			!
			PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_EMP_MASTER::ACCT, GL_CHART::DESCR, &
				"PR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(GL_CHART::DESCR, 20%), &
				5%, 15%, , SMG$M_BOLD)

		CASE 25%
			IF PR_EMP_MASTER::OPER <> ""
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
					PR_EMP_MASTER::OPER, &
					PR_OPER::OPER, &
					"PR", MLOOP, "PRG", &
					"Operation", PR_MAIN_OPER.ID)
			END IF

		CASE 26%
			IF PR_EMP_MASTER::UNION <> ""
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
					PR_EMP_MASTER::UNION, &
					PR_UNPN_DESC::DESCR, &
					"PR", MLOOP, "PRG", &
					"Union/Pen", PR_MAIN_UNPN_DESC.ID)
			END IF

		CASE 27%
			IF PR_EMP_MASTER::LOCATION <> ""
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
					PR_EMP_MASTER::LOCATION, &
					UTL_LOCATION::LOCNAME, &
					"PR", MLOOP, "LOC", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

		CASE 34%
			!
			! Is the input defined?
			!
			PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_EMP_MASTER::TAX_PKG, &
				PR_TAX_PKG::CODE, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_TAX_PKG.ID)

		CASE 36%
			IF PR_EMP_MASTER::RATE_CDE <> ""
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
					"P" + PR_EMP_MASTER::RATE_CDE, &
					PR_ERNDED_DEF::DESCR, &
					"PR", MLOOP, "PRG", &
					"Code", PR_MAIN_ERNDED_DEF.ID)
			END IF

		CASE 37%
			IF PR_EMP_MASTER::WC <> ""
			THEN
			!
			! Is the input defined?
			!
			PR_MAIN_EMPLOYEE = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_EMP_MASTER::WC, &
				PR_WC_DESCR::DESCR, &
				"PR", MLOOP, "PRG", &
				"Workmen Comp is undefined", PR_MAIN_WC_DESCR.ID)
			END IF

20390		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		PR_MAIN_EMPLOYEE = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE" AND &
			PR_EMP_MASTER::ACTIVE_FLAG = "Y"
		THEN
			PR_MAIN_EMPLOYEE = 1%

			CALL ENTR_3MESSAGE(SCOPE, &
				"Employee is active.  Erase not allowed", 0%)
		END IF

	!
	! Set PR_EMP_MASTER_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_MASTER_OLD = PR_EMP_MASTER

	!
	! Restore PR_EMP_MASTER_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_MASTER = PR_EMP_MASTER_OLD

	!
	! Display junk
	!
	CASE OPT_DISPLAY
		!
		! Stuff for only the second page
		!
		IF (SMG_WINDOW::HFLAG(23%) AND 2%) = 0%
		THEN
			IF SMG_WINDOW::LCURR = 1%
			THEN
				!
				! Is the input defined?
				!
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + PR_EMP_MASTER::ACCT) <> 1%
				THEN
					GL_CHART::DESCR = &
						STRING$(LEN(GL_CHART::DESCR), &
						63%)
				END IF

				!
				! Print chart description
				!
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					LEFT(GL_CHART::DESCR, 20%), &
					5%, 15%, , SMG$M_BOLD)
			END IF
		END IF

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_MASTER2 = PR_EMP_MASTER
		PR_EMP_MASTER2::W2_1099 = "N"

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_MASTER = PR_EMP_MASTER2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " Emp number  Name                " + &
				"Sort           SSN           Loc  Dept   " + &
				"WC"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,033,048,062,067,074"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PR_EMP_MASTER::EMPNUM + " " + &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
				LEFT(PR_EMP_MASTER::SORT, 14%) + " " + &
				LEFT(PR_EMP_MASTER::SSN, 13%) + " " + &
				PR_EMP_MASTER::LOCATION + " " + &
				PR_EMP_MASTER::DEPT + " " + &
				LEFT(PR_EMP_MASTER::WORK_CENTER, 5%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #1% GE PR_EMP_MASTER::EMPNAME + "", &
				REGARDLESS

		CASE 2%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #2% GE PR_EMP_MASTER::SORT + "", &
				REGARDLESS

		CASE 3%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #3% GE PR_EMP_MASTER::SSN + "", &
				REGARDLESS

		CASE 4%
			FIND #PR_EMP_MASTER.CH%, KEY #4% GE &
				PR_EMP_MASTER::LOCATION + &
				PR_EMP_MASTER::DEPT + &
				PR_EMP_MASTER::WORK_CENTER, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD039
	!	^*(39) Active Flag\*
	!	.p
	!	The ^*Active Flag\* field indicates whether or
	!	not an employee is currently active A "Y" = Yes; an "N" = No.
	!
	! Index:
	!	.x Employee>Active Flag
	!	.x Active Flag>Employee
	!
	!--
