1	%TITLE "Employee Master File Maintenance"
	%SBTTL "PR_MAIN_EMP_W2"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_W2(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.p
	!	The ^*Maintain Employee File\* option
	!	maintains all information relative to each employee.  The file for each
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
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_W2/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_W2
	!	$ DELETE PR_MAIN_EMP_W2.OBJ;*
	!
	! Author:
	!
	!	01/11/2001 - Kevin Handy
	!
	! Modification history:
	!
	!	01/16/2000 - Kevin Handy
	!		Added field for Social Security Number
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.HB"
	MAP (PR_EMP_W2) PR_EMP_W2_CDD PR_EMP_W2
	MAP (PR_EMP_W22) PR_EMP_W2_CDD PR_EMP_W2_OLD, PR_EMP_W22

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
		PR_EMP_W2.CH%, &
		PR_EMP_W2.READONLY%

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
		SMG_WINDOW::DESCR = "Payroll Employee W2 Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_W2"
		SMG_WINDOW::CHAN  = PR_EMP_W2.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 21%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 0%
		SMG_WINDOW::LTITLE(0%) = "First Page"

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Employee-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

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

		FOR I% = 0% TO 10%
			PR_EMP_W2::EARNINGS(I%) = 0.0
			PR_EMP_W2::EARNINGS_CODE(I%) = ""
			PR_EMP_W2::TAXES(I%) = 0.0
			PR_EMP_W2::TAXABLE(I%) = 0.0
			PR_EMP_W2::TAXES_CODE(I%) = ""
			PR_EMP_W2::TAXES_STATE(I%) = ""
			PR_EMP_W2::TAXES_ID(I%) = ""
			PR_EMP_W2::DEDUCTIONS(I%) = 0.0
			PR_EMP_W2::DEDUCTIONS_CODE(I%) = ""
		NEXT I%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_W2.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_W2.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_W2 = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_W2.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.OPN"
		USE
			PR_MAIN_EMP_W2 = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_W2.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_W2.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_W2.CH%

		WHEN ERROR IN
			RESET #PR_EMP_W2.CH%
			GET #PR_EMP_W2.CH%, REGARDLESS
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

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1, 1,	"(01) Emp #", &
			2, 1,	"(02) Name", &
			4, 1,	"(03) Address", &
			5, 1,	"(04) Address", &
			6, 1,	"(05) City", &
			7, 1,	"(06) State", &
			8, 1,	"(07) Zip code", &
			9, 1,	"(08) Country", &
			10, 1,	"(09) Location", &
			12, 1, "(10) Federal WH", &
			12,30, "(11) Federal Pay", &
			13, 1, "(12) Fica(HI) Tax", &
			13,30, "(13) Fica Pay", &
			14, 1, "(14) Fica(OASSDU) Tax", &
			14,30, "(15) Fica Pay", &
			15, 1, "(16) State Tax", &
			15,30, "(17) State Pay", &
			15,60, "(18) State Cd", &
			4,40, "(19) Fed ID #", &
			5,40, "(20) State ID#", &
			6,40, "(21) SocSecNum", &
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
	!	The ^*Employee _#\* field will accommodate up to ten (10) alphanumeric
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

			PR_EMP_W2::EMPNUM  = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;15", TEMP$, &
				PR_EMP_W2::EMPNUM, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.ts 55
	!	^*(02) Name	30 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field will
	!	accommodate up to thirty (30) characters. The employee name
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

			PR_EMP_W2::EMPNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;15", TEMP$, &
				PR_EMP_W2::EMPNAME, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Address	20 Characters\*
	!	.b
	!	.lm +5
	!	The first ^*Address\* line will accommodate up to twenty (20)
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

			PR_EMP_W2::ADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;15", TEMP$, &
				PR_EMP_W2::ADD1, MFLAG, "'E", MVALUE)

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

			PR_EMP_W2::ADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;15", TEMP$, &
				PR_EMP_W2::ADD2, MFLAG, "'E", MVALUE)

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

			PR_EMP_W2::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;15", TEMP$, &
				PR_EMP_W2::CITY, MFLAG, &
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

			PR_EMP_W2::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;15", TEMP$, &
				PR_EMP_W2::STATE, MFLAG, "'E", MVALUE)

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

			PR_EMP_W2::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;15", TEMP$, &
				PR_EMP_W2::ZIP, MFLAG, "'E", MVALUE)

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

			PR_EMP_W2::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;15", TEMP$, &
				LEFT$(PR_EMP_W2::COUNTRY, 2%), MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					LEFT$(PR_EMP_W2::COUNTRY, 2%)) = 1%
				THEN
					PR_EMP_W2::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO E0Loop
			END IF

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field has a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;16", TEMP$, &
				PR_EMP_W2::LOCATION, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX ") = 1%
				THEN
					PR_EMP_W2::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO E0Loop
			END IF


		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES(0%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;16", TEMP$, &
				PR_EMP_W2::TAXES(0%), MFLAG, &
				"######.##", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXABLE(0%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;46", TEMP$, &
				PR_EMP_W2::TAXABLE(0%), MFLAG, &
				"######.##", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(10) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES(1%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;16", TEMP$, &
				PR_EMP_W2::TAXES(1%), MFLAG, &
				"######.##", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXABLE(1%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;46", TEMP$, &
				PR_EMP_W2::TAXABLE(1%), MFLAG, &
				"######.##", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(10) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES(2%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;16", TEMP$, &
				PR_EMP_W2::TAXES(2%), MFLAG, &
				"######.##", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(13) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXABLE(2%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;46", TEMP$, &
				PR_EMP_W2::TAXABLE(2%), MFLAG, &
				"######.##", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(10) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES(3%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;16", TEMP$, &
				PR_EMP_W2::TAXES(3%), MFLAG, &
				"######.##", MVALUE)

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(13) Fed WH\*
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXABLE(3%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;46", TEMP$, &
				PR_EMP_W2::TAXABLE(3%), MFLAG, &
				"######.##", MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field has a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES_STATE(3%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;75", TEMP$, &
				PR_EMP_W2::TAXES_STATE(3%), MFLAG, &
				"'E", MVALUE)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field has a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES_ID(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;55", TEMP$, &
				PR_EMP_W2::TAXES_ID(0%), MFLAG, &
				"'E", MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field has a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::TAXES_ID(3%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;55", TEMP$, &
				PR_EMP_W2::TAXES_ID(3%), MFLAG, &
				"'E", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(27) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters a physical location designation for an employee of up to four
	!	(4) characters. The field may be blank.
	!	.p
	!	This field has a default value during time entry
	!	routines. The default may be overwritten.
	!
	! Index:
	!	.x Employee>Location
	!	.x Location>Employee
	!
	!--

			PR_EMP_W2::SSN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;55", TEMP$, &
				PR_EMP_W2::SSN, MFLAG, &
				"'E", MVALUE)

		END SELECT

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_EMP_W2 = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_EMP_W2::EMPNUM = ""
			THEN
				PR_MAIN_EMP_W2 = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank Employee # not allowed", 1%)
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #PR_EMP_W2.CH%, &
							KEY #0% EQ PR_EMP_W2::EMPNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					PR_MAIN_EMP_W2 = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				END IF
			END IF

		CASE 9%
			IF PR_EMP_W2::LOCATION <> ""
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_EMP_W2 = FUNC_TESTENTRY(SMG_WINDOW, &
					PR_EMP_W2::LOCATION, &
					UTL_LOCATION::LOCNAME, &
					"PR", MLOOP, "LOC", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

20390		END SELECT

	!
	! Set PR_EMP_W2_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_W2_OLD = PR_EMP_W2

	!
	! Restore PR_EMP_W2_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_W2 = PR_EMP_W2_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_W22 = PR_EMP_W2

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_W2 = PR_EMP_W22

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
			MVALUE = PR_EMP_W2::EMPNUM + " " + &
				LEFT(PR_EMP_W2::EMPNAME, 20%) + " " + &
				LEFT(PR_EMP_W2::SSN, 13%) + " " + &
				PR_EMP_W2::LOCATION

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_W2.CH%, &
				KEY #0% GE PR_EMP_W2::EMPNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #PR_EMP_W2.CH%, &
				KEY #1% GE PR_EMP_W2::SSN + "", &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
