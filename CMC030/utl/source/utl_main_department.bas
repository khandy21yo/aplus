1	%TITLE "Department Profile"
	%SBTTL "UTL_MAIN_DEPARTMENT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_DEPARTMENT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! ID:0135
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Department Profile\* program maintains the Department Profile file.
	!	.lm -5
	!
	! Index:
	!	.x Department Profile
	!	.x Profile>Department
	!
	! Option:
	!
	!
	! COMPILE:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_DEPARTMENT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_DEPARTMENT
	!	$ DELETE UTL_MAIN_DEPARTMENT.OBJ;*
	!
	! Author:
	!
	!	12/02/87 - Frank Starman
	!
	! Modification history:
	!
	!	05/24/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	02/04/89 - Frank Starman
	!		New layout
	!
	!	05/24/91 - Craig Tanner
	!		Added supervisor feild
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/12/97 - Kevin Handy
	!		Lose map for UTL_WORKCENTER.CH%.
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	06/15/98 - Kevin Handy
	!		Fixed setting the default value to use
	!		UTL_LOCATION::LOCATION instead of MVALUE.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT)	UTL_DEPARTMENT_CDD	UTL_DEPARTMENT
	MAP (UTL_DEPARTMENT_OLD) UTL_DEPARTMENT_CDD	UTL_DEPARTMENT_OLD
	MAP (UTL_DEPARTMENT_DEF) UTL_DEPARTMENT_CDD	UTL_DEPARTMENT_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_DEPARTMENT) &
		UTL_DEPARTMENT.CH%, &
		UTL_DEPARTMENT.READONLY%

	MAP (CH_UTL_LOCATION) &
		UTL_LOCATION.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_UTL_DEPARTMENT) RARRAY_RECORD RARRAY(1000%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Department Profile"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "UTL_MAIN_DEPARTMENT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 10%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Department"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 128%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::VHPOS = 2%
		SMG_WINDOW::VVPOS = 2%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF UTL_DEPARTMENT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_DEPARTMENT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_DEPARTMENT = ERR
			CONTINUE 770
		END WHEN

		UTL_DEPARTMENT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.OPN"
		USE
			UTL_MAIN_DEPARTMENT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_DEPARTMENT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_DEPARTMENT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_DEPARTMENT.CH%
		WHEN ERROR IN
			RESET #UTL_DEPARTMENT.CH%
			GET #UTL_DEPARTMENT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	02,05, "(01) Department #", &
			03,05, "(02) Dept Name", &
			04,05, "(03) Dept Group", &
			05,05, "(04) Phone", &
			06,05, "(05) Supervisor", &
			0, 0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Department _#\*
	!	.b
	!	.lm +5
	!	The ^*Department _#\* field enters a
	!	user defined number which will identify a particular
	!	department within the company.
	!	.b
	!	The field may contain six (6) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Department>Number
	!
	!--

			UTL_DEPARTMENT::DEPT_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$,	UTL_DEPARTMENT::DEPT_NUM, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Department Name\*
	!	.b
	!	.lm +5
	!	The ^*Department Name\* field enters a
	!	description of the Department _# entered in field (01).
	!	.b
	!	The field may contain twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Department>Name
	!	.x Name>Department
	!
	!--

			UTL_DEPARTMENT::DESCRIPTION= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, UTL_DEPARTMENT::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Department Group\*
	!	.b
	!	.lm +5
	!	The ^*Department Group\* field identifies a
	!	particular group within the department.
	!	.b
	!	The field may contain two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Department>Group
	!	.x Group>Department
	!
	!--

			UTL_DEPARTMENT::DEPGROUP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;25", TEMP$, UTL_DEPARTMENT::DEPGROUP, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field enters the phone number for
	!	this specific department of the company.
	!	.b
	!	The field may contain ten (10) numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Department>Phone
	!	.x Phone>Department
	!
	!--
			UTL_DEPARTMENT::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;25", TEMP$, UTL_DEPARTMENT::PHONE, MFLAG, &
				0%, MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Supervisor\*
	!	.b
	!	.lm +5
	!	The ^*Supervisor\* field enters the department
	!	supervisor of this specific department of the company.
	!	.b
	!	The field may contain thirty (30) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Department>Supervisor
	!	.x Supervisor>Department
	!
	!--
			UTL_DEPARTMENT::SUPERVISOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;25", TEMP$, UTL_DEPARTMENT::SUPERVISOR, &
				MFLAG, "'E", MVALUE)



		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_DEPARTMENT = 0%

	!
	! Set UTL_DEPARTMENT_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_DEPARTMENT_OLD = UTL_DEPARTMENT

	!
	! Restore UTL_DEPARTMENT_OLD value
	!
	CASE OPT_RESETOLD
		UTL_DEPARTMENT = UTL_DEPARTMENT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_DEPARTMENT_DEF = UTL_DEPARTMENT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_DEPARTMENT = UTL_DEPARTMENT_DEF
		UTL_DEPARTMENT::LOCATION = UTL_LOCATION::LOCATION

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Dept#  DepartmentName             " + &
				"              Group Phone         Supervisor"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,050,056,070"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_DEPARTMENT::DEPT_NUM + " " + &
				UTL_DEPARTMENT::DESCRIPTION+ " " + &
				UTL_DEPARTMENT::DEPGROUP + "    " + &
				PRNT_PHONE(UTL_DEPARTMENT::PHONE, 0%) + " " + &
				UTL_DEPARTMENT::SUPERVISOR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_DEPARTMENT.CH%, &
				KEY #0% GE UTL_DEPARTMENT::LOCATION + &
				UTL_DEPARTMENT::DEPT_NUM, REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF UTL_DEPARTMENT::LOCATION = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			UTL_DEPARTMENT::LOCATION = MVALUE

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More Menu option hidden in Mast.
	!++
	! Abstract:WORK_CENTER
	!	^*Work Center\*
	!	.b
	!	.lm +5
	!	The ^*Work Center\* option
	!	maintains the Work Center file.
	!	.lm -5
	!
	! Index:
	!
	!--
