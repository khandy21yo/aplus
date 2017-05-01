1	%TITLE "GENERAL LEDGER CONTROL FILE MAINTENANCE"
	%SBTTL "GL_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! ID:1017
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*CONTROL\* file defines the number
	!	of periods in a fiscal year and describe those periods. The
	!	last period closed is automatically recorded in this file.
	!	The current year, the current budget year, the summary total,
	!	and the summary account are also recorded in this file.
	!	.lm -5
	!
	! Index:
	!	.X Change>Control File
	!	.x Control File>Change
	!	.x Control File>General Ledger
	!	.x General Ledger>Control File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_CONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_CONTROL
	!	$ DELETE GL_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/01/88 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	07/15/91 - Kevin Handy
	!		Moved line 27000 into its correct position.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
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
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD
	MAP	(GL_PERIOD2)	GL_PERIOD_CDD	GL_PERIOD_OLD, GL_PERIOD2


	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_PERIOD) &
		GL_PERIOD.CH%
	COM (TT_GL_PERIOD) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(3%) = 20%, &
		GL_PERIOD.READONLY%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

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
		SMG_WINDOW::DESCR = "Period Definition Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 33%
		SMG_WINDOW::FLAGS = 128%	! Relative file

		SMG_WINDOW::NKEYS = 0%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Define Closing Type
		!
		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "3"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Closing"
		CLOSETYPE$(3%) = "2    Resetting"

700		!
		! Declare channels
		!
		IF GL_PERIOD.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_PERIOD.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		GL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		USE
			GL_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_PERIOD.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_PERIOD.CH%
		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit"

20100	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "Period Description           End Date", &
			2,  1, "                               (MMDD)", &
			3, 39, "(27) Last period closed", &
			4, 39, "(28) Periods per year", &
			6, 39, "(29) Current Year", &
			7, 39, "(30) New Year", &
			9, 39, "(31) Summary Acct", &
			10, 39, "(32) Summary Total", &
			12, 39, "(33) Close Flag", &
			0,  0, ""

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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"(" + FORMAT$(I%, "<0>#") + ")", &
			I% + 2%, 2%) &
			FOR I% = 1% TO 13%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"(" + FORMAT$(I% + 13%, "<0>#") + ")", &
			I% + 2%, 29%) &
			FOR I% = 1% TO 13%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user,
	! display data, set defaults, and return the data back
	! according to MFLAG.
	!
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1% TO 13%
			SCOPE::PRG_ITEM = "FLD001DESC"
	!++
	! Abstract:FLD001DESC
	!	.x Description>Period
	!	^*(01-13) Period Description\*
	!	.b
	!	.lm +5
	!	The ^*(01-13) Period Description\* fields are used to
	!	define the accounting periods (or operating cycles) of a
	!	company.
	!	.lm -5
	!
	! Index:
	!	.x Period>Description
	!	.x Control>Period>Description
	!	.x Control>Description>Period
	!
	!--
			GL_PERIOD::PERIOD(MLOOP)= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, NUM1$(MLOOP + 2%) + ";8", &
				TEMP$, GL_PERIOD::PERIOD(MLOOP), MFLAG, &
				"'E", MVALUE)

		CASE 14% TO 26%
			SCOPE::PRG_ITEM = "FLD014DATE"
	!++
	! Abstract:FLD014DATE
	!	.x Period>Date
	!	^*(14-26) Last Date in Period\*
	!	.b
	!	.lm +5
	!	The ^*(14-26) Last Date in Period\* fields are used to define
	!	the last date in an accounting period so the posting process
	!	can check dates to determine if the journal is being posted
	!	into the correct accounting period.
	!	.b
	!	The format for entry would be MMDD.
	!	.lm -5
	!
	! Index:
	!	.x Date>Period
	!	.x Control>Period>Date
	!	.x Control>Date>Period
	!
	!--
			GL_PERIOD::ENDDATE(MLOOP - 13%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP - 11%) + ";34", &
				TEMP$, GL_PERIOD::ENDDATE(MLOOP - 13%), &
				MFLAG, "'E", MVALUE)

		CASE 27%

	!++
	! Abstract:FLD027
	!	.x Period>Closed
	!	^*(27) Last Period Closed\*
	!	.b
	!	.lm +5
	!	The ^*Last Period Closed\* field represents the last
	!	period the General Ledger was closed. This field is
	!	automatically updated whenever the General Ledger is
	!	closed.
	!	.b
	!	^*Note: Except for possible initialization procedures,
	!	changes to this field should not
	!	be considered.\*
	!	.lm -5
	!
	! Index:
	!	.x Closed>Period
	!	.x Control>Period>Closed
	!	.x Control>Closed>Period
	!	.x Last>Period
	!	.x Period>Last
	!	.x Control>Last>Period
	!	.x Control>Period>Last
	!
	!--
			GL_PERIOD::LASTPERCLO = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;66", TEMP$, &
				GL_PERIOD::LASTPERCLO * 1.0, MFLAG, "##", &
				MVALUE)

		CASE 28%

	!++
	! Abstract:FLD028
	!	.x Period>Per Year
	!	^*(28) Periods Per Year\*
	!	.b
	!	.lm +5
	!	The ^*Periods Per Year\* defines how many periods there
	!	will be in a fiscal year. There must be more than 1
	!	accounting period per year.
	!	.lm -5
	!
	! Index:
	!	.x Period>Year
	!	.x Year>Period
	!	.x Control>Period>Year
	!	.x Control>Year>Period
	!	.x Control>Period>Per Year
	!	.x Control>Per Year>Period
	!
	!--
			GL_PERIOD::FPFY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;66", TEMP$, &
				GL_PERIOD::FPFY * 1.0, MFLAG, "##", MVALUE)

		CASE 29%

	!++
	! Abstract:FLD029
	!	.x Current>Year
	!	^*(29) Current Year\*
	!	.b
	!	.lm +5
	!	The ^*Current Year\* field defines which fiscal year
	!	is the current fiscal year.
	!	.b
	!	The format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!	.x Year>Current
	!	.x Control>Current>Year
	!	.x Control>Year>Current
	!	.x Control>Fiscal>Year
	!	.x Control>Year>Fiscal
	!	.x Year>Fiscal
	!	.x Fiscal>Year
	!
	!--
			GL_PERIOD::YEAR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;66", TEMP$, &
				GL_PERIOD::YEAR, MFLAG, "'E", MVALUE)

		CASE 30%

	!++
	! Abstract:FLD030
	!	^*(30) New Year\*
	!	.b
	!	.lm +5
	!	A ^*New Year\* must occur each January, regardless of when a
	!	fiscal year change occurs. The purpose of this field is to
	!	flag the accounting cycle in which January occurs. For
	!	example, if the user has twelve accounting periods with the
	!	fiscal year beginning in April, then the calendar year
	!	change would occur at the tenth period. The user would
	!	indicate the calendar change to occur in January by
	!	entering "10".
	!	.b
	!	Note: Notice that you are not specifying the month here.
	!	You are specifying which period, as listed in (01) to (13),
	!	that the calendar year starts.
	!	If field (07) is January, then this field should be set to 7.
	!	.lm -5
	!
	! Index:
	!	.x New Year
	!	.x Control>New Year
	!	.x Year>New Year
	!	.x Control>Year>New Year
	!	.x Fiscal>New Year
	!	.x New Year>Fiscal
	!	.x Year>Fiscal
	!	.x Period>New Year
	!
	!--
			GL_PERIOD::NEWYEAR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;66", TEMP$, &
				GL_PERIOD::NEWYEAR * 1.0, MFLAG, "##", MVALUE)

		CASE 31%

	!++
	! Abstract:FLD031
	!	.x Summary>Account
	!	^*(31) Summary Account\*
	!	.b
	!	.lm +5
	!	The ^*Summary Account\* field is the General Ledger
	!	account number to which the nominal accounts will automatically
	!	be closed at the end of the fiscal year. This number is
	!	automatically assigned by the system based on the "S" type code
	!	entered in the Chart of Accounts.
	!	.b
	!	This field is for informational uses only. It is not used by the
	!	system to determine which account to summarize to. That is done
	!	by the "S" type code. This field is automatically filled in by
	!	the ledger close process.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Account>Summary
	!	.x Control>Summary>Account
	!	.x Control>Account>Summary
	!
	!--
			GL_PERIOD::SUMMARYACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;66", TEMP$, &
				GL_PERIOD::SUMMARYACCT, MFLAG, "'E", MVALUE)

		CASE 32%

	!++
	! Abstract:FLD032
	!	^*(32) Summary Total\*
	!	.b
	!	.lm +5
	!	The ^*Summary Total\* field is the summary total of
	!	all nominal accounts in the General Ledger for the last fiscal
	!	year closed.
	!	.b
	!	^*Note: The system will automatically enter this amount.
	!	The user would never be required to enter or change this field.\*
	!
	! Index:
	!	.x Summary>Total
	!	.x Total>Summary
	!	.x Control>Summary>Total
	!	.x Control>Total>Summary
	!	.x Nominal>Summary
	!	.x Nominal>Total
	!
	!--
			GL_PERIOD::SUMMARYTOTAL = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;66", TEMP$, &
				GL_PERIOD::SUMMARYTOTAL, MFLAG, "#########.##", MVALUE)

		CASE 33%

	!++
	! Abstract:FLD033
	!	^*(33) Close Flag\*
	!	.b
	!	.lm +5
	!	The ^*Close Flag\* is used to signal the system if a G/L
	!	close or reset is in process. If a close is in process, the
	!	flag is set to '1'. If a reset is in process the flag is set
	!	to '2'. The user can press ^*<List Choices>\* to view all of
	!	the options available when changing this field.
	!	.lm -5
	!
	! Index:
	!	.x Close>Flag
	!	.x Flag>Close
	!	.x Control>Close>Flag
	!	.x Control>Flag>Close
	!
	!--
			GL_PERIOD::CLOSEFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;66", TEMP$, GL_PERIOD::CLOSEFLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$


20300	!**********************************************************
	! Test values
	!**********************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 27%
			IF GL_PERIOD::LASTPERCLO > GL_PERIOD::FPFY
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Must be less than periods per year", 0%)
				GL_MAIN_CONTROL = 1%
				GL_PERIOD::LASTPERCLO = GL_PERIOD_OLD::LASTPERCLO
			END IF

		CASE 28%
			IF GL_PERIOD::FPFY < 2% OR GL_PERIOD::FPFY > 13%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Must be between 2 and 13", 0%)
				GL_MAIN_CONTROL = 1%
				GL_PERIOD::FPFY = GL_PERIOD_OLD::FPFY
			END IF

			IF GL_PERIOD::FPFY > 0% AND GL_PERIOD::FPFY < 14%
			THEN
				GL_PERIOD::PERIOD(LOOP%) = "** NOT USED **" &
					FOR LOOP% = GL_PERIOD::FPFY + 1% TO 13%

				GL_PERIOD::ENDDATE(LOOP%) = "****" &
					FOR LOOP% = GL_PERIOD::FPFY + 1% TO 13%
			END IF

		END SELECT

20500	!**********************************************************
	! Set GL_PERIOD_OLD value
	!**********************************************************
	CASE OPT_SETOLD
		GL_PERIOD_OLD = GL_PERIOD

	!**********************************************************
	! Restore GL_PERIOD_OLD value
	!**********************************************************
	CASE OPT_RESETOLD
		GL_PERIOD = GL_PERIOD_OLD

	!**********************************************************
	! Set default value
	!**********************************************************
	CASE OPT_SETDEFAULT
		GL_PERIOD2 = GL_PERIOD

	!**********************************************************
	! Restore default value
	!**********************************************************
	CASE OPT_RESETDEFAULT
		GL_PERIOD = GL_PERIOD2

	END SELECT

27000	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for Period file
	!
	GL_PERIOD::PERIOD(I%) = "Period " + NUM1$(I%) &
		FOR I% = 1% TO 13%
	GL_PERIOD::ENDDATE(I%) = "" &
		FOR I% = 1% TO 13%
	GL_PERIOD::LASTPERCLO	= 1%
	GL_PERIOD::FPFY		= 12%
	GL_PERIOD::YEAR		= DATE_TODAY
	GL_PERIOD::SUMMARYTOTAL	= 0.0
	GL_PERIOD::SUMMARYACCT	= ""
	GL_PERIOD::BTHNUM	= ""
	GL_PERIOD::CLOSEFLAG	= "0"
	GL_PERIOD::NEWYEAR	= 1%
	PUT #GL_PERIOD.CH%

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
