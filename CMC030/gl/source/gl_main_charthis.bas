1	%TITLE "CHART OF ACCOUNTS HISTORY MAINTENANCE"
	%SBTTL "GL_MAIN_CHARTHIS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_CHARTHIS(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:1015
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The [Chart of] ^*Accounts History Maintenance\* program
	!	maintains historical dollar, unit and hour balances on each
	!	General Ledger account for 21 immediately preceeding periods.
	!	Information in this file is maintained automatically by the
	!	system and, except during possible initialization procedures,
	!	^*manual changes to records in this file should not be considered\*.
	!	.b
	!	In addition to the 21 period historical data, this program
	!	maintains running dollars, running units, running hours,
	!	current dollars, current units and current hours fields. The
	!	running total fields indicate current balances in real (balance
	!	sheet) accounts. Running total fields in nominal account
	!	records will always contain zeros. The current total fields
	!	display net transaction amounts which have occurred during the
	!	current period.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account History
	!	.x Account History>General Ledger
	!	.x Account History>Change
	!	.x Change>Account History
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_CHARTHIS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_CHARTHIS
	!	$ DELETE GL_MAIN_CHARTHIS.OBJ;*
	!
	! Author:
	!
	!	04/27/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/10/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	11/01/91 - Kevin Handy
	!		Modified to display current period for each record
	!		to give us someplace to look for errors.
	!
	!	11/06/91 - Kevin Handy
	!		Modified to allow entry of +/- 999,999,999.99
	!		instead of +99,999,999.99 and -9,999,999.99
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/06/99 - Kevin Handy
	!		Add period number to display of year
	!		Some cleanup of formatting
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART
	MAP	(GL_CHART2)	GL_CHART_CDD	GL_CHART_OLD, GL_CHART2

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%
	COM (CH_GL_PERIOD) &
		GL_PERIOD.CH%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

	!
	! Dimensions
	!
	DIM GLPERIOD$(23%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!****************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!****************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Chart of Accounts History Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_CHARTHIS"
		SMG_WINDOW::CHAN  = GL_CHART.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%

		SMG_WINDOW::NITEMS= 70%
		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 40%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 70%

		SMG_WINDOW::FLAGS = 4%		! Edit only (No add).

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Account"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults for Chart of Accounts
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Get record from Control File
		!
		GOSUB 25000

700		!
		! Declare channels
		!
		IF GL_CHART.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_CHART.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_CHARTHIS = ERR
			CONTINUE 770
		END WHEN

		GL_CHART.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
		USE
			GL_MAIN_CHARTHIS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_CHART.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_CHART.CH%)
		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_CHART.CH%
		WHEN ERROR IN
			RESET #GL_CHART.CH%
			GET #GL_CHART.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!****************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!****************************************************************
	CASE OPT_BACKGROUND

		SELECT MLOOP

		!
		! Main screen
		!
		CASE 0%
			SMG_STATUS% = &
				SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	1,  1, "(01) Account", &
				1, 37, "Descr:", &
				2, 37, "Perd:", &
				4,  2, "Fiscal", &
				4, 33, "Dollars", &
				4, 52, "Units", &
				4, 69, "Hours", &
				0,  0, ""

			RESTORE

			READ XPOS, YPOS, XSTR$
			I% = 0%
			WHILE (XPOS <> 0%)
				I% = I% + 1%
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
				READ XPOS, YPOS, XSTR$
			NEXT

			GLPERIOD$(1%) = "Running total"
			GLPERIOD$(2%) = "Current total"

			LASTPERCLO% = GL_PERIOD::LASTPERCLO
			YEAR$ = GL_PERIOD::YEAR
			FPFY% = GL_PERIOD::FPFY

			FOR LOOP% = 3% TO 23%
				GLPERIOD$(LOOP%) = YEAR$ + "_" + &
					FORMAT$(LASTPERCLO%, "<0>#") + " " + &
					GL_PERIOD::PERIOD(LASTPERCLO%)
				LASTPERCLO% = LASTPERCLO% - 1%
				IF LASTPERCLO% < 1%
				THEN
					LASTPERCLO% = FPFY%
					YEAR$ = FORMAT$(VAL%(YEAR$) - &
						1%, "<0>###")
				END IF
			NEXT LOOP%

			FOR I% = 1% TO 13%
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TRM$(GLPERIOD$(I%)), &
					I% + 5%, 2%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(I% + 1%, "(<0>#)"), &
					I% + 5%, 24%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(I% + 14%, "(<0>#)"), &
					I% + 5%, 41%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(I% + 27%, "(<0>#)"), &
					I% + 5%, 58%)
			NEXT I%

			SMG_STATUS% = &
				SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! 1st page
		!
		CASE 1%
			SMG_STATUS% = &
				SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = &
				SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = &
				SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				" Fiscal" + SPACE$(25%) + &
				"Dollars            " + &
				"Units            " + &
				"Hours", &
				1%, 1%)
			GLPERIOD$(1%) = "Running total"
			GLPERIOD$(2%) = "Current total"

			LASTPERCLO% = GL_PERIOD::LASTPERCLO
			YEAR$ = GL_PERIOD::YEAR
			FPFY% = GL_PERIOD::FPFY

			FOR LOOP% = 3% TO 23%
				GLPERIOD$(LOOP%) = YEAR$ + "_" + &
					FORMAT$(LASTPERCLO%, "<0>#") + " " + &
					GL_PERIOD::PERIOD(LASTPERCLO%)
				LASTPERCLO% = LASTPERCLO% - 1%
				IF LASTPERCLO% < 1%
				THEN
					LASTPERCLO% = FPFY%
					YEAR$ = FORMAT$(VAL%(YEAR$) - &
						1%, "<0>###")
				END IF
			NEXT LOOP%

			FOR I% = 14% TO 23%
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					TRM$(GLPERIOD$(I%)), &
					I% - 11%, 2%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					FORMAT$(I% + 27%, "(<0>#)"), &
					I% - 11%, 24%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					FORMAT$(I% + 37%, "(<0>#)"), &
					I% - 11%, 41%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					FORMAT$(I% + 47%, "(<0>#)"), &
					I% - 11%, 58%)
			NEXT I%

			SMG_STATUS% = &
				SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

		END SELECT

		IF GL_PERIOD::CLOSEFLAG = "1"
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"GL close in process.  Data is unreliable!", 0%)
		END IF

		IF GL_PERIOD::CLOSEFLAG = "2"
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"GL reset in process.  Data is unreliable!", 0%)
		END IF

20200	!****************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!****************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field refers to the accounts recorded in the
	!	Chart of Accounts file.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Chart History>Account Number
	!
	!--

			IF INSTR(1%, "!Change!Blank!Initialize!Default!", &
				TEMP$) AND MFLAG <> 1%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Field cannot be accessed with this command", 0%)

				SCOPE::SCOPE_EXIT = 0%
			ELSE
				GL_CHART::ACCT = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, &
					"1;15", TEMP$, &
					GL_CHART::ACCT, MFLAG, "'E", MVALUE)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					GL_CHART::DESCR, &
					1%, 44%)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					NUM1$(GL_CHART::CPERIOD), &
					2%, 44%)
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Running Dollars\*
	!	.b
	!	.lm +5
	!	The ^*Running Dollars\* field (02) is used to store the current
	!	balance of a real (balance sheet) account number. A nominal
	!	account record will always display zeros. This field is
	!	automatically updated whenever the user posts to the General
	!	Ledger.
	!	.b
	!	.lm +5
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.lm -5
	!	.b
	!	^*Note: Manual changes to this field should not be
	!	considered.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::RUNDOL = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;28", TEMP$, &
				GL_CHART::RUNDOL, MFLAG, &
				"#########.##-", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Current Dollars\*
	!	.b
	!	.lm +5
	!	The ^*Current Dollars\* field is used to store the sum of
	!	transactions which have been posted to an account during the
	!	current period or accounting cycle. This field is accessed by
	!	the working financial report routine in order to create a flash
	!	financial report. This field is automatically updated whenever
	!	the user posts to the General Ledger.
	!	.lm +5
	!	.b
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.b
	!	.lm -5
	!	^*Note: Manual changes to this field should not be
	!	considered.\*
	!
	! Index:
	!
	!--

			GL_CHART::CURDOL = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;28", TEMP$, &
				GL_CHART::CURDOL, MFLAG, &
				"#########.##-", MVALUE)

		CASE 4% TO 14%
			SCOPE::PRG_ITEM = "FLD004DOLL"
	!++
	! Abstract:FLD004DOLL
	!	^*(04)-(14) _& (41)-(50) Dollars History\*
	!	.b
	!	.lm +5
	!	The ^*Dollars History\* fields are used to store the
	!	balance of dollars in a General Ledger account for prior
	!	accounting periods. These fields are automatically updated
	!	whenever a user closes the General Ledger.
	!	.lm +5
	!	.b
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.b
	!	.lm -5
	!	^*Note: Except for initialization procedures, manual changes to these
	!	fields should not be considered.\*
	!
	! Index:
	!	.x Chart History>Dollars History
	!	.x Dollars History>Chart History
	!
	!--
			GL_CHART::DOLLAR(MLOOP - 4%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP + 4%) + ";28", TEMP$, &
				GL_CHART::DOLLAR(MLOOP - 4%), MFLAG, &
				"#########.##-", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Running Units\*
	!	.b
	!	.lm +5
	!	The ^*Running Units\* field is used to store the current
	!	balance of a real (balance sheet) account number. Nominal
	!	account records will always display zeros. This field is
	!	automatically updated whenever the user posts to the General
	!	Ledger.
	!	.lm +5
	!	.b
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.lm -5
	!	.b
	!	^*Note: Manual changes to this field should not be
	!	considered.\*
	!	.lm -5
	!
	! Index:
	!	.x Chart History>Running Units
	!	.x Running Units>Chart History
	!
	!--

			GL_CHART::RUNUNIT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;46", TEMP$, &
				GL_CHART::RUNUNIT, MFLAG, &
				"########.##-", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Current Units\*
	!	.b
	!	.lm +5
	!	The ^*Current Units\* field is used to store the sum of transactions
	!	which have been posted to a real (balance sheet) account during the
	!	current period. This field is accessed by the working financial report
	!	routine in order to create a flash financial report whenever "units"
	!	are an integral part of the financial reports. This field is
	!	automatically updated whenever a user posts to the General Ledger.
	!	.b
	!	.lm +5
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.b
	!	.lm -5
	!	^*Note: Manual changes to this field should not be considered.\*
	!
	! Index:
	!
	!--

			GL_CHART::CURUNIT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;46", TEMP$, &
				GL_CHART::CURUNIT, MFLAG, &
				"########.##-", MVALUE)

		CASE 17% TO 27%
			SCOPE::PRG_ITEM = "FLD017UNIT"

	!++
	! Abstract:FLD017UNIT
	!	^*(17)-(27) _&_ (51)-(60) Units History\*
	!	.b
	!	.lm +5
	!	The ^*Units History\* fields are used to store the balance
	!	of units in a General Ledger account for prior accounting periods.
	!	These fields are automatically updated whenever a user closes the
	!	General Ledger.
	!	.b
	!	.lm +5
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.lm -5
	!	.b
	!	^*Note: Except for initialization procedures, manual changes
	!	to these fields should not be considered.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::UNIT(MLOOP - 17%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP - 9%) + ";46", TEMP$, &
				GL_CHART::UNIT(MLOOP - 17%), MFLAG, &
				"########.##-", MVALUE)

		CASE 28%

	!++
	! Abstract:FLD028
	!	^*(28) Running Hours\*
	!	.b
	!	.lm +5
	!	The ^*Running Hours\* field is used to store the current
	!	balance of a real (balance sheet) account number. Nominal
	!	account records will always display zeros. This field is
	!	automatically updated whenever the user posts to the General
	!	Ledger.
	!	.lm +5
	!	.b
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.lm -5
	!	.b
	!	^*Note: Manual change in this field should not be
	!	considered.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::RUNHOUR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;63", TEMP$, &
				GL_CHART::RUNHOUR, MFLAG, &
				"########.##-", MVALUE)

		CASE 29%

	!++
	! Abstract:FLD029
	!	^*(29) Current Hours\*
	!	.b
	!	.lm +5
	!	The ^*Current Hours\* field is used to store the sum of
	!	transactions which have been posted to a real (balance sheet)
	!	account during the current period. This field is accessed by
	!	the working financial report routine in order to create a flash
	!	financial report whenever "hours" are an integral part of the
	!	financial reports. This field is automatically updated whenever
	!	a user posts to the General Ledger.
	!	.lm +5
	!	.b
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.b
	!	.lm -5
	!	^*Note: Manual changes to this field should not be considered.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::CURHOUR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;63", TEMP$, &
				GL_CHART::CURHOUR, MFLAG, &
				"########.##-", MVALUE)

		CASE 30% TO 40%
			SCOPE::PRG_ITEM = "FLD030HOUR"

	!++
	! Abstract:FLD030HOUR
	!	^*(30)-(40) _&_ (61)-(70) Hours History\*
	!	.b
	!	.lm +5
	!	The ^*Hours History\* fields are used to store the balance of
	!	hours in a General Ledger account for prior accounting periods.
	!	These fields are automatically updated whenever a user closes the
	!	General Ledger.
	!	.b
	!	.lm +5
	!	^*-\* represents a credit
	!	.b
	!	^*+\* represents a debit
	!	.b
	!	.lm -5
	!	^*Note: Except for initialization procedures, manual changes
	!	to these fields should not be considered.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::HOUR(MLOOP - 30%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP - 22%) + ";63", TEMP$, &
				GL_CHART::HOUR(MLOOP - 30%), MFLAG, &
				"########.##-", MVALUE)


		CASE 41% TO 50%
			SCOPE::PRG_ITEM = "FLD004DOLL"

			GL_CHART::DOLLAR(MLOOP - 30%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				NUM1$(MLOOP - 38%) + ";28", TEMP$, &
				GL_CHART::DOLLAR(MLOOP - 30%), MFLAG, &
				"#########.##-", MVALUE)


		CASE 51% TO 60%
			SCOPE::PRG_ITEM = "FLD017UNIT"

			GL_CHART::UNIT(MLOOP - 40%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				NUM1$(MLOOP - 48%) + ";46", TEMP$, &
				GL_CHART::UNIT(MLOOP - 40%), MFLAG, &
				"########.##-", MVALUE)

		CASE 61% TO 70%
			SCOPE::PRG_ITEM = "FLD030HOUR"

			GL_CHART::HOUR(MLOOP - 50%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				NUM1$(MLOOP - 58%) + ";63", TEMP$, &
				GL_CHART::HOUR(MLOOP - 50%), MFLAG, &
				"########.##-", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20500	!****************************************************************
	! Set GL_CHART_OLD value
	!****************************************************************
	CASE OPT_SETOLD
		GL_CHART_OLD = GL_CHART

	!****************************************************************
	! Restore GL_CHART_OLD value
	!****************************************************************
	CASE OPT_RESETOLD
		GL_CHART = GL_CHART_OLD

	!****************************************************************
	! Set default value
	!****************************************************************
	CASE OPT_SETDEFAULT
		GL_CHART2 = GL_CHART

	!****************************************************************
	! Restore default value
	!****************************************************************
	CASE OPT_RESETDEFAULT
		GL_CHART = GL_CHART2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Account            Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_CHART::ACCT + " " + &
				GL_CHART::DESCR

		END SELECT

	!****************************************************************
	! Find
	!****************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account number)
			!
			FIND #GL_CHART.CH%, KEY #0% GE GL_CHART::ACCT + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

25000	!****************************************************************
	! Subroutine to get record from Control file
	!****************************************************************
	GOTO 25020 IF GL_PERIOD.CH% > 0%

	CALL ASSG_CHANNEL(GL_PERIOD.CH%, STAT%)
	IF STAT%
	THEN
		GL_MAIN_CHARTHIS = 1%
		GOTO ExitFunction
	END IF

	!
	! Open up period file, and grab record
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

25010	GET #GL_PERIOD.CH%, RECORD 1%

	CLOSE GL_PERIOD.CH%

25020	RETURN

	%PAGE

29000	!****************************************************************
	! Trap errors
	!****************************************************************
	ON ERROR GO BACK

32767	!****************************************************************
	! End of GL_MAIN_CHARTHIS function
	!****************************************************************
	END FUNCTION
