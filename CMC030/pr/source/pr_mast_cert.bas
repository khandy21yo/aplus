1	%TITLE "Payroll Certificate of Minimum Wage Maintenance"
	%SBTTL "PR_MAST_CERT"
	%IDENT "V3.6a Calico"

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
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Certificate of Minimum Wage Maintenance\* option
	!	enters the minimum wage and the date the wage
	!	becomes effective.
	!
	! Index:
	!	.x Certificate Minimum Wage>Maintain
	!	.x Maintain>Certificate Minimum Wage
	!
	! Option:
	!
	!	PR_MAIN_CERT$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_CERT
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_MAST_CERT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_CERT.OBJ;*
	!
	! Author:
	!
	!	11/24/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/23/96 - Kevin Handy
	!		Clean up (Check)
	!		Create PR_WINDOW id for CERT_MIN_WAGE file
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

400	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_CERT_MIN_WAGE.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.HB"
	MAP (PR_CERT_MIN_WAGE)	PR_CERT_MIN_WAGE_CDD	PR_CERT_MIN_WAGE
	MAP (PR_CERT_MIN_WAGE2)	PR_CERT_MIN_WAGE_CDD	PR_CERT_MIN_WAGE_OLD, &
		PR_CERT_MIN_WAGE2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_PR_CERT_MIN_WAGE) &
		PR_CERT_MIN_WAGE.CH%, &
		PR_CERT_MIN_WAGE.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PR_MAIN_CERT_MIN_WAGE.ID

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
			SMG_WINDOW::DESCR = "PR Certificate of Min Wage Maintenance"
			SMG_WINDOW::NHELP = "PR_MAST_CERT"
			SMG_WINDOW::HSIZE = 78%
			SMG_WINDOW::VSIZE = 18%
			SMG_WINDOW::HVIEW = 78%
			SMG_WINDOW::VVIEW = 18%
			SMG_WINDOW::HPOS  = 2%
			SMG_WINDOW::VPOS  = 2%
			SMG_WINDOW::NITEMS= 2%
			SMG_WINDOW::FLAGS = 0%

			SMG_WINDOW::NKEYS = 1%
			SMG_WINDOW::KNAME(0%) = "Effective_date"
				SMG_WINDOW::KFIELD(0%, 0%) = 1%
				SMG_WINDOW::KFIELD(0%, 1%) = 1%

			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW) &
				IF INSTR(1%, " QV", MVALUE) <= 1%

20700			!
			! Declare channels
			!
			IF PR_CERT_MIN_WAGE.CH% > 0%
			THEN
				!
				! Already open, set flag to read-only if was
				! that way from last time.
				!
				SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
					IF PR_CERT_MIN_WAGE.READONLY%
				GOTO 20790
			END IF

			!
			! Open main file (existing) for modification
			!
20750			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.CRE"
			USE
				CONTINUE 20760 IF ERR = 10%
				MAINT_GROUP = ERR
				CONTINUE 20770
			END WHEN

			PR_CERT_MIN_WAGE.READONLY% = 0%
			GOTO 20790

20760			!
			! If unable to open for modify, try to open with read
			! access only.
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.OPN"
			USE
				MAINT_GROUP = ERR
				CONTINUE 20770
			END WHEN

			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
			PR_CERT_MIN_WAGE.READONLY% = -1%

			GOTO 20790

20770			!
			! File not open, so reset channel
			!
			CALL ASSG_FREECHANNEL(PR_CERT_MIN_WAGE.CH%)

			EXIT FUNCTION

20790			SMG_WINDOW::CHAN  = PR_CERT_MIN_WAGE.CH%
			WHEN ERROR IN
				RESET #PR_CERT_MIN_WAGE.CH%
				GET #PR_CERT_MIN_WAGE.CH%, REGARDLESS
			USE
				CONTINUE 32767
			END WHEN

		!
		! Display the background
		!
		! This option is used to display the background information
		! on the screen.  It must first clear any junk on the screen,
		! and then write the background onto it.
		!
		CASE OPT_BACKGROUND

			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	05, 05, "(01) Effective Date", &
				06, 05, "(02) Rate", &
				0,  0, ""

			RESTORE

			READ XPOS%, YPOS%, XSTR$

			WHILE (XPOS% <> 0%)
				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER,XSTR$, XPOS%, YPOS%)
				READ XPOS%, YPOS%, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Enter/Display/Default
		!
		! This option is used to enter the data from the user,
		! display data, set defaults, and return the data back
		! according to MFLAG.
		!
		CASE OPT_ENTRY

			TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
			TEMP$ = "View starting at" IF TEMP$ = "View"

			SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter1:		SCOPE::SCOPE_EXIT = 0%

			SELECT MLOOP

			CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Effective Date\*
	!	.p
	!	The ^*Effective Date\* field defines the date the minimum
	!	wage goes into effective.  The format for entry in MMDDYY or MMDDYYYY.
	!
	! Index:
	!	.x Effective Date
	!	.x Dateÿ>Effective
	!
	!--
				PR_CERT_MIN_WAGE::EFF_DATE = ENTR_3DATE(SCOPE, &
					SMG_WINDOW::WNUMBER, "05;30", TEMP$, &
					PR_CERT_MIN_WAGE::EFF_DATE, MFLAG, &
					"8", MVALUE)

			CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Rate\*
	!	.p
	!	The ^*Rate\* field defines the minimum rate which becomes
	!	effective on the specified date.
	!
	! Index:
	!	.x Rate
	!
	!--
				PR_CERT_MIN_WAGE::RATE = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, "06;33", TEMP$, &
					PR_CERT_MIN_WAGE::RATE * 1.0, MFLAG, &
					"####.##", MVALUE)

			END SELECT

			SCOPE::PRG_ITEM = TEMP1$

		!
		! Test values
		!
		CASE OPT_TESTENTRY
			MAINT_GROUP = 0%

		CASE OPT_DISPLAY

		!
		! Set PR_CERT_MIN_WAGE_OLD value
		!
		CASE OPT_SETOLD
			PR_CERT_MIN_WAGE_OLD = PR_CERT_MIN_WAGE

		!
		! Restore PR_CERT_MIN_WAGE_OLD value
		!
		CASE OPT_RESETOLD
			PR_CERT_MIN_WAGE = PR_CERT_MIN_WAGE_OLD

		!
		! Set default value
		!
		CASE OPT_SETDEFAULT
			PR_CERT_MIN_WAGE2 = PR_CERT_MIN_WAGE

		!
		! Restore default value
		!
		CASE OPT_RESETDEFAULT
			PR_CERT_MIN_WAGE = PR_CERT_MIN_WAGE2

		!
		! View header
		!
		CASE OPT_VIEW
			SELECT MLOOP

			!
			! Title (One line only)
			!
			CASE 1%
				MVALUE = "  Effective Date               Rate"

			!
			! Positions of lines
			!
			CASE 2%
				MVALUE = "019"

			!
			! Convert current record into text
			!
			CASE 3%
				MVALUE = "  " + &
					PRNT_DATE(PR_CERT_MIN_WAGE::EFF_DATE, 8%) + &
					"              " + &
					FORMAT$(PR_CERT_MIN_WAGE::RATE, &
					"####.##")
			END SELECT
		!
		! Find
		!
		CASE OPT_FIND
			SELECT MLOOP
			CASE 0%
				FIND #PR_CERT_MIN_WAGE.CH%, KEY #0% &
					GE PR_CERT_MIN_WAGE::EFF_DATE, &
					REGARDLESS
			END SELECT

		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	FILENAME$ = ""
	RESUME ExitFunction

32767	END FUNCTION
