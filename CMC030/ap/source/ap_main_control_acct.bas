1	%TITLE "Control Line File Maintenance"
	%SBTTL "AP_MAIN_CONTROL_ACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_CONTROL_ACCT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1995 BY
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
	! supported bySoftware Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program maintains the control file accounts.
	!	.b
	!	One place these accounts are used is in the AP
	!	to GL comparison report.
	!	.lm -5
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_CONTROL_ACCT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_CONTROL_ACCT
	!	$ DELETE AP_MAIN_CONTROL_ACCT.OBJ;*
	!
	! Author:
	!
	!	11/02/95 - Kevin Handy
	!		Based upon AR_MAIN_CONTROL_ACCOUNT
	!
	! Modification history:
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/29/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD and Map statements
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.HB"
	MAP (AP_CONTROL_ACCOUNT)		AP_CONTROL_ACCOUNT_CDD	AP_CONTROL_ACCOUNT
	MAP (AP_CONTROL_ACCOUNT_OLD)	AP_CONTROL_ACCOUNT_CDD	AP_CONTROL_ACCOUNT_OLD, AP_CONTROL_ACCOUNT2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_CONTROL_ACCOUNT) &
		AP_CONTROL_ACCOUNT.CH%, &
		AP_CONTROL_ACCOUNT.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	COM (TT_AP_CONTROL_ACCOUNT) RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Valid GL Accounts"
		SMG_WINDOW::NHELP = "AP_MAIN_CONTROL_ACCT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 1%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 18%
		SMG_WINDOW::LINREC = 1%

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
		IF AP_CONTROL_ACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_CONTROL_ACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_CONTROL_ACCT = ERR
			CONTINUE 770
		END WHEN

		AP_CONTROL_ACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.OPN"
		USE
			AP_MAIN_CONTROL_ACCT  = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_CONTROL_ACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_CONTROL_ACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AP_CONTROL_ACCOUNT.CH%
		WHEN ERROR IN
			RESET #AP_CONTROL_ACCOUNT.CH%
			GET #AP_CONTROL_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"   (01)" + SPACE$(71%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Account             Description" + SPACE$(45%), &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 1%

			A% = VAL%(MID("021", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Account Number\* field enters the General
	!	Ledger Chart of Accounts number to which this record pertains.
	!	.b
	!	Valid General Ledger numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Account Number>Accounts Receivable Control File Maintain
	!	.x Accounts Receivable Control File Maintain>Account Number
	!
	!--

			AP_CONTROL_ACCOUNT::ACCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AP_CONTROL_ACCOUNT::ACCOUNT, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_CONTROL_ACCOUNT::ACCOUNT = &
					GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				GOTO E0Loop
			END IF

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AP_CONTROL_ACCOUNT::ACCOUNT) = 1%
			THEN
				CON_DESCR$ = GL_CHART::DESCR
			ELSE
				CON_DESCR$ = STRING$(20%, A"?"B)
			END IF

			CON_DESCR$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";23", TEMP$, &
				CON_DESCR$, 1%, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_CONTROL_ACCT = 0%

		CASE 1%
			!
			! Is the input defined?
			!
			AP_MAIN_CONTROL_ACCT = FUNC_TESTENTRY( SMG_WINDOW, &
				AP_CONTROL_ACCOUNT::ACCOUNT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account", GL_MAIN_CHART.ID)

	!
	! Set AP_CONTROL_ACCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		AP_CONTROL_ACCOUNT_OLD = AP_CONTROL_ACCOUNT

	!
	! Restore AP_CONTROL_ACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		AP_CONTROL_ACCOUNT = AP_CONTROL_ACCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_CONTROL_ACCOUNT2 = AP_CONTROL_ACCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_CONTROL_ACCOUNT = AP_CONTROL_ACCOUNT2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				RESET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			!
			! Add information to array
			!
			SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
			RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
				GETRFA(SMG_WINDOW::CHAN)
			GOTO 27120

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
