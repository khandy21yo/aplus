1	%TITLE "Control Line File Maintenance"
	%SBTTL "AR_MAIN_CONTROL_ACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CONTROL_ACCT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	!	.b
	!	.lm +5
	!	This program maintains the control file accounts.
	!	.b
	!	One place these accounts are used is in the AR to GL comparison report.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CONTROL_ACCT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_MAIN_CONTROL_ACCT
	!	$ DELETE AR_MAIN_CONTROL_ACCT.OBJ;*
	!
	! Author:
	!
	!	11/20/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/28/88 - Kevin Handy
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	02/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Use WHEN ERROR
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL_ACCT.HB"
	MAP (AR_CONTROL_ACCT)		AR_CONTROL_ACCT_CDD	AR_CONTROL_ACCT
	MAP (AR_CONTROL_ACCT_OLD)	AR_CONTROL_ACCT_CDD	AR_CONTROL_ACCT_OLD, AR_CONTROL_ACCT2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_CONTROL_ACCT) &
		AR_CONTROL_ACCT.CH%, &
		AR_CONTROL_ACCT.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	COM (TT_AR_CONTROL_ACCT) RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

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
		SMG_WINDOW::NHELP = "AR_MAIN_CONTROL_ACCT"
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
		IF AR_CONTROL_ACCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CONTROL_ACCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL_ACCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CONTROL_ACCT = ERR
			CONTINUE 770
		END WHEN

		AR_CONTROL_ACCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL_ACCT.OPN"
		USE
			AR_MAIN_CONTROL_ACCT  = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CONTROL_ACCT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CONTROL_ACCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CONTROL_ACCT.CH%

		WHEN ERROR IN
			RESET #AR_CONTROL_ACCT.CH%
			GET #AR_CONTROL_ACCT.CH%, REGARDLESS
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

			AR_CONTROL_ACCT::ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AR_CONTROL_ACCT::ACCT, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AR_CONTROL_ACCT::ACCT = &
					GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				GOTO E0Loop
			END IF

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + AR_CONTROL_ACCT::ACCT) = 1%
			THEN
				CON.DESCR$ = GL_CHART::DESCR
			ELSE
				CON.DESCR$ = STRING$(20%, A"?"B)
			END IF

			CON.DESCR$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";23", TEMP$, &
				CON.DESCR$, 1%, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_CONTROL_ACCT = 0%

		CASE 1%
			!
			! Is the input defined?
			!
			AR_MAIN_CONTROL_ACCT = FUNC_TESTENTRY( SMG_WINDOW, &
				AR_CONTROL_ACCT::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account", GL_MAIN_CHART.ID)

	!
	! Set AR_CONTROL_ACCT_OLD value
	!
20500	CASE OPT_SETOLD
		AR_CONTROL_ACCT_OLD = AR_CONTROL_ACCT

	!
	! Restore AR_CONTROL_ACCT_OLD value
	!
	CASE OPT_RESETOLD
		AR_CONTROL_ACCT = AR_CONTROL_ACCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_CONTROL_ACCT2 = AR_CONTROL_ACCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AR_CONTROL_ACCT = AR_CONTROL_ACCT2

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

32767	END FUNCTION
