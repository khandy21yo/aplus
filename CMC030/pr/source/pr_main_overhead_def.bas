1	%TITLE "Overhead Definition Maintenance"
	%SBTTL "PR_MAIN_OVERHEAD_DEF"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_OVERHEAD_DEF(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program maintains the Overhead Definition file.
	!	.lm -5
	!
	! Index:
	!	.x Overhead Definition Maintenance
	!	.x Maintenance>Overhead Definition
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_OVERHEAD_DEF/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_OVERHEAD_DEF
	!	$ DELETE PR_MAIN_OVERHEAD_DEF.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP	(PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF
	MAP (PR_OVERHEAD_DEF_OLD) PR_OVERHEAD_DEF_CDD PR_OVERHEAD_DEF_OLD, &
		PR_OVERHEAD_DEF2

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP	(PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_OVERHEAD_DEF) RARRAY_RECORD RARRAY(1500%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_OVERHEAD_DEF) &
		PR_OVERHEAD_DEF.CH%, &
		PR_OVERHEAD_DEF.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Overhead Definition"
		SMG_WINDOW::NHELP = "PR_MAIN_OVERHEAD_DEF"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 13%
		SMG_WINDOW::LINREC = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_OVERHEAD_DEF.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_OVERHEAD_DEF.READONLY%
			GOTO 790
		END IF

		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_OVERHEAD_DEF = ERR
			CONTINUE 770
		END WHEN

		PR_OVERHEAD_DEF.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.OPN"
		USE
			PR_MAIN_OVERHEAD_DEF = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_OVERHEAD_DEF.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_OVERHEAD_DEF.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_OVERHEAD_DEF.CH%
		WHEN ERROR IN
			RESET #PR_OVERHEAD_DEF.CH%
			GET #PR_OVERHEAD_DEF.CH%, REGARDLESS
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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)                  (02)                  " + &
			"                                 ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Subject Account       Operation             " + &
			"                                 ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 1%
			A% = VAL%(MID("021", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Subject Account\*
	!	.b
	!	.lm +5
	!	The ^*Subject Account\* field
	!	enters a General Ledger account or accounts which relate(s)
	!	to a specific Overhead Key.
	!	.lm -5
	!
	! Index:
	!	.x Subject Account>Overhead Table
	!	.x Overhead>Table>Subject Account
	!
	!--

			PR_OVERHEAD_DEF::OVH_KEY = PR_OVERHEAD_DESC::OVH_KEY

			PR_OVERHEAD_DEF::SUBJ_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_OVERHEAD_DEF::SUBJ_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_OVERHEAD_DEF::SUBJ_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field
	!	enters an operation associated with a subject General Ledger account,
	!	both of which relate to a specific Overhead Key.
	!	^*
	!	.note
	!	A blank in the operation field will cause this overhead key to
	!	be used as the default if it cannot find a match using the operation.
	!	\*
	!	.end note
	!
	! Index:
	!	.x Operation>Overhead Table
	!	.x Overhead>Table>Operation
	!
	!--

			PR_OVERHEAD_DEF::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";25", TEMP$, &
				PR_OVERHEAD_DEF::OPER, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_OVERHEAD_DEF = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_MAIN_OVERHEAD_DEF = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_OVERHEAD_DEF::SUBJ_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set PR_OVERHEAD_DEF_OLD value
	!
20500	CASE OPT_SETOLD
		PR_OVERHEAD_DEF_OLD = PR_OVERHEAD_DEF

	!
	! Restore PR_OVERHEAD_DEF_OLD value
	!
	CASE OPT_RESETOLD
		PR_OVERHEAD_DEF = PR_OVERHEAD_DEF_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_OVERHEAD_DEF2 = PR_OVERHEAD_DEF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_OVERHEAD_DEF = PR_OVERHEAD_DEF2

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
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PR_OVERHEAD_DESC::OVH_KEY + "", &
					REGARDLESS
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
			IF (PR_OVERHEAD_DEF::OVH_KEY = &
				PR_OVERHEAD_DESC::OVH_KEY)
			THEN
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, &
				RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, &
				RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_OVERHEAD_DEF::OVH_KEY = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
