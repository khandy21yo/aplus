1	%TITLE "County Tax Profile Maintenance"
	%SBTTL "PR_MAIN_TAX_PROFILE_D"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TAX_PROFILE_D(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Abstract:HELP
	!	.p
	!	The ^*County Tax Profile Maitenance\* program maintains the County Tax profile
	!	file.
	!
	! Index:
	!	.x County Tax Profile Maintenance
	!	.x Maintenance>County Tax Profile
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TAX_PROFILE_D/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_TAX_PROFILE_D
	!	$ DELETE PR_MAIN_TAX_PROFILE_D.OBJ;*
	!
	! Author:
	!
	!	09/17/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/23/92 - Dan Perkins
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
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	11/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_D.HB"
	MAP (PR_TAX_PROFILE_F)		PR_TAX_PROFILE_D_CDD	PR_TAX_PROFILE_D
	MAP (PR_TAX_PROFILE_D_OLD) PR_TAX_PROFILE_D_CDD PR_TAX_PROFILE_D_OLD, &
		PR_TAX_PROFILE_D2

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TAX_PROFILE_F) &
		PR_TAX_PROFILE.CH%, &
		PR_TAX_PROFILE.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_TAX_PROFILE_D) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "County Tax Profile"
		SMG_WINDOW::NHELP = "PR_MAIN_TAX_PROFILE_D"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 18%

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
		IF PR_TAX_PROFILE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TAX_PROFILE.READONLY%
			GOTO 790
		END IF

		FLAGOPEN% = (PR_TAX_PROFILE.CH% = 0%)

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TAX_PROFILE_D = ERR
			CONTINUE 770
		END WHEN

		PR_TAX_PROFILE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
		USE
			PR_MAIN_TAX_PROFILE_D = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TAX_PROFILE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		IF FLAGOPEN%
		THEN
			ST% = LIB$FREE_LUN(PR_TAX_PROFILE.CH%)
			PR_TAX_PROFILE.CH% = 0%
		ELSE
			PR_TAX_PROFILE.CH% = -PR_TAX_PROFILE.CH%
		END IF

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TAX_PROFILE.CH%
		WHEN ERROR IN
			RESET #PR_TAX_PROFILE.CH%
			GET #PR_TAX_PROFILE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"(01)        (02)                (03)         " + &
			"                                 ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Code Tax Id number        County Withholding " + &
			"Liability Account                ", &
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
		FOR I% = 1% TO 3%
			A% = VAL%(MID("005,026", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
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
	!	^*(01) Code\*
	!	.p
	!	The ^*Code\* field
	!	enters a two (2) character code designating the specific
	!	county to which the record refers.
	!
	! Index:
	!	.x County Payroll Tax Profile>Code
	!	.x County Code>County Payroll Tax Profile
	!
	!--

			PR_TAX_PROFILE_D::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_TAX_PROFILE_D::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Tax Id Number\*
	!	.p
	!	The ^*Tax Identification Number\* field
	!	enters the County Withholding
	!	Tax identification number for the specific county to which the
	!	record relates.
	!
	! Index:
	!	.x County Payroll Tax Profile>Tax Identification Number
	!	.x Tax Identification Number>County Payroll Tax Profile
	!
	!--

			PR_TAX_PROFILE_D::REPNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";6", TEMP$, &
				PR_TAX_PROFILE_D::REPNO, MFLAG, &
				"'LLLLLLLLLLLLL", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) County Withholding Liability Account\*
	!	.p
	!	The ^*County Withholding Liability Account\* field
	!	enters the General
	!	Ledger account which will be credited with the amount of County
	!	Withholding Taxes withheld from employees.
	!
	! Index:
	!	.x County Payroll Tax Profile>Withholding Liability Account
	!	.x County Withholding Liability Account>County Payroll Tax Profile
	!
	!--

			PR_TAX_PROFILE_D::COU_LIA_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE1$ + ";27", TEMP$, &
				PR_TAX_PROFILE_D::COU_LIA_ACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_D::COU_LIA_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TAX_PROFILE_D = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Don't allow blank account numbers
			!
			IF PR_TAX_PROFILE_D::CODE = ""
			THEN
				PR_MAIN_TAX_PROFILE_D = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal code", 1%)
				EXIT FUNCTION
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			PR_MAIN_TAX_PROFILE_D = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_TAX_PROFILE_D::COU_LIA_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set PR_TAX_PROFILE_D_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TAX_PROFILE_D_OLD = PR_TAX_PROFILE_D

	!
	! Restore PR_TAX_PROFILE_D_OLD value
	!
	CASE OPT_RESETOLD
		PR_TAX_PROFILE_D = PR_TAX_PROFILE_D_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TAX_PROFILE_D2 = PR_TAX_PROFILE_D

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TAX_PROFILE_D = PR_TAX_PROFILE_D2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_TAX_PROFILE_D::AUTH = "D"

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_TAX_PROFILE.CH%, &
				KEY #0% GE PR_TAX_PROFILE_D::AUTH + "", &
				REGARDLESS
		END SELECT

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
					KEY #0% GE "D", &
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

			IF PR_TAX_PROFILE_D::AUTH = "D"
			THEN
				!
				! Add information to array
				!
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
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_TAX_PROFILE_D::AUTH = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
