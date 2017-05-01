1	%TITLE "Maintain List of Tax Fringe Cross Ref Accounts"
	%SBTTL "PR_MAIN_TAX_PROFILE_FRI"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TAX_PROFILE_FRI(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
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
	!	The ^*Cross Reference Account\* program maintains the Tax Profile Fringe
	!	definition file.
	!
	! Index:
	!	.x Maintenance>Tax Profile Fringe Definition
	!	.x Tax Profile Fringe Definition Maintenance
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TAX_PROFILE_FRI/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TAX_PROFILE_FRI
	!	$ DELETE PR_MAIN_TAX_PROFILE_FRI.OBJ;*
	!
	! Author:
	!
	!	03/20/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/01/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.HB"
	MAP (PR_TAX_PROFILE_FRI)	PR_TAX_PROFILE_FRI_CDD	PR_TAX_PROFILE_FRI
	MAP (PR_TAX_PROFILE_FRI_OLD)	PR_TAX_PROFILE_FRI_CDD	PR_TAX_PROFILE_FRI_OLD, PR_TAX_PROFILE_FRI2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TAX_PROFILE_FRI) &
		PR_TAX_PROFILE_FRI.CH%, &
		PR_TAX_PROFILE_FRI.READONLY%, &
		TAXTITLE$ = 25%, &
		TAXTYPE$(3%) = 25%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	PRINE = 3	! Line number
	END RECORD

	MAP (TT_PR_TAX_PROFILE_FRI) RARRAY_RECORD RARRAY(500%)	! Allocate for 300

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Tax Fringe Distribution"
		SMG_WINDOW::NHELP = "PR_MAIN_TAX_PROFILE_FRI"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 17%

		!
		! Tax Type table
		!
		TAXTITLE$ = "Type Description"
		TAXTYPE$(0%) = "3"
		TAXTYPE$(1%) = "FI FICA"
		TAXTYPE$(2%) = "FU Federal Unemployment"
		TAXTYPE$(3%) = "SU State Unemployment"

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
		IF PR_TAX_PROFILE_FRI.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TAX_PROFILE_FRI.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TAX_PROFILE_FRI = ERR
			CONTINUE 770
		END WHEN

		PR_TAX_PROFILE_FRI.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.OPN"
		USE
			PR_MAIN_TAX_PROFILE_FRI = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TAX_PROFILE_FRI.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TAX_PROFILE_FRI.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TAX_PROFILE_FRI.CH%
		WHEN ERROR IN
			RESET #PR_TAX_PROFILE_FRI.CH%
			GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
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
			"  (01) (02)                (03)" + SPACE$(52%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Type Labor Account       Fringe Ex Acct" + SPACE$(42%), &
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
			FORMAT$(SMG_WINDOW::TOTREC, "###") + SPACE$(59%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 2%

			A% = VAL%(MID("007,027", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Type\*
	!	.p
	!	The ^*Type\* field enters the type of tax used in the
	!	distribution.
	!	Valid types inclde:
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*FI\* FICA
	!	.le
	!	^*FU\* Federal Unemployement
	!	.le
	!	^*SU\* State Unemployement
	!	.le
	!	^*SX\* State Unemployment
	!	.els
	!
	! Index:
	!	.x Type>Fringe Distribution
	!	.x Fringe Distribution>Type
	!
	!--

			PR_TAX_PROFILE_FRI::TAX_TYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";4", TEMP$, &
				PR_TAX_PROFILE_FRI::TAX_TYPE, MFLAG, "'E", &
				MVALUE, &
				TAXTYPE$(), TAXTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Labor Account\*
	!	.p
	!	The ^*Labor Account\* field enters the account number
	!	used for the labor.
	!
	! Index:
	!	.x Labor Account>Fringe Distribution
	!	.x Fringe Distribution>Labor Account
	!
	!--

			PR_TAX_PROFILE_FRI::LABOR_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";8", TEMP$, &
				PR_TAX_PROFILE_FRI::LABOR_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PR_TAX_PROFILE_FRI::LABOR_ACCT = &
						GL_CHART::ACCT
				END IF

				GOTO E0Loop
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Fringe Expense Account\*
	!	.p
	!	The ^*Fringe Expense Account\* enters the Account
	!	number charged for the Fringe Benefits.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Fringe Distribution>Expense Account
	!	.x Expense Account>Fringe Distribution
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_FRI::FRI_EX_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";28", TEMP$, &
				PR_TAX_PROFILE_FRI::FRI_EX_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PR_TAX_PROFILE_FRI::FRI_EX_ACCT = &
						GL_CHART::ACCT
				END IF

				GOTO E0Loop
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TAX_PROFILE_FRI = 0%

		SELECT MLOOP

		CASE 2%, 3%
			ACCT_TEMP$ = PR_TAX_PROFILE_FRI::LABOR_ACCT
			ACCT_TEMP$ = PR_TAX_PROFILE_FRI::FRI_EX_ACCT &
				IF MLOOP = 3%

			IF INSTR(1%, ACCT_TEMP$, "?") = 0%
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_TAX_PROFILE_FRI = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					ACCT_TEMP$, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		END SELECT

	!
	! Set PR_TAX_PROFILE_FRI_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TAX_PROFILE_FRI_OLD = PR_TAX_PROFILE_FRI

	!
	! Restore PR_TAX_PROFILE_FRI_OLD value
	!
	CASE OPT_RESETOLD
		PR_TAX_PROFILE_FRI = PR_TAX_PROFILE_FRI_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TAX_PROFILE_FRI2 = PR_TAX_PROFILE_FRI

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TAX_PROFILE_FRI = PR_TAX_PROFILE_FRI2

		!*
		!*  Set special default values for the key of a new record
		!*    which is the most likely case when this function is
		!*    to be called.
		!*

	!
	! Find
	!
	CASE OPT_FIND

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
