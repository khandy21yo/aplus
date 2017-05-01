1	%TITLE "PR Union/Pension Definition Maintenance"
	%SBTTL "PR_MAIN_UNPN_DEF"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_UNPN_DEF(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the PR Union/Pension Definition file.
	!
	! Index:
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_UNPN_DEF/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_UNPN_DEF
	!	$ DELETE PR_MAIN_UNPN_DEF.OBJ;*
	!
	! Author:
	!
	!	12/11/87 - B. Craig Larsen
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
	!	12/08/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DEF.HB"
	MAP (PR_UNPN_DEF)	PR_UNPN_DEF_CDD	PR_UNPN_DEF
	MAP (PR_UNPN_DEF_OLD)	PR_UNPN_DEF_CDD	PR_UNPN_DEF_OLD, PR_UNPN_DEF2

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.HB"
	MAP (PR_UNPN_DESC)	PR_UNPN_DESC_CDD	PR_UNPN_DESC

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_UNPN_DEF) &
		PR_UNPN_DEF.CH%, &
		PR_UNPN_DEF.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_UNPN_DEF) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	PBTITLE$ = "Type Description"
	PB$(0%) = "4"
	PB$(1%) = "1  Paid by EmployER"
	PB$(2%) = "2  Paid by EmployEE"
	PB$(3%) = "3  Paid by Both"

	BBTITLE$ = "Type Description"
	BB$(0%) = "4"
	BB$(1%) = "1  Rate Per Hour"
	BB$(2%) = "2  % of Gross"
	BB$(3%) = "3  Withholding Flat Amt"

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Union/Pension Definition"
		SMG_WINDOW::NHELP = "PR_MAIN_UNPN_DEF"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::NITEMS= 8%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 4%
		SMG_WINDOW::BOTLIN = 12%
		SMG_WINDOW::LINREC = 2%

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
		IF PR_UNPN_DEF.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_UNPN_DEF.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DEF.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_UNPN_DEF = ERR
			CONTINUE 770
		END WHEN

		PR_UNPN_DEF.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DEF.OPN"
		USE
			PR_MAIN_UNPN_DEF = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_UNPN_DEF.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_UNPN_DEF.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_UNPN_DEF.CH%
		WHEN ERROR IN
			RESET #PR_UNPN_DEF.CH%
			GET #PR_UNPN_DEF.CH%, REGARDLESS
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
			"  01 02                   03    04       05     " + &
			"06                  07    08    ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Ty Description                                " + &
			"                                ", &
			2%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"                          PB  EE Rate  ER Rate " + &
			"Liability Account  Basis D Code ", &
			3%, 1%, , SMG$M_REVERSE)

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
		FOR I% = 1% TO 4%
			A% = VAL%(MID("038,047,066,072", &
				I% * 4% - 3%, 3%))

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
	!	^*(01) Type\*
	!	.p
	!	The Type field
	!	enters a two (2) character alphanumeric code which identifies
	!	a specific type of Union/Pension definition for which contributions or
	!	assessments will be made by a subject employee and/or the employer.
	!
	! Index:
	!	.x Pension>Type
	!	.x Union>Type
	!	.x Type>Union
	!	.x Type>Pension
	!
	!--

			PR_UNPN_DEF::CODE = PR_UNPN_DESC::CODE

			PR_UNPN_DEF::DTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_UNPN_DEF::DTYPE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a description
	!	of the definition type.
	!	.p
	!	The field will accommodate up to thirty (30) characters.
	!
	! Index:
	!	.x Union>Description
	!	.x Pension>Description
	!	.x Description>Union
	!	.x Description>Pension
	!
	!--

			PR_UNPN_DEF::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";6", TEMP$, &
				PR_UNPN_DEF::DESCR, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Paid By\*
	!	.p
	!	The ^*Paid By\* field
	!	indicates whether
	!	contributions are paid by the employer only, the employee only, or both.
	!	.p
	!	Valid codes are:
	!	.lm +5
	!	.ls 0
	!	.le
	!	- Paid by Employer
	!	.le
	!	- Paid by Employee
	!	.le
	!	- Paid by Both
	!	.els
	!
	! Index:
	!	.x Union>Paid By
	!	.x Pension>Paid By
	!	.x Paid By>Union
	!	.x Paid By>Pension
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::PAID_BY = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";27", TEMP$, &
				PR_UNPN_DEF::PAID_BY, MFLAG, "'", MVALUE, &
				PB$(), PBTITLE$, "005"), -1%)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Employee Rate\*
	!	.p
	!	The ^*Employee Rate\* field
	!	enters a rate for employee contributions in reference to
	!	a related union or pension fund.
	!
	! Index:
	!	.x Rate>Union
	!	.x Rate>Pension
	!	.x Union>Rate
	!	.x Pension>Rate
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::EMPE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";30", TEMP$, &
				PR_UNPN_DEF::EMPE_RATE, MFLAG, "#####.##", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Employer Rate\*
	!	.p
	!	The ^*Employer Rate\* field
	!	enters the rate for employer contributions in reference to
	!	a related union or pension fund.
	!
	! Index:
	!	.x Rate>Pension
	!	.x Rate>Union
	!	.x Pension>Rate
	!	.x Union>Rate
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::EMPR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";39", TEMP$, &
				PR_UNPN_DEF::EMPR_RATE, MFLAG, "#####.##", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Liability Account\*
	!	.p
	!	The ^*Liability Account\* field enters the General
	!	Ledger account which is credited with amounts withheld from employees
	!	pay and/or accruals in reference to the employer's contributions to a related
	!	union or pension fund.
	!	.p
	!	This field must have a value and the value must be valid.
	!
	! Index:
	!	.x Account>Union
	!	.x Account>Pension
	!	.x Union>Account
	!	.x Pension>Account
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::LIA_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";48", TEMP$, &
				PR_UNPN_DEF::LIA_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_UNPN_DEF::LIA_ACCT = GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Basis\*
	!	.p
	!	The ^*Basis\* field indicates
	!	the basis for the contribution to a related union or pension fund.
	!	.p
	!	This field must contain a valid value.
	!	.p
	!	Valid codes are:
	!	.lm +5
	!	.ls 0,"-"
	!	.le
	!	Rate per Hour
	!	.le
	!	% of Gross
	!	.le
	!	Flat Amount
	!	.els
	!
	! Index:
	!	.x Basis>Pension
	!	.x Basis>Union
	!	.x Pension>Basis
	!	.x Union>Basis
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::BASIS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";69", TEMP$, &
				PR_UNPN_DEF::BASIS, MFLAG, "'", MVALUE, &
				BB$(), BBTITLE$, "005"), -1%)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Deduction Code\*
	!	.p
	!	The ^*Deduction Code\* field
	!	enters a valid deduction code if contributions are to be
	!	deducted from an employee.
	!
	! Index:
	!	.x Code>Pension
	!	.x Code>Union
	!	.x Pension>Code
	!	.x Union>Code
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_UNPN_DEF::DED_CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";75", TEMP$, &
				PR_UNPN_DEF::DED_CODE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_UNPN_DEF = 0%

		SELECT MLOOP

		CASE 6%
			!
			! Is the input defined?
			!
			PR_MAIN_UNPN_DEF = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_UNPN_DEF::LIA_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set PR_UNPN_DEF_OLD value
	!
20500	CASE OPT_SETOLD
		PR_UNPN_DEF_OLD = PR_UNPN_DEF

	!
	! Restore PR_UNPN_DEF_OLD value
	!
	CASE OPT_RESETOLD
		PR_UNPN_DEF = PR_UNPN_DEF_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_UNPN_DEF2 = PR_UNPN_DEF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_UNPN_DEF = PR_UNPN_DEF2

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
					KEY #0% GE PR_UNPN_DESC::CODE + "", &
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
			IF (PR_UNPN_DEF::CODE = PR_UNPN_DESC::CODE)
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
			PR_UNPN_DEF::CODE = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
