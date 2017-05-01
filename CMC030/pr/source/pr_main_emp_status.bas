1	%TITLE "Employee Status Maintenance"
	%SBTTL "PR_MAIN_EMP_STATUS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_STATUS(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Status\* function
	!	accesses the Employee Status file where
	!	an employee's marital status and number of exemptions are maintained.
	!	.p
	!	A record is maintained for any jurisdiction to which an employee's
	!	earnings may be subject to withholding taxes, including the possibility
	!	of multiple state and local jurisdictions.
	!	.p
	!	The status and number of exemptions need not be equal for all
	!	jurisdictions.
	!
	! Index:
	!	.x Marital Status
	!	.x Employee>Marital Status
	!	.x Employee>Tax Exemptions
	!	.x Tax Exemptions
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_STATUS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_STATUS
	!	$ DELETE PR_MAIN_EMP_STATUS.OBJ;*
	!
	! Author:
	!
	!	09/17/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI Tax package.
	!
	!	10/03/89 - Kevin Handy
	!		Put the include for MAIN_WINDOW.COM back in so
	!		the function would work properly.
	!
	!	12/12/90 - Kevin Handy
	!		Added secondary exemption field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)		PR_EMP_STATUS_CDD	PR_EMP_STATUS
	MAP (PR_EMP_STATUS_OLD) PR_EMP_STATUS_CDD PR_EMP_STATUS_OLD, &
		PR_EMP_STATUS2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_STATUS) RARRAY_RECORD RARRAY(300%)

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_STATUS) &
		PR_EMP_STATUS.CH%, &
		PR_EMP_STATUS.READONLY%

	COM (TT_EMP_STATUS) &
		STTITLE$ = 25%, &
		ST$(10%) = 25% &

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
		SMG_WINDOW::DESCR = "Employee status"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_STATUS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 2%
		SMG_WINDOW::BOTLIN = 14%

		!
		! Withholding types
		!
		STTITLE$ = "Type Description" &

		ST$(0%) = "8"
		ST$(1%) = "FW   Federal Withholding"
		ST$(2%) = "SW   State Withholding"
		ST$(3%) = "SU   State Unemployment"
		ST$(4%) = "SX   OST"
		ST$(5%) = "CW   City Withholding"
		ST$(6%) = "DW   County Witholding"
		ST$(7%) = "EW   School Witholding"
		ST$(8%) = "SI   WC Liability Ins."

		!
		! Load in defaults for Status
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_STATUS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_STATUS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_STATUS = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_STATUS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
		USE
			PR_MAIN_EMP_STATUS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_STATUS.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_STATUS.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_STATUS.CH%
		WHEN ERROR IN
			RESET #PR_EMP_STATUS.CH%
			GET #PR_EMP_STATUS.CH%, REGARDLESS
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
			"  (01) Type   " + &
			"(02) Code                " + &
			"(03) Status (04) Exempt    (05) Exempt1", &
			1%, 1%, , SMG$M_REVERSE)

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
			A% = VAL%(MID("014,039,051,066", I% * 4% - 3%, 3%))

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
	!	^*(01) Type\*
	!	.p
	!	The ^*Type\* column refers to the specific tax
	!	to which earnings may be subject and any consequential taxes withheld
	!	from the employee's earnings. Valid Types are:
	!	.b
	!	.lm +5
	!	.LIST 0,"*"
	!	.LE
	!	FW = Federal Withholding Tax
	!	.LE
	!	SW = State Withholding Tax
	!	.LE
	!	CW = City Withholding Tax
	!	.LE
	!	DW = County Withholding Tax
	!	.LE
	!	EW = School District Withholding Tax
	!	.ELS
	!	.lm -5
	!
	! Index:
	!	.x Federal Withholding>Employee Status
	!	.x State Withholding>Employee Status
	!	.x Other State>Employee Status
	!	.x City Withholding>Employee Status
	!	.x County Withholding>Employee Status
	!	.x School District Withholding>Employee Status
	!	.x Tax Type>Employee Status
	!	.x Employee Status>Tax Type
	!
	!--

			PR_EMP_STATUS::STTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";9", TEMP$, &
				PR_EMP_STATUS::STTYPE, MFLAG, "'E", MVALUE, &
				ST$(), STTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Code\*
	!	.p
	!	The ^*Code\* column provides a two
	!	character code field with which to identify the specific State, city,
	!	county or school district for corresponding tax types. Codes must be
	!	equal to the codes established in the Tax Table file. State codes are
	!	equal to the State Post Office codes. Local tax codes must be equal
	!	to the codes for the related jurisdictions as they exist in the Tax
	!	Tables. The code field for Federal Withholding Tax type would be left
	!	blank.
	!	.p
	!	Multiple State and local tax jurisdictions may be entered for a
	!	single employee.
	!
	! Index:
	!	.x Employee Status>Code
	!	.x Code>Employee Status
	!
	!--

			PR_EMP_STATUS::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";21", TEMP$, &
				PR_EMP_STATUS::CODE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Status\*
	!	.p
	!	The ^*Status\* column
	!	records the marital status of the employee as it relates to each tax
	!	type. The status need not be equal for each type or jurisdiction.
	!	Standard status codes are:
	!	.lm +10
	!	.b
	!	.LIST "*"
	!	.LE
	!	M = Married
	!	.LE
	!	S = Single
	!	.LE
	!	E = Exempt
	!	.ELS
	!	.lm -10
	!	.p
	!	Specialized status codes are:
	!	.b
	!	.lm +10
	!	.LIST "*"
	!	.LE
	!	1 = Married, only one spouse with income
	!	.LE
	!	2 = Married, both spouses with income or
	!	married, claiming two or more exemptions,
	!	depending upon specific Tax Tables
	!	.LE
	!	H = Head of household
	!	.ELS
	!	.lm -10
	!	.p
	!	.NOTE CAUTION
	!	Before using any of the specialized status codes, it
	!	is imperative that the user make certain that a supporting
	!	table exists in the Tax Table for the specific jurisdiction
	!	for which a specialized status code is to be used.
	!	.end note
	!	.NOTE
	!	The exempt (*E) code causes no taxes to be calculated, but
	!	will still create a record in the deduction file with a
	!	reportable amount.
	!	The (*E) code means that he is exempt from having taxes calculated,
	!	not that he is exempt from paying the tax.
	!	.end note
	!
	! Index:
	!	.x Status>Employee Status
	!	.x Employee Status>Marital Status
	!	.x Marital Status>Employee Status
	!
	!--

			PR_EMP_STATUS::STSTATUS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";47", TEMP$, &
				PR_EMP_STATUS::STSTATUS, MFLAG, "'", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Exempt\*
	!	.p
	!	The ^*Exempt\* column
	!	records the number of tax exemptions claimed by an employee
	!	as recorded on a W-4 form or other acceptable form. The number of
	!	exemptions need not necessarily be equal for all tax types.
	!
	! Index:
	!	.x Exemptions>Employee Status
	!	.x Employee Status>Exemptions
	!
	!--

			PR_EMP_STATUS::EXEMPT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";57", TEMP$, &
				PR_EMP_STATUS::EXEMPT * 1.0, &
				MFLAG, "###", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Exempt 1\*
	!	.p
	!	The ^*Exempt 1\* column
	!	records the number of tax exemptions claimed by an employee
	!	as recorded on a W-4 form or other acceptable form. The number of
	!	exemptions need not necessarily be equal for all tax types.
	!	.p
	!	This secondary number of exemptions is for use by those
	!	states/localities that require two different exemption
	!	numbers.
	!
	! Index:
	!	.x Exemptions>Employee Status
	!	.x Employee Status>Exemptions
	!
	!--

			PR_EMP_STATUS::ADDEXEMPT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";69", TEMP$, &
				PR_EMP_STATUS::ADDEXEMPT * 1.0, &
				MFLAG, "###", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_STATUS = 0%

	!
	! Set PR_EMP_STATUS_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_STATUS_OLD = PR_EMP_STATUS

	!
	! Restore PR_EMP_STATUS_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_STATUS = PR_EMP_STATUS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_STATUS2 = PR_EMP_STATUS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_STATUS = PR_EMP_STATUS2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_STATUS::EMPNUM = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_STATUS.CH%, &
				KEY #0% GE PR_EMP_STATUS::EMPNUM + "", &
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
					KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
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

			IF PR_EMP_STATUS::EMPNUM = PR_EMP_MASTER::EMPNUM
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
			PR_EMP_STATUS::EMPNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
