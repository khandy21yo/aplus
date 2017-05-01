1	%TITLE "PR Workmans Comp Insurance Maintenance"
	%SBTTL "PR_MAIN_WC_INSURANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_WC_INSURANCE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*inSurance\* function
	!	accesses the Workmen's Compensation
	!	Table file where a record is maintained for each applicable Workman's
	!	Compensation code.
	!	.p
	!	The following data is maintained in each record in the file:
	!	.LIST "*"
	!	.LE
	!	State codes for each State where insurance liability
	!	could feasibly exist relative to a specific Code.
	!	.LE
	!	Effective dates for each premium rate or combination
	!	of premium rates.
	!	.LE
	!	Insurance rates for Workmen's Compensation, Property
	!	Liability and Personal Injury Liability insurances.
	!	.LE
	!	Flags to indicate whether overtime premium labor
	!	costs are subject to Workmen's Compensation, Property
	!	Liability or Personal Injury Liability insurance
	!	premium calculations in reference to each Code.
	!	.ELS
	!
	! Index:
	!	.x Insurance>Workmen's Compensation>Rates
	!	.x Insurance>Property Liability>Rates
	!	.x Insurance>Personal Injury>Rates
	!	.x Workmen's Compensation>Premium Rates
	!	.x Workmen's Compensation>Table>Insurance Function
	!	.x Property Liability Insurance>Premium Rates
	!	.x Personal Injury Liability Insurance>Premium Rates
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_WC_INSURANCE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_WC_INSURANCE
	!	$ DELETE PR_MAIN_WC_INSURANCE.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source
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
	!	12/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE
	MAP (PR_WC_INSURANCE_OLD) PR_WC_INSURANCE_CDD	PR_WC_INSURANCE_OLD, &
		PR_WC_INSURANCE2

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD	PR_WC_DESCR

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_WC_INSURANCE) &
		PR_WC_INSURANCE.CH%, &
		PR_WC_INSURANCE.READONLY%
	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	MAP (TT_PR_WC_INSURANCE) &
		RARRAY_RECORD RARRAY(300%)

	COM (TT_PR_WC_INS_METHOD) &
		MTDTITLE$ = 34%, &
		MTD$(5%) = 34%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Workman's Comp Insurance"
		SMG_WINDOW::NHELP = "PR_MAIN_WC_INSURANCE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 13%
		SMG_WINDOW::LINREC = 1%

		!
		! Methods
		!
		MTDTITLE$ = "Method Description"

		MTD$(0%) = "5"
		MTD$(1%) = "1      Per Dollar (Prem Not Subj)"
		MTD$(2%) = "2      Per Dollar (Prem Subj)"
		MTD$(3%) = "3      Per Hour (Ovt Subj)"
		MTD$(4%) = "4      Per Hour (Ovt Not Subj)"
		MTD$(5%) = "5      Per Day"

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
		IF PR_WC_INSURANCE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_WC_INSURANCE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_WC_INSURANCE = ERR
			CONTINUE 770
		END WHEN

		PR_WC_INSURANCE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
		USE
			PR_MAIN_WC_INSURANCE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_WC_INSURANCE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_WC_INSURANCE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_WC_INSURANCE.CH%
		WHEN ERROR IN
			RESET #PR_WC_INSURANCE.CH%
			GET #PR_WC_INSURANCE.CH%, REGARDLESS
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
			"  (01)  (02)    (03)     (04)       (05)" + &
			"          (06)          (07)          ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  State Type Eff Date   Method Employer " + &
			"Rate Employee Rate MaxQtrHours        ", &
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
		FOR I% = 1% TO 7%
			A% = VAL%(MID("008,013,024,031,045,059,071", &
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

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) State\*
	!	.p
	!	The ^*State\* column defines a
	!	two (2) character State code representing
	!	States where a workman's compensation and liability insurance liability
	!	may exist relative to a specific workmen's compensation code.
	!
	! Index:
	!	.x State>Workmans Comp
	!	.x Workmans Comp>State
	!	.x State>WC
	!	.x WC>State
	!
	!--

			PR_WC_INSURANCE::CODE = PR_WC_DESCR::CODE

			PR_WC_INSURANCE::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";4", TEMP$, &
				PR_WC_INSURANCE::STATE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Type\*
	!	.p
	!	The ^*Type\* field indicates a type code
	!	for the type of compensation a worker will receive.
	!	This field does not match with any pay/deduction code.
	!	There should only be one "type" per state.
	!
	! Index:
	!	.x Type>Workmans Comp
	!	.x Workmans Comp>Type
	!	.x WC>Type
	!	.x Type>WC
	!
	!--

			PR_WC_INSURANCE::INS_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";10", TEMP$, &
				PR_WC_INSURANCE::INS_TYPE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Effective Date\*
	!	.p
	!	The ^*Effective Date\* column
	!	defines the date on which a specific insurance rate
	!	or rates will become effective. The date is to be entered in MMDDYY
	!	format.
	!
	! Index:
	!	.x Effective Date>Workmans Comp
	!	.x Effective Date>WC
	!	.x Workmans Comp>Effective Date
	!	.x WC>Effective Date
	!
	!--

			PR_WC_INSURANCE::EFFDAT = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", TEMP$, &
				PR_WC_INSURANCE::EFFDAT, MFLAG, "8", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Method\*
	!	.p
	!	The ^*Method\* field indicates the pay
	!	method which will be used in paying the compensation to the employee.
	!	By pressing the ^*<List Choices>\* key, a list of all choices is provided
	!	and a selection may be made.
	!	.b
	!	.lm +5
	!	.list 0,"*"
	!	.le
	!	1 - Per Dollar (Premium not Subject)
	!	.le
	!	2 - Per Dollar (Premium Subject)
	!	.le
	!	3 - Per Hour (Overtime Subject)
	!	.le
	!	4 - Per Hour (Overtime not Subject)
	!	.le
	!	5 - Per Day (Assumes an 8 hour day)
	!	.end list
	!	.lm -5
	!
	! Index:
	!	.x Method>Workmans Comp
	!	.x Workmans Comp>Method
	!	.x Method>WC
	!	.x WC>Method
	!
	!--

			PR_WC_INSURANCE::METHOD = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";27", TEMP$, &
				PR_WC_INSURANCE::METHOD, MFLAG, "'", MVALUE, &
				MTD$(), MTDTITLE$, "007"), -1%)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Employer Rate\*
	!	.p
	!	The ^*Employer Rate\* column
	!	refers to ^*Employer Rate\* and defines
	!	a rate per hour/dollar/day.
	!
	! Index:
	!	.x Workmans Comp>Employer Rate
	!	.x Employer Rate>Workmans Comp
	!	.x WC>Employer Rate
	!	.x Employer Rate>WC
	!
	!--

			PR_WC_INSURANCE::EMPLR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";35", TEMP$, &
				PR_WC_INSURANCE::EMPLR_RATE, MFLAG, &
				"#,###.####", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Employee Rate\*
	!	.p
	!	The ^*Employee Rate\* column in a Workmen's Compensation Table record
	!	refers to ^*Employee Rate\* and defines
	!	a rate per hour/dollar/day.
	!
	! Index:
	!	.x Workmans Comp>Employee Rate
	!	.x Employee Rate>Workmans Comp
	!	.x WC>Employee Rate
	!	.x Employee Rate>WC
	!
	!--

			PR_WC_INSURANCE::EMPLE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";49", TEMP$, &
				PR_WC_INSURANCE::EMPLE_RATE, MFLAG, &
				"#,###.####", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Maximum Quarter Hours\*
	!	.p
	!	The ^*Maximum Quarter Hours\* column
	!	refers
	!	to the rates and defines
	!	the maximum number of quarter hours which will be paid.
	!
	! Index:
	!	.x Maximum Quarter Hours>Workmans Comp
	!	.x Workmans Comp>Maximum Quarter Hours
	!	.x Maximum Quarter Hours>WC
	!	.x WC>Maximum Quarter Hours
	!
	!--

			PR_WC_INSURANCE::MAXQHOURS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";63", TEMP$, &
				PR_WC_INSURANCE::MAXQHOURS, MFLAG, "#,###.##", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_WC_INSURANCE = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_WC_INSURANCE::STATE = ""
			THEN
				PR_MAIN_WC_INSURANCE = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank State not allowed", 1%)
			END IF

		END SELECT


	!
	! Set PR_WC_INSURANCE_OLD value
	!
20500	CASE OPT_SETOLD
		PR_WC_INSURANCE_OLD = PR_WC_INSURANCE

	!
	! Restore PR_WC_INSURANCE_OLD value
	!
	CASE OPT_RESETOLD
		PR_WC_INSURANCE = PR_WC_INSURANCE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_WC_INSURANCE2 = PR_WC_INSURANCE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_WC_INSURANCE = PR_WC_INSURANCE2

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
					KEY #0% GE PR_WC_DESCR::CODE + "", &
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
			IF (PR_WC_INSURANCE::CODE = PR_WC_DESCR::CODE)
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
			PR_WC_INSURANCE::CODE = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
