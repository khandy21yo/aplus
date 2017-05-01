1	%TITLE "Payment/Deductions"
	%SBTTL "PR_MAIN_REG_ERNDED"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_REG_ERNDED(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Payment/Deduction\* screen allows for input of data concerning the
	!	payments and deductions.
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_REG_ERNDED/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_REG_ERNDED
	!	$ DELETE PR_MAIN_REG_ERNDED.OBJ;*
	!
	! Author:
	!
	!	10/15/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	05/25/89 - Kevin Handy
	!		Fixed bug where list choices did not work on
	!		input line 2.
	!
	!	07/13/91 - Kevin Handy
	!		Added "A" type to file for keeping track of
	!		accruals.
	!
	!	07/13/91 - Kevin Handy
	!		Modified to check accrual type "A" against the
	!		pay "P" types.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	01/24/95 - Kevin Handy
	!		Added two more digits to the Unit fields,
	!		so that KingB wouldn't overflow.
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
	!	05/26/98 - Kevin Handy
	!		Added 'F' as an allowed type
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED
	MAP (PR_REG_ERNDED_OLD) PR_REG_ERNDED_CDD PR_REG_ERNDED_OLD, &
		PR_REG_ERNDED2

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	MAP (PR_REG_YYYY) YYYY$ = 4%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_REG_ERNDED) RARRAY_RECORD RARRAY(100%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_REG_CODE_ERNDED) &
		REG_PTTITLE$ = 20%, &
		REG_PT$(7%) = 20%

	COM (CH_PR_REG_ERNDED) &
		PR_REG_ERNDED.CH%, &
		PR_REG_ERNDED.READONLY%


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
		SMG_WINDOW::DESCR = "PR Pay/Deduct Register"
		SMG_WINDOW::NHELP = "PR_MAIN_REG_ERNDED"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 12%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 18%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 12%
		SMG_WINDOW::LINREC = 5%

		!
		! Pay types
		!
		REG_PTTITLE$ = "Type Description"
		REG_PT$(0%) = "6"
		REG_PT$(1%) = "A    Accrual"
		REG_PT$(2%) = "P    Payment"
		REG_PT$(3%) = "D    Deduction"
		REG_PT$(4%) = "T    Noncompensation"
		REG_PT$(5%) = "M    Memo"
		REG_PT$(6%) = "f    Final (Deduction)"

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
		IF PR_REG_ERNDED.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_REG_ERNDED.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_REG_ERNDED = ERR
			CONTINUE 770
		END WHEN

		PR_REG_ERNDED.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
		USE
			PR_MAIN_REG_ERNDED = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_REG_ERNDED.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_REG_ERNDED.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_REG_ERNDED.CH%
		WHEN ERROR IN
			RESET #PR_REG_ERNDED.CH%
			GET #PR_REG_ERNDED.CH%, REGARDLESS
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
			"(01) (02)         (03)      (07)    (11)  " + &
			"  (15)                              ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Type Code         Amount  Reg Hrs Ovt Hrs " + &
			"  Units                             ", &
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
		FOR I% = 1% TO 7%
			A% = VAL%(MID("005,010,016,026,034,042,052", &
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
	!	The ^*Type\* column
	!	contains codes for transaction types. Valid types are:
	!	.b
	!	.lm +5
	!	.LIST 0,"o"
	!	.LE
	!	P = Payments
	!	.LE
	!	D = Deductions
	!	.LE
	!	T = Non Compensation Entries, i.e. tips
	!	.LE
	!	M = Memo Entries, i.e. reportable benefits
	!	.le
	!	A = Accrual Entries
	!	.lm -5
	!	.ELS
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.END NOTE
	!
	! Index:
	!	.x Payroll Register>Payments/Deduction>Type
	!	.x Type>Payroll Register>Payments/Deduction
	!
	!--

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 1", &
				SMG_WINDOW::CURLIN, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Total: Amount", &
				SMG_WINDOW::CURLIN, 53%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 2", &
				SMG_WINDOW::CURLIN + 1%, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"       Reg Hrs", &
				SMG_WINDOW::CURLIN + 1%, 53%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 3", &
				SMG_WINDOW::CURLIN + 2%, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"       Ovt Hrs", &
				SMG_WINDOW::CURLIN + 2%, 53%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 4", &
				SMG_WINDOW::CURLIN + 3%, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"       Units", &
				SMG_WINDOW::CURLIN + 3%, 53%)

			PR_REG_ERNDED::ETYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_REG_ERNDED::ETYPE, MFLAG, "'", MVALUE, &
				REG_PT$(), REG_PTTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Code\*
	!	.p
	!	The ^*Code\* column
	!	contains a two (2) character code designating the specific kind of
	!	payment, deduction, etc. Codes are user defined. Valid codes can
	!	be viewed by pressing the ^*<List Choices>\* key. The system will
	!	list all codes established in the Payments, Accruals, Deductions
	!	Code Definition file.
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.END NOTE
	!
	!
	! Index:
	!	.x Payroll Register>Tax Profile
	!
	!--

			PR_REG_ERNDED::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";7", TEMP$, &
				PR_REG_ERNDED::CODE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, &
					"VX") = 1%)
				THEN
					PR_REG_ERNDED::CODE = &
						PR_ERNDED_DEF::CODE
					GOTO E0Loop
				END IF
			END IF

		CASE 3% TO 6%
			SCOPE::PRG_ITEM = "FLD003AMT"
	!++
	! Abstract:FLD003AMT
	!	^*(03) - (06) Amount\*
	!	.p
	!	The ^*Amount\* field refers to the amount of pay that is relevant to the
	!	situation on hand.
	!
	! Index:
	!	.x Amount>Payments/Deductions
	!
	!--
			PR_REG_ERNDED::QTR_DOLL(MLOOP - 3%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 3%) + ";17", &
				TEMP$, &
				PR_REG_ERNDED::QTR_DOLL(MLOOP - 3%), &
				MFLAG, "######.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_ERNDED::QTR_DOLL(0%) + &
				PR_REG_ERNDED::QTR_DOLL(1%) + &
				PR_REG_ERNDED::QTR_DOLL(2%) + &
				PR_REG_ERNDED::QTR_DOLL(3%), "#######.##"), &
				SMG_WINDOW::CURLIN, 67%)

		CASE 7% TO 10%
			SCOPE::PRG_ITEM = "FLD007RHRS"
	!++
	! Abstract:FLD007RHRS
	!	^*(07) - (10) Regular Hours\*
	!	.B
	!	.LM +5
	!	The ^*Regular Hours\* field refers to the amount of regular hours worked
	!	by the employee over a specific time period.
	!	.LM -5
	!
	! Index:
	!	.x Regular Hours
	!
	!--
			PR_REG_ERNDED::REG_HRS(MLOOP - 7%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 7%) + ";27", &
				TEMP$, &
				PR_REG_ERNDED::REG_HRS(MLOOP - 7%), &
				MFLAG, "####.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_ERNDED::REG_HRS(0%) + &
				PR_REG_ERNDED::REG_HRS(1%) + &
				PR_REG_ERNDED::REG_HRS(2%) + &
				PR_REG_ERNDED::REG_HRS(3%), "#######.##"), &
				SMG_WINDOW::CURLIN + 1%, 67%)

		CASE 11% TO 14%
			SCOPE::PRG_ITEM = "FLD011OHRS"
	!++
	! Abstract:FLD011OHRS
	!	^*(11) - (14) Overtime Hours\*
	!	.p
	!	The ^*Overtime Hours\* field refers to the overtime hours worked by the
	!	employee over a specific time period.
	!
	! Index:
	!	.x Overtime Hours
	!
	!--
			PR_REG_ERNDED::PRE_HRS(MLOOP - 11%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 11%) + ";35", &
				TEMP$, &
				PR_REG_ERNDED::PRE_HRS(MLOOP - 11%) * 1.0, &
				MFLAG, "####.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_ERNDED::PRE_HRS(0%) + &
				PR_REG_ERNDED::PRE_HRS(1%) + &
				PR_REG_ERNDED::PRE_HRS(2%) + &
				PR_REG_ERNDED::PRE_HRS(3%), "#######.##"), &
				SMG_WINDOW::CURLIN + 2%, 67%)

		CASE 15% TO 18%
			SCOPE::PRG_ITEM = "FLD015UNT"
	!++
	! Abstract:FLD015UNT
	!	^*(15) - (18) Units\*
	!	.p
	!	The ^*Units\* field refers to the number of units completed by each employee.
	!	This is used by those on unit pay systems instead of hourly wage system.
	!
	! Index:
	!	.x Units
	!
	!--
			PR_REG_ERNDED::UNITS(MLOOP - 15%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 15%) + ";43", &
				TEMP$, &
				PR_REG_ERNDED::UNITS(MLOOP - 15%), &
				MFLAG, "######.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_ERNDED::UNITS(0%) + &
				PR_REG_ERNDED::UNITS(1%) + &
				PR_REG_ERNDED::UNITS(2%) + &
				PR_REG_ERNDED::UNITS(3%), "#######.##"), &
				SMG_WINDOW::CURLIN + 3%, 67%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_REG_ERNDED = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Don't allow blank account numbers
			!
			IF PR_REG_ERNDED::ETYPE = ""
			THEN
				PR_MAIN_REG_ERNDED = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal code", 1%)
				EXIT FUNCTION
			END IF

		CASE 2%
			IF PR_REG_ERNDED::CODE = ""
			THEN
				PR_MAIN_REG_ERNDED = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal code", 1%)
				EXIT FUNCTION
			END IF

			IF PR_REG_ERNDED::ETYPE = "A"
			THEN
				TEMP$ = "P"
			ELSE
				TEMP$ = PR_REG_ERNDED::ETYPE
			END IF

			PR_MAIN_REG_ERNDED = FUNC_TESTENTRY(SMG_WINDOW, &
				TEMP$ + PR_REG_ERNDED::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		END SELECT

	!
	! Set PR_REG_ERNDED_OLD value
	!
20500	CASE OPT_SETOLD
		PR_REG_ERNDED_OLD = PR_REG_ERNDED

	!
	! Restore PR_REG_ERNDED_OLD value
	!
	CASE OPT_RESETOLD
		PR_REG_ERNDED = PR_REG_ERNDED_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_REG_ERNDED2 = PR_REG_ERNDED

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_REG_ERNDED = PR_REG_ERNDED2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_REG_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_REG_ERNDED.CH%, &
				KEY #0% GE PR_REG_ERNDED::EMPNUM + "", &
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

			IF PR_REG_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM
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
			PR_REG_ERNDED::EMPNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION

