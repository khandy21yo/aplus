1	%TITLE "Tax Profile Maintenance"
	%SBTTL "PR_MAIN_REG_TAXES"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_REG_TAXES(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Tax Profile Maintenance\* option maintains the
	!	Tax Profile in the Annual Payroll Register.
	!
	! Index:
	!	.X Taxes>Payroll Annual Register
	!	.X Payroll Annual Register>Taxes
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_REG_TAXES/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_REG_TAXES
	!	$ DELETE PR_MAIN_REG_TAXES.OBJ;*
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
	!	04/03/89 - Kevin Handy
	!		Added code for SI Tax package.
	!
	!	05/25/89 - Kevin Handy
	!		Made it so that the ST$() array is held in a
	!		common area so that it is still there after
	!		the init. (maybe we will be able to add records
	!		now).
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
	!	03/05/98 - Kevin Handy
	!		Allow entry of FH code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERRO IN
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES
	MAP (PR_REG_TAXES_OLD) PR_REG_TAXES_CDD PR_REG_TAXES_OLD, PR_REG_TAXES2

	MAP (PR_REG_YYYY) YYYY$ = 4%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_REG_TAXES) &
		RARRAY_RECORD RARRAY(100%)

	MAP (TT_PR_REG_TAXES_2) &
		STTITLE$ = 16%, &
		ST$(20%) = 16%

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_REG_TAXES) &
		PR_REG_TAXES.CH%, &
		PR_REG_TAXES.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "PR Tax Register"
		SMG_WINDOW::NHELP = "PR_MAIN_REG_TAXES"
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
		! Withholding types
		!
		STTITLE$ = "Type Description" &

		ST$(0%) = "10"
		ST$(1%) = "FH   Fica"
		ST$(2%) = "FI   Fica"
		ST$(3%) = "FW   Federal"
		ST$(4%) = "SW   State"
		ST$(5%) = "SX   OST"
		ST$(6%) = "CW   City"
		ST$(7%) = "DW   County"
		ST$(8%) = "EW   School"
		ST$(9%) = "SI   WC Liability Ins."
		ST$(10%) = "SU   State Unemp. Ins."

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
		IF PR_REG_TAXES.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_REG_TAXES.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_REG_TAXES = ERR
			CONTINUE 770
		END WHEN

		PR_REG_TAXES.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
		USE
			PR_MAIN_REG_TAXES = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_REG_TAXES.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_REG_TAXES.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_REG_TAXES.CH%
		WHEN ERROR IN
			RESET #PR_REG_TAXES.CH%
			GET #PR_REG_TAXES.CH%, REGARDLESS
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
			"(01) (02)          (03)      (07)       (11)   (15)" + &
			"                           ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Type Code         Taxable Reportable Taxes    Wks Wkd        " + &
			"                 ", &
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
			A% = VAL%(MID("005,010,016,026,036,046,054", &
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
	!	The ^*Type\* column contains two
	!	(2) character tax type codes. Valid codes are:
	!	.b
	!	.lm +5
	!	.LIST 0,"o"
	!	.LE
	!	FI = FICA
	!	.LE
	!	FW = Federal Withholding Tax
	!	.LE
	!	SW = State Withholding Tax
	!	.LE
	!	SX = Other State Tax
	!	.LE
	!	CW = City Withholding Tax
	!	.LE
	!	DW = County Withholding Tax
	!	.LE
	!	EW = School District Withholding Tax
	!	.ELS
	!	.lm -5
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.end note
	!
	! Index:
	!	.x Payroll Tax Register>Maintenance>Type
	!	.x Type>Payroll Tax Register Maintenance
	!
	!--

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 1", &
				SMG_WINDOW::CURLIN, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Taxable", &
				SMG_WINDOW::CURLIN, 56%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Reportable", &
				SMG_WINDOW::CURLIN + 1%, 56%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 2", &
				SMG_WINDOW::CURLIN + 1%, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Taxes", &
				SMG_WINDOW::CURLIN + 2%, 56%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 3", &
				SMG_WINDOW::CURLIN + 2%, 11%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Wks Wkd", &
				SMG_WINDOW::CURLIN + 3%, 56%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"Qtr 4", &
				SMG_WINDOW::CURLIN + 3%, 11%, , SMG$M_REVERSE)

			PR_REG_TAXES::TTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_REG_TAXES::TTYPE, MFLAG, "'E", MVALUE, &
				ST$(), STTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Code\*
	!	.p
	!	The ^*Code\* column contains two
	!	(2) character codes identifying specific States, cities, counties
	!	and/or school districts related to the appropriate tax types. State
	!	codes are equal to Post Office State codes. Other codes are user
	!	defined as required. (Records for Federal type taxes require no
	!	codes in column (02)).
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.end note
	!
	! Index:
	!	.x Payroll Tax Register>Code
	!	.x Code>Payroll Tax Register
	!
	!--

			PR_REG_TAXES::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";7", TEMP$, &
				PR_REG_TAXES::CODE, MFLAG, &
				"'E", MVALUE)

		CASE 3% TO 6%

			SCOPE::PRG_ITEM = "FLD003WAG"
	!++
	! Abstract:FLD003WAG
	!	^*(03) - (06) Wages\*
	!	.p
	!	The ^*Wages\* column contains
	!	the subject earnings amount for each calendar quarter relative
	!	to a specific tax type and code.
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.end note
	!
	! Index:
	!	.x Payroll Tax Register>Wages
	!	.x Wages>Payroll Tax Register
	!	.x Earnings>Payroll Tax Register
	!
	!--

			PR_REG_TAXES::TAXABLE(MLOOP - 3%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 3%) + ";17", &
				TEMP$, &
				PR_REG_TAXES::TAXABLE(MLOOP - 3%), &
				MFLAG, "######.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_TAXES::TAXABLE(0%) + &
				PR_REG_TAXES::TAXABLE(1%) + &
				PR_REG_TAXES::TAXABLE(2%) + &
				PR_REG_TAXES::TAXABLE(3%), "#######.##"), &
				SMG_WINDOW::CURLIN, 68%)

		CASE 7% TO 10%

			SCOPE::PRG_ITEM = "FLD007WAG"
	!++
	! Abstract:FLD003WAG
	!	^*(03) - (06) Wages\*
	!	.p
	!	The ^*Wages\* column contains
	!	the subject earnings amount for each calendar quarter relative
	!	to a specific tax type and code.
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.end note
	!
	! Index:
	!	.x Payroll Tax Register>Wages
	!	.x Wages>Payroll Tax Register
	!	.x Earnings>Payroll Tax Register
	!
	!--

			PR_REG_TAXES::REPORTABLE(MLOOP - 7%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 7%) + ";27", &
				TEMP$, &
				PR_REG_TAXES::REPORTABLE(MLOOP - 7%), &
				MFLAG, "######.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_TAXES::REPORTABLE(0%) + &
				PR_REG_TAXES::REPORTABLE(1%) + &
				PR_REG_TAXES::REPORTABLE(2%) + &
				PR_REG_TAXES::REPORTABLE(3%), "#######.##"), &
				SMG_WINDOW::CURLIN + 1%, 68%)


		CASE 11% TO 14%

			SCOPE::PRG_ITEM = "FLD007TAX"
	!++
	! Abstract:FLD011TAX
	!	^*(11) - (14) Taxes\*
	!	.p
	!	The ^*Taxes\* column contains the
	!	amount of taxes withheld for each calendar quarter relative to
	!	the specific tax type and code.
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.END NOTE
	!
	! Index:
	!	.x Payroll Tax Register>Taxes
	!	.x Taxes>Payroll Tax Register
	!
	!--

			PR_REG_TAXES::TAX(MLOOP - 11%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 11%) + ";37", &
				TEMP$, &
				PR_REG_TAXES::TAX(MLOOP - 11%), &
				MFLAG, "######.##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_TAXES::TAX(0%) + &
				PR_REG_TAXES::TAX(1%) + &
				PR_REG_TAXES::TAX(2%) + &
				PR_REG_TAXES::TAX(3%), "#######.##"), &
				SMG_WINDOW::CURLIN + 2%, 68%)

		CASE 15% TO 18%

		SCOPE::PRG_ITEM = "FLD015WW"
	!++
	! Abstract:FLD015WW
	!	^*(15) - (18) Weeks Worked\*
	!	.p
	!	The ^*Weeks Worked\* column
	!	contains the number of weeks worked during each calendar quarter.
	!	(The information is not recorded in reference to FI (FICA) type
	!	taxes.)
	!	.note
	!	The Payroll Register Maintenance option is ^*not\*
	!	intended for regular data entry or file editing,
	!	except during system initialization procedures
	!	when the payroll system is installed at a time
	!	other than the beginning of a calendar year.
	!	.END NOTE
	!
	! Index:
	!	.x Payroll Tax Register>Weeks Worked
	!	.x Weeks Worked>Payroll Tax Register
	!
	!--
			PR_REG_TAXES::WKWRK(MLOOP - 15%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(SMG_WINDOW::CURLIN + MLOOP - 15%) + ";49", &
				TEMP$, &
				PR_REG_TAXES::WKWRK(MLOOP - 15%) * 1.0, &
				MFLAG, "##", MVALUE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_REG_TAXES::WKWRK(0%) + &
				PR_REG_TAXES::WKWRK(1%) + &
				PR_REG_TAXES::WKWRK(2%) + &
				PR_REG_TAXES::WKWRK(3%), "##########"), &
				SMG_WINDOW::CURLIN + 3%, 68%)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_REG_TAXES = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Don't allow blank account numbers
			!
			IF PR_REG_TAXES::TTYPE = ""
			THEN
				PR_MAIN_REG_TAXES = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal code", 1%)
				EXIT FUNCTION
			END IF

		END SELECT

	!
	! Set PR_REG_TAXES_OLD value
	!
20500	CASE OPT_SETOLD
		PR_REG_TAXES_OLD = PR_REG_TAXES

	!
	! Restore PR_REG_TAXES_OLD value
	!
	CASE OPT_RESETOLD
		PR_REG_TAXES = PR_REG_TAXES_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_REG_TAXES2 = PR_REG_TAXES

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_REG_TAXES = PR_REG_TAXES2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_REG_TAXES::EMPNUM = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_REG_TAXES.CH%, &
				KEY #0% GE PR_REG_TAXES::EMPNUM + "", &
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

			IF PR_REG_TAXES::EMPNUM = PR_EMP_MASTER::EMPNUM
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
			PR_REG_TAXES::EMPNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
