1	%TITLE "Employee Rate Maintenance"
	%SBTTL "PR_MAIN_EMP_RATE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_RATE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Rate\* function
	!	accesses the file where an employee's pay rate(s)
	!	is(are) recorded. An employee may have multiple rates, depending upon
	!	the operation codes. Both hourly (time related) and piece rates can be
	!	recorded. Any rate in the file is date sensitive, and a historical
	!	record of pay rates is maintained. Overtime rates are recorded as a
	!	percentage of the regular rates. If applicable, efficiency rates are
	!	also recorded.
	!
	! Index:
	!	.x Employee>Pay Rates
	!	.x Pay Rates
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_RATE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_RATE
	!	$ DELETE PR_MAIN_EMP_RATE.OBJ;*
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
	!	01/25/89 - Kevin Handy
	!		Removed '%' from inside of the-format on the
	!		overtime factor because it did not allow
	!		defaults to work.
	!
	!	10/25/90 - Kevin Handy
	!		Added rate type 'X' (eXcess).
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/09/92 - Kevin Handy
	!		Increased dimension from 300 to 600.  DWI had
	!		an employee with a LOT of rates on her.
	!
	!	07/01/94 - Kevin Handy
	!		Increased dimension from 600 to 900. WOW that's
	!		a lot of rates.
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
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/16/2004 - Kevin Handy
	!		Add more digits to rate field (Marco)
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE
	MAP (PR_EMP_RATE_OLD)	PR_EMP_RATE_CDD	PR_EMP_RATE_OLD, PR_EMP_RATE2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_RATE) RARRAY_RECORD RARRAY(900%)

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_RATE) &
		PR_EMP_RATE.CH%, &
		PR_EMP_RATE.READONLY%

	COM (TT_EMP_RATE) &
		ECTITLE$ = 20%, &
		EC$(6%) = 20% &

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
		SMG_WINDOW::DESCR = "Employee rates"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_RATE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%

		!
		! List of types
		!
		ECTITLE$ = "Type Description"
		EC$(0%) = "5"
		EC$(1%) = "H    Hourly"
		EC$(2%) = "S    Salary"
		EC$(3%) = "P    Piece"
		EC$(4%) = "M    Mileage"
		EC$(5%) = "X    eXcess"
		EC$(6%) = ""

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_RATE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_RATE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_RATE = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_RATE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
		USE
			PR_MAIN_EMP_RATE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_RATE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_RATE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_RATE.CH%
		WHEN ERROR IN
			RESET #PR_EMP_RATE.CH%
			GET #PR_EMP_RATE.CH%, REGARDLESS
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
			"  (01)        (02)    (03) (04)       (05)     " + &
			"(06)  (07)     (08)     (09)   ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Oper     Effct Date Type Code    Hr Rate     " + &
			"Unit OvtPct Eff Rate Re-eval   ", &
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
		FOR I% = 1% TO 8%
			A% = VAL%(MID("011,022,027,032,043,052,059;068", &
				I% * 4% - 3%, 3%))

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
	!	^*(01) Operation\*
	!	.P
	!	The ^*Operation\* column
	!	establishes different pay rates for each possible operation
	!	at which an employee may work.
	!	.p
	!	If a rate of pay is not operation dependent, the field would
	!	be left blank.
	!
	! Index:
	!	.x Operation>Pay Rate
	!	.x Pay Rate>Operation
	!
	!--

			PR_EMP_RATE::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_EMP_RATE::OPER, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Effective Date\*
	!	.p
	!	The ^*Effective Date\* column
	!	records the date a specific rate will become effective.
	!	Any time or pieces entered for dates equal to or greater than the
	!	Effective Date will be multiplied by the related rate, otherwise, the
	!	pay rate will default to a rate with a prior date.
	!	.p
	!	The system provides the ability to change pay rates effective on
	!	any date rather than at the beginning of a payroll period only. Also,
	!	since an old pay rate is not overwritten by a new pay rate, historical
	!	pay rate information is maintained for each employee.
	!
	! Index:
	!	.x Effective Date>Pay Rate
	!	.x Pay Rate>Effective Date
	!
	!--

			PR_EMP_RATE::EFFDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";12", TEMP$, &
				PR_EMP_RATE::EFFDAT, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Type\*
	!	.p
	!	The ^*Type\* column
	!	records an employee's exemption status in regard to Federal Wage
	!	and Hour Law, or as to the method the employee will be paid, i.e. by
	!	the time worked or by the number of pieces produced. Valid types are:
	!	.b
	!	.lm +5
	!	.LIST 0,"o"
	!	.LE
	!	H = Time rate, non-exempt (Hourly)
	!	.LE
	!	S = Exempt (Salaried)
	!	.LE
	!	P = Piece rate
	!	.le
	!	M = Mileage
	!	.le
	!	X = eXcess (Maximum of Hour/Piece)
	!	.ELS
	!	.lm -5
	!
	! Index:
	!	.x Type>Pay Rate
	!	.x Pay Rate>Type
	!
	!--

			PR_EMP_RATE::RATE_TYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";24", TEMP$, &
				PR_EMP_RATE::RATE_TYPE, MFLAG, "'", MVALUE, &
				EC$(), ECTITLE$, "005"), -1%)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Code\*
	!	.p
	!	The ^*Code\* column
	!	enters a Pay Type Code as established in the PRETBL file.
	!	Most generally, the value would be equal to the user defined code
	!	representing regular taxable earnings.
	!
	! Index:
	!	.x Code>Pay Rate
	!	.x Pay Rate>Code
	!
	!--

			PR_EMP_RATE::RATE_CDE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";29", TEMP$, &
				PR_EMP_RATE::RATE_CDE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, &
					"V0 ") = 1%)
				THEN
					PR_EMP_RATE::RATE_CDE = &
						PR_ERNDED_DEF::CODE
					GOTO E0Loop
				END IF
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* column
	!	enters hourly or time related rates. Multiple
	!	rates may be entered for each employee depending upon the operation
	!	codes. The field will accommodate four positions to the left of the
	!	decimal point and three positions to the right of the decimal point.
	!	.p
	!	It is recommended that pay rates for salaried personnel be
	!	converted to equivalent hourly rates. Doing so, enables tracking
	!	various kinds of pay, i.e. regular pay, vacation pay, sick pay,
	!	personal leave, etc. However, the size of the "hourly" rate field
	!	enables the field to be used not just as an "hourly" rate but as a
	!	"time" unit rate. The value in this field could be a "salary" per
	!	pay period. Entering a time unit of "1" would result in the salary
	!	being paid.
	!
	! Index:
	!	.x Hourly Rate>Pay Rate
	!	.x Pay Rate>Hourly Rate
	!
	!--

			PR_EMP_RATE::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";33", TEMP$, &
				PR_EMP_RATE::HOUR_RATE * 1.0, &
				MFLAG, "#######.###", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Unit\*
	!	.p
	!	The ^*Unit\* column
	!	enters piece rates. Multiple piece rates for each employee
	!	may be entered depending upon different operations. The field will
	!	accommodate up to four (4) positions to the left of the decimal point
	!	and three (3) positions to the right of the decimal point.
	!
	! Index:
	!	.x Pay Rate>Unit Rate
	!	.x Unit Rate>Pay Rate
	!
	!--

			PR_EMP_RATE::PIECE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";44", TEMP$, &
				PR_EMP_RATE::PIECE_RATE * 1.0, &
				MFLAG, "###.####", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Overtime Percent\*
	!	.p
	!	The ^*Overtime Percent\* column
	!	enters overtime rates which are expressed as a
	!	percentage of the regular rates. For example, if an overtime rate
	!	were to be paid at "time and a half", the value of "150" would be
	!	entered in this field.
	!
	! Index:
	!	.x Pay Rate>Overtime Percent
	!	.x Overtime Percent>Pay Rate
	!
	!--

			PR_EMP_RATE::FACTOR = FUNC_ROUND(ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";55", TEMP$, &
				PR_EMP_RATE::FACTOR * 1.0, &
				MFLAG, "###", MVALUE), 3%)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Efficiency Rate\*
	!	.p
	!	The ^*Efficiency Rate\* column
	!	enters efficiency rates which are expressed
	!	as a percentage of an industrial standard rate to the nearest one
	!	one-hundredth of a percent.
	!
	! Index:
	!	.x Pay Rate>Efficiency Rate
	!	.x Efficiency Rate>Pay Rate
	!
	!--

			PR_EMP_RATE::STDEFF = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";61", TEMP$, &
				PR_EMP_RATE::STDEFF * 1.0, MFLAG, &
				"####.##", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Re-evaluate\*
	!	.p
	!	The ^*Re-evaluate\* field indicates the date the employee will be evaluated
	!	again in terms of performance. Such an evaluation could result in a change of
	!	the employee's rate or level with in the company.
	!
	! Index:
	!	.x Pay Rate>Re-evaluate Date
	!	.x Re-evaluate Date>Pay Rate
	!
	!--
			PR_EMP_RATE::EVAL_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";69", TEMP$, &
				PR_EMP_RATE::EVAL_DATE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_RATE = 0%

		SELECT MLOOP

		CASE 4%
			PR_MAIN_EMP_RATE = FUNC_TESTENTRY( SMG_WINDOW, &
				"P" + PR_EMP_RATE::RATE_CDE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		END SELECT

	!
	! Set PR_EMP_RATE_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_RATE_OLD = PR_EMP_RATE

	!
	! Restore PR_EMP_RATE_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_RATE = PR_EMP_RATE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_RATE2 = PR_EMP_RATE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_RATE = PR_EMP_RATE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_RATE::EMPNUM = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_RATE.CH%, &
				KEY #0% GE PR_EMP_RATE::EMPNUM + "", &
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

			IF PR_EMP_RATE::EMPNUM = PR_EMP_MASTER::EMPNUM
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
			PR_EMP_RATE::EMPNUM = RIGHT(MVALUE, 2%)

		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Nop right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
