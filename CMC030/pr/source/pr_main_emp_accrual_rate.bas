1	%TITLE "Employee Accrual Rate Maintenance"
	%SBTTL "PR_MAIN_EMP_ACCRUAL_RATE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_ACCRUAL_RATE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!
	! Index:
	!	.x Employee>Accrual Rate
	!	.x Accrual Rate>Employee
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_ACCRUAL_RATE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_ACCRUAL_RATE
	!	$ DELETE PR_MAIN_EMP_ACCRUAL_RATE.OBJ;*
	!
	! Author:
	!
	!	01/03/91 - Kevin Handy
	!
	! Modification history:
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
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
	!	12/13/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL_RATE.HB"
	MAP	(PR_EMP_ACCRUAL_RATE)	PR_EMP_ACCRUAL_RATE_CDD	PR_EMP_ACCRUAL_RATE
	MAP	(PR_EMP_ACCRUAL_RATE_OLD)	PR_EMP_ACCRUAL_RATE_CDD	PR_EMP_ACCRUAL_RATE_OLD, PR_EMP_ACCRUAL_RATE2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_ACCRUAL_RATE) &
		RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_ACCRUAL_RATE) &
		PR_EMP_ACCRUAL_RATE.CH%, &
		PR_EMP_ACCRUAL_RATE.READONLY%

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
		SMG_WINDOW::DESCR = "Employee Accrual Rate"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_ACCRUAL_RATE"
		SMG_WINDOW::HSIZE = 64%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 16%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 12%

		!
		! Load in defaults for Status
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_ACCRUAL_RATE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_ACCRUAL_RATE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL_RATE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_ACCRUAL_RATE = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_ACCRUAL_RATE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL_RATE.OPN"
		USE
			PR_MAIN_EMP_ACCRUAL_RATE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_ACCRUAL_RATE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_ACCRUAL_RATE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_ACCRUAL_RATE.CH%
		WHEN ERROR IN
			RESET #PR_EMP_ACCRUAL_RATE.CH%
			GET #PR_EMP_ACCRUAL_RATE.CH%, REGARDLESS
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
			"     (01)      (02)     (03)       (04)      (05) " + &
			"  (06)   (07) ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Start Date Min Hour Max Hour     Rate    Max Acc" + &
			" Method| Freq ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

 ! '   (01)   |  (02)  |  (03)  |    (04)  |   (05) | (06) | (07) |
 ! 'Start Date|Min Hour|Max Hour|    Rate  | Max Acc|Method| Freq |
 !->xx/xx/xxxx|#####.##|#####.##|#####.####|#####.##|   X  |12345S|
 !0        1         2         3         4         5         6         7
 !123456789012345678901234567890123456789012345678901234567890123456789012345678

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
		FOR I% = 1% TO 6%
			A% = VAL%(MID("013,022,031,042,051,058", &
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
	!
	! Index:
	!
	!--

			PR_EMP_ACCRUAL_RATE::SDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				PR_EMP_ACCRUAL_RATE::SDATE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Employee Accrual>Rate>Minimum Hours
	!	^*(02) Min Hour\*
	!	.p
	!	The ^*Minimum Hours\* field
	!	enters the number of hours to be worked in a pay period below which
	!	there will be no accrual made.
	!
	! Index:
	!	.x Minimum Hours>Employee Accrual>Rate
	!
	!--

			PR_EMP_ACCRUAL_RATE::MINHOUR = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", TEMP$, &
				PR_EMP_ACCRUAL_RATE::MINHOUR, MFLAG, "#####.##", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Employee Accrual>Rate>Maximum Hours
	!	^*(03) Max Hour\*
	!	.p
	!	The ^*Maximum Hours\* field
	!	enters (in the event that an accrual is based upon hours worked, rather
	!	than a flat rate) the number of hours worked to be considered a maximum, after
	!	which no further accrual would be calculated.
	!	.p
	!	For example, if an employee were paid weekly and were to have 80 hours (or
	!	two weeks) vacation accrued during a year based upon 52 weeks of 40 hours each,
	!	he/she would accrue vacation at .0385 hours per hour worked up to 40 hours, at
	!	which point the accrual calculation would reach the maximum of 1.54 hours for
	!	the week (40 x .0385 = 1.54).  If the employee worked more than 40 hours, there
	!	would be no additional vacation accrued.
	!
	! Index:
	!	.x Maximum Hours>Employee Accrual>Rate
	!
	!--

			PR_EMP_ACCRUAL_RATE::MAXHOUR = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";23", TEMP$, &
				PR_EMP_ACCRUAL_RATE::MAXHOUR, MFLAG, "#####.##", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Employee Accrual>Rate>Rate
	!	^*(03) Rate\*
	!	.p
	!	The ^*Rate\* field
	!	enters the number of hours to be accrued in each accrual period as
	!	defined in the ^*(07) Frequency\* field.
	!	.p
	!	For example, if an employee were to have two (2) week vacation accrued per
	!	year and the accrual period is biweekly, the rate to be entered in this field
	!	should be 3.077 hours.  Rate x accrual periods = annual accrual.  In this
	!	example, 3.077 x 26 = 80 hours or two (2) weeks.
	!
	! Index:
	!	.x Rate>Employee Accrual>Rate
	!
	!--

			PR_EMP_ACCRUAL_RATE::HOURRATE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";32", TEMP$, &
				PR_EMP_ACCRUAL_RATE::HOURRATE, MFLAG, "#####.####", MVALUE)

		CASE 5%

	!++
	!
	! Index:
	!
	!--

			PR_EMP_ACCRUAL_RATE::MAXACCRUE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";43", TEMP$, &
				PR_EMP_ACCRUAL_RATE::MAXACCRUE, MFLAG, "#####.##", MVALUE)


		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Method\*
	!	.p
	!	The ^*Method\* field
	!	determines which method to use to calculate the number of hours to be accrued.
	!	.p
	!	Valid options are:
	!	.b
	!	.list 0,"*"
	!	.lm +5
	!	.le
	!	1 - Flat Rate.
	!	.le
	!	2 - Per Hour.
	!	.els
	!	.lm -5
	!	The Flat Rate option causes the accrual to be calculated at the rate
	!	specified in the ^*Rate\* field.  The Per Hour option causes the
	!	accrual to be calculated depending upon the number of hours worked.
	!	.p
	!	With either option, if the ^*Minimum Hours\* field is used, there would
	!	be no accrual calculated if an employee worked less than the hours
	!	indicated in the ^*Minimum Hours\* field.
	!
	! Index:
	!	.x Method>Employee Accrual>Rate
	!	.x Employee Accrual>Rate>Method
	!
	!--

			PR_EMP_ACCRUAL_RATE::RATECODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";55", TEMP$, &
				PR_EMP_ACCRUAL_RATE::RATECODE, MFLAG, "'", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Employee Accrual>Rate>Frequency
	!	^*(07) Freq\*
	!	.p
	!	The ^*Frequency\* field
	!	indicates when an accrual will be calculated.
	!	.p
	!	There are six (6) positions in this field.  Each position has reference to a
	!	possible payroll period within any given month, i.e. the first position
	!	refers to the the first payroll period, the second position refers to the
	!	second payroll period, etc.  The sixth position refers to a special period,
	!	such as a bonus payroll.
	!	.p
	!	For example, if an employee were to be paid biweekly and an accrual were
	!	intended to be calculated on all regular payroll periods, but no other periods,
	!	this field would contain "YYYNNN", indicating an accrual should be calculated
	!	on the first, second, and third payroll periods only in any month.
	!
	! Index:
	!	.x Frequency>Employee Accrual>Rate
	!
	!--
			PR_EMP_ACCRUAL_RATE::FREQ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";59", TEMP$, &
				PR_EMP_ACCRUAL_RATE::FREQ, MFLAG, "'LLLLL", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_ACCRUAL_RATE = 0%

	!
	! Set PR_EMP_ACCRUAL_RATE_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_ACCRUAL_RATE_OLD = PR_EMP_ACCRUAL_RATE

	!
	! Restore PR_EMP_ACCRUAL_RATE_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_ACCRUAL_RATE = PR_EMP_ACCRUAL_RATE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_ACCRUAL_RATE2 = PR_EMP_ACCRUAL_RATE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_ACCRUAL_RATE = PR_EMP_ACCRUAL_RATE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_ACCRUAL_RATE::EMPNUM = PR_EMP_ACCRUAL::EMPNUM
		PR_EMP_ACCRUAL_RATE::ATYPE = PR_EMP_ACCRUAL::ATYPE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_ACCRUAL_RATE.CH%, &
				KEY #0% GE PR_EMP_ACCRUAL_RATE::EMPNUM + &
					PR_EMP_ACCRUAL_RATE::ATYPE, REGARDLESS
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
					KEY #0% GE PR_EMP_ACCRUAL::EMPNUM + &
					PR_EMP_ACCRUAL::ATYPE, &
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

			IF PR_EMP_ACCRUAL_RATE::EMPNUM = PR_EMP_ACCRUAL::EMPNUM AND &
				PR_EMP_ACCRUAL_RATE::ATYPE = PR_EMP_ACCRUAL::ATYPE
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
			PR_EMP_ACCRUAL_RATE::EMPNUM = RIGHT(MVALUE, 2%)
			PR_EMP_ACCRUAL_RATE::ATYPE = RIGHT(MVALUE, 12%)

		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Nop right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD005
	!	.x Employee Accrual>Rate>Maximum Hours
	!	^*(05) Max Acc\*
	!	.p
	!	The ^*Maximum Hours Accrued\* field
	!	enters the number of accrued hours balance beyond which
	!	there will be no further accrual made until this balance is reduced below the
	!	contents of this field.
	!	.p
	!	For example, if company policy dictates that an employee cannot have more
	!	than four (4) weeks vacation accrued, the ^*Maximum Hour\* field would
	!	contain 160 hours.  If the employee's accrued vacation hours equaled 160,
	!	no further vacation would be accrued for the employee until the employee used
	!	vacation which would cause the accrued hours to be decrimented below the 160
	!	hour cap.
	!
	! Index:
	!	.x Maximum Hours>Employee Accrual>Rate
	!
	!--
