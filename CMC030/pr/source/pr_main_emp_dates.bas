1	%TITLE "Chronicle"
	%SBTTL "PR_MAIN_EMP_DATES"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_DATES(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	!	The ^*Chronicle\* function
	!	accesses the Employee Chronicle file where an employee's
	!	work history is maintained.
	!	.p
	!	A record is maintained for all events by code with the beginning and ending
	!	date. A field for comment is also available.
	!
	! Index:
	!	.x Employee>Chronicle
	!	.x Chronicle>Employee
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_DATES/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_DATES
	!	$ DELETE PR_MAIN_EMP_DATES.OBJ;*
	!
	! Author:
	!
	!	03/01/89 - Kevin Handy
	!
	! Modification history:
	!
	!	10/03/89 - Kevin Handy
	!		Put include for MAIN_WINDOW.COM back in so
	!		that the function would work.
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
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"
	MAP	(PR_EMP_DATES)		PR_EMP_DATES_CDD	PR_EMP_DATES
	MAP	(PR_EMP_DATES_OLD)	PR_EMP_DATES_CDD	PR_EMP_DATES_OLD, PR_EMP_DATES2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_DATES) RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_DATES) &
		PR_EMP_DATES.CH%, &
		PR_EMP_DATES.READONLY%

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
		SMG_WINDOW::DESCR = "Employee Chronicle"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_DATES"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%

		!
		! Load in defaults for Status
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_DATES.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_DATES.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_DATES = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_DATES.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.OPN"
		USE
			PR_MAIN_EMP_DATES = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_DATES.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_DATES.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_DATES.CH%
		WHEN ERROR IN
			RESET #PR_EMP_DATES.CH%
			GET #PR_EMP_DATES.CH%, REGARDLESS
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
			"(01)    (02)       (03)    (04)   " + &
			SPACE$(78% - 34%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Code Begin Date  End Date  Comment" + &
			SPACE$(78% - 34%), &
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
		FOR I% = 1% TO 3%
			A% = VAL%(MID("005,016,027", I% * 4% - 3%, 3%))

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
	!	^*(01) Code\*
	!	.p
	!	The code defines what the dates on this lines
	!	are for.
	!	The following codes are currently recognized:
	!	.B
	!	.list 0,"*"
	!	.le
	!	^*AC\* - Active. Dates are hire/terminate dates.
	!	.le
	!	^*LA\* - Leave of absence.
	!	.le
	!	^*CL\* - Class. Put class code in description.
	!	.le
	!	^*VA\* - Vacation.
	!	.le
	!	^*SK\* - Sick.
	!	.le
	!	^*AE\* - Absence (excused).
	!	.le
	!	^*AU\* - Absence (unexcused).
	!	.els
	!
	! Index:
	!	.x Chronicle>Code
	!	.x Code>Chronicle
	!
	!--

			PR_EMP_DATES::DATECD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_EMP_DATES::DATECD, MFLAG OR 16%, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Beginning Date\*
	!	.p
	!	The ^*Beginning Date\* field refers the the date in which the situation
	!	will or did begin. For example, the beginning date of the vacation period.
	!
	! Index:
	!	.x Beginning Date>Chronicle
	!	.x Chronicle>Beginning Date
	!
	!--
			PR_EMP_DATES::DATEBEGIN = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";6", TEMP$, &
				PR_EMP_DATES::DATEBEGIN, MFLAG, "8", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) End Date\*
	!	.p
	!	The ^*End Date\* refers to the date in which the condition will or did end.
	!	For Example, the date the vacation period ends.
	!
	! Index:
	!	.x End Date>Chronicle
	!	.x Chronicle>End Date
	!
	!--
			PR_EMP_DATES::DATEEND = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";17", TEMP$, &
				PR_EMP_DATES::DATEEND, MFLAG, "8", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Comment\*
	!	.p
	!	The ^*Comment\* field allows for the user to input any comments pertaining
	!	to the data input concerning the situation at hand.
	!
	! Index:
	!	.x Comment>Chronicle
	!	.x Chronicle>Comment
	!
	!--
			PR_EMP_DATES::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";28", TEMP$, &
				PR_EMP_DATES::DESCR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_DATES = 0%

	!
	! Set PR_EMP_DATES_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_DATES_OLD = PR_EMP_DATES

	!
	! Restore PR_EMP_DATES_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_DATES = PR_EMP_DATES_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_DATES2 = PR_EMP_DATES

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_DATES = PR_EMP_DATES2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_DATES::EMPLOYEE = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_DATES.CH%, &
				KEY #0% GE PR_EMP_DATES::EMPLOYEE + "", &
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

			IF PR_EMP_DATES::EMPLOYEE = PR_EMP_MASTER::EMPNUM
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
			PR_EMP_DATES::EMPLOYEE = RIGHT(MVALUE, 2%)

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
