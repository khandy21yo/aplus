1	%TITLE "PR Check Time Keeper Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_TK02_CHECK"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_TK02_CHECK(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the PR Check Journal file
	!	for the time keeper.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_TK02_CHECK/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_TK02_CHECK
	!	$ DELETE PR_MAIN_TRN_TK02_CHECK.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
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
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/13/2001 - Kevin Handy
	!		Put folder date in title bar
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_TRN_CHECK_OLD)	PR_TRN_CHECK_CDD	PR_TRN_CHECK_OLD, PR_TRN_CHECK2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TRN_CHECK) &
		PR_TRN_CHECK.CH%, &
		PR_TRN_CHECK.READONLY%

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8, &
		END_DATE$ = 8, &
		BATCH_ENTRY$ = 2%, &
		LOCATION$ = 4%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_TRN_CHECK) RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Check Journal " + &
			PRNT_DATE(BATCH_NO$, 8%)
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_TK02_CHECK"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 18%
		SMG_WINDOW::LINREC = 1%

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
		IF PR_TRN_CHECK.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TRN_CHECK.READONLY%
			GOTO 790
		END IF

		!
		! Initialize the total records only when file is opened.
		!
		SMG_WINDOW::TOTREC = 0%

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TRN_TK02_CHECK = ERR
			CONTINUE 770
		END WHEN

		PR_TRN_CHECK.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
		USE
			PR_MAIN_TRN_TK02_CHECK = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TRN_CHECK.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TRN_CHECK.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TRN_CHECK.CH%
		WHEN ERROR IN
			RESET #PR_TRN_CHECK.CH%
			GET #PR_TRN_CHECK.CH%, REGARDLESS
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
			"  (01)                                    " + &
			"(02)       (03)                     ", 1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Emp #          Employee Name            " + &
			"Check#     Check Date               ", 2%, 1%, , SMG$M_REVERSE)

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
			A% = VAL%(MID("015,040,051", I% * 4% - 3%, 3%))

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

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Employee Number\*
	!	.p
	!	The ^*Employee Number\* field refers to the number given to the employee
	!	by the company to represent them in company dealings.
	!
	! Index:
	!	.x Employee Number>Timekeeper>Check
	!	.x Timekeeper>Check>Employee Number
	!
	!--
			PR_TRN_CHECK::PR_END_DATE = END_DATE$

			PR_TRN_CHECK::EMPNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_TRN_CHECK::EMPNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, "VX ") = 1%)
				THEN
					PR_TRN_CHECK::EMPNUM = PR_EMP_MASTER::EMPNUM
				END IF

				GOTO E0Loop
			END IF


		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Check _#\*
	!	.p
	!	The ^*Check _#\* field refers to the number of the check issued to the employee
	!	in any payment.
	!
	! Index:
	!	.x Check Number>Timekeeper>Check
	!	.x Timekeeper>Check>Check Number
	!
	!--
			PR_TRN_CHECK::CHECK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";43", TEMP$, &
				PR_TRN_CHECK::CHECK, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Check Date\*
	!	.p
	!	The ^*Check Date\* field refers to the date the check was written to the
	!	employee.
	!
	! Index:
	!	.x Check Date>Timekeeper>Check
	!	.x Date>Timekeeper>Check
	!	.x Timekeeper>Check>Check Date
	!
	!--
			PR_TRN_CHECK::CHECK_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";54", TEMP$, &
				PR_TRN_CHECK::CHECK_DATE, MFLAG, "8", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TEMP_FLAG%, PR_MAIN_TRN_TK02_CHECK = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_MAIN_TRN_TK02_CHECK = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_CHECK::EMPNUM, &
				PR_EMP_MASTER::EMPNAME, &
				"PR", MLOOP, "PRG", &
				"Employee Number", PR_MAIN_TK_EMP_QUERY.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%), &
				SMG_WINDOW::CURLIN, 16%,, SMG$M_BOLD)

			IF TEMP_FLAG% = 0%
			THEN
				IF EDIT$(PR_EMP_MASTER::TERMDAY, -1%) > "00000000"
				THEN
					!
					! Employee has been terminated.
					!
					TEMP_FLAG%, PR_MAIN_TRN_TK02_CHECK = 1%
					CALL ENTR_3MESSAGE(SCOPE, "Employee has a termination date ", 0%)
				END IF
			END IF

			IF TEMP_FLAG% = 0%
			THEN
				IF EDIT$(PR_EMP_MASTER::LOCATION, -1%) <> &
					LOCATION$ AND EDIT$(LOCATION$, -1%) <> ""
				THEN
					!
					! Has the employee been terminated.
					!
					PR_MAIN_TRN_TK02_CHECK = 1%
					CALL ENTR_3MESSAGE(SCOPE, "Employee does not work in this location ", 0%)
				END IF
			END IF


		END SELECT

	!
	! Set PR_TRN_CHECK_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TRN_CHECK_OLD = PR_TRN_CHECK

	!
	! Restore PR_TRN_CHECK_OLD value
	!
	CASE OPT_RESETOLD
		PR_TRN_CHECK = PR_TRN_CHECK_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TRN_CHECK2 = PR_TRN_CHECK

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TRN_CHECK = PR_TRN_CHECK2

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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%

		!
		! Handle anything extra
		!
		CASE 7%

			!
			! Is the input defined?
			!
			PR_EMP_MASTER::EMPNAME = STRING$( &
				LEN(PR_EMP_MASTER::EMPNAME), 63%) &
				IF MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, &
				"Q0" + PR_TRN_CHECK::EMPNUM) <> 1%

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%), &
				SMG_WINDOW::CURLIN, 16%,, SMG$M_BOLD)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
