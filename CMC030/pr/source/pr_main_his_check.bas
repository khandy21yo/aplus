1	%TITLE "Check Journal Maintenance"
	%SBTTL "PR_MAIN_HIS_CHECK"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_HIS_CHECK(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the Check Journal file.
	!
	! Index:
	!	.x Check Journal Maintenance
	!	.x Maintenance>Check Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_HIS_CHECK/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_HIS_CHECK
	!	$ DELETE PR_MAIN_HIS_CHECK.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - B. Craig Larsen
	!
	!	10/04/89 - Kevin Handy
	!		Taken from PR_MAIN_TRN_CHECK (as read only).
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	08/25/89 - Kevin Handy
	!		Modified to allow reading of posted folders.
	!
	!	10/24/89 - Kevin Handy
	!		Modified to allow editing data.
	!
	!	11/16/89 - Kevin Handy
	!		Modified to allow erasing records.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/18/95 - Kevin Handy
	!		Added batch number to screen.
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	MAP (PR_HIS_CHECK)	PR_HIS_CHECK_CDD	PR_HIS_CHECK
	MAP (PR_HIS_CHECK_OLD) PR_HIS_CHECK_CDD PR_HIS_CHECK_OLD, PR_HIS_CHECK2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8%, &
		CLOSE_FLAG%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_HIS_CHECK) &
		PR_HIS_CHECK.CH%, &
		PR_HIS_CHECK.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_HIS_CHECK) RARRAY_RECORD RARRAY(300%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items"
		SMG_WINDOW::NHELP = "PR_MAIN_HIS_CHECK"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 5%
		IF CLOSE_FLAG%
		THEN
			SMG_WINDOW::FLAGS = 2%
		ELSE
			SMG_WINDOW::FLAGS = 0%
		END IF

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%
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
		IF PR_HIS_CHECK.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_HIS_CHECK.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.cre"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_HIS_CHECK = ERR
			CONTINUE 770
		END WHEN

		PR_HIS_CHECK.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
		USE
			PR_MAIN_HIS_CHECK = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_HIS_CHECK.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_HIS_CHECK.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_HIS_CHECK.CH%
		WHEN ERROR IN
			RESET #PR_HIS_CHECK.CH%
			GET #PR_HIS_CHECK.CH%, REGARDLESS
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
			"  (01)         (02)       (03)               " + &
			"(04)         (05)" + SPACE$(29%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  End Date     Check  #   Check Date         " + &
			"Pay Freq     Batch" + SPACE$(15%), &
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
		FOR I% = 1% TO 4%

			A% = VAL%(MID("014,025,038,055", I% * 4% - 3%, 3%))

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
	!	^*(01) End Date\*
	!	.p
	!	The ^*End Date\* refers to the date designated are the payroll period
	!	ending date.
	!
	! Index:
	!	.x End Date>Check Journal Maintenance
	!	.x Check Journal Maintenance>End Date
	!
	!--
			PR_HIS_CHECK::PR_END_DATE = BATCH_NO$ &
				IF PR_HIS_CHECK::PR_END_DATE = ""
			PR_HIS_CHECK::PR_END_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_HIS_CHECK::PR_END_DATE, MFLAG, "8", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Check _#\*
	!	.p
	!	The ^*Check _#\* refers to the number of the check used in the payroll
	!	payment for the period.
	!
	! Index:
	!	.x Check Number>Check Journal Maintenance
	!	.x Check Journal Maintenance>Check Number
	!
	!--
			PR_HIS_CHECK::CHECK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";16", TEMP$, &
				PR_HIS_CHECK::CHECK, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Check Date\*
	!	.p
	!	The ^*Check Date\* refers to the date that the check was written for the
	!	payment.
	!
	! Index:
	!	.x Check Date>Check Journal Maintenance
	!	.x Check Journal Maintenance>Check Date
	!	.x Date>Check
	!
	!--
			PR_HIS_CHECK::CHECK_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";27", TEMP$, &
				PR_HIS_CHECK::CHECK_DATE, MFLAG, "8", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Pay Frequency\*
	!	.p
	!	The ^*Pay Frequency\* refers to the frequency of the payments to the employee.
	!	Common frequencies are weekly, bi-weekly, and monthly.
	!
	! Index:
	!	.x Pay Frequency>Check Journal Maintenance
	!	.x Check Journal Maintenance>Pay Frequency
	!	.x Frequency>Pay
	!
	!--
			PR_HIS_CHECK::PAYFREQ = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";48", TEMP$, &
				PR_HIS_CHECK::PAYFREQ * 1.0, MFLAG, &
				"###", MVALUE)


		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Batch _#\*
	!	.p
	!
	! Index:
	!	.x Batch Number>Check Journal Maintenance
	!	.x Check Journal Maintenance>Batch Number
	!
	!--
			PR_HIS_CHECK::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";59", TEMP$, &
				PR_HIS_CHECK::BATCH, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_HIS_CHECK = 0%

	!
	! Test option
	!
 !	CASE OPT_TESTOPT
 !		FUNC_FLAG% = 0%


	!
	! Set PR_HIS_CHECK_OLD value
	!
20500	CASE OPT_SETOLD
		PR_HIS_CHECK_OLD = PR_HIS_CHECK

	!
	! Restore PR_HIS_CHECK_OLD value
	!
	CASE OPT_RESETOLD
		PR_HIS_CHECK = PR_HIS_CHECK_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_HIS_CHECK2 = PR_HIS_CHECK

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_HIS_CHECK = PR_HIS_CHECK2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_HIS_CHECK::EMPNUM = PR_EMP_MASTER::EMPNUM


	!
	! Find
	!
	CASE OPT_FIND
		FIND #PR_HIS_CHECK.CH%, KEY #0% &
			GE PR_HIS_CHECK::EMPNUM + PR_HIS_CHECK::PR_END_DATE, &
			REGARDLESS

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
				FIND #PR_HIS_CHECK.CH%, KEY #0% &
					GE PR_EMP_MASTER::EMPNUM + "", &
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

			IF (PR_HIS_CHECK::EMPNUM = PR_EMP_MASTER::EMPNUM)
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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_HIS_CHECK::EMPNUM = MID(MVALUE, 2%, 10%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
