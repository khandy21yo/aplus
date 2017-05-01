1	%TITLE "Period Description"
	%SBTTL "UTL_MAIN_PERIOD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_PERIOD(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0184
	!
	! Abstract:HELP
	!	.p
	!	The ^*Period Description\* program maintains the Period Description file.
	!
	! Index:
	!	.x Period Description
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_PERIOD/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_PERIOD
	!	$ DELETE UTL_MAIN_PERIOD.OBJ;*
	!
	! Author:
	!
	!	12/21/87 - Frantisek Starman
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	09/16/88 - Frantisek Starman
	!		Change primary key
	!
	!	04/22/92 - Dan Perkins
	!		Change "Status" to "Qtr" to identify which
	!		quarter of the fiscal year the cycle belongs.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/07/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Lose lines 28500-28599 (Dead Code)
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/24/2006 - Kevin Handy
	!		Added display of aging sequence number to help
	!		debug AD problems.
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.HB"
	MAP (UTL_PERIOD)	UTL_PERIOD_CDD		UTL_PERIOD
	MAP (UTL_PERIOD_OLD)	UTL_PERIOD_CDD		UTL_PERIOD_OLD, &
		UTL_PERIOD2, UTL_PERIOD3

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)		UTL_ERA_CDD		UTL_ERA

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_PERIOD) &
		UTL_PERIOD.CH%, &
		UTL_PERIOD.READONLY%


	COM (TT_UTL_PERIODSTAT) &
		CLOSETITLE$    = 20%, &
		CLOSETYPE$(4%) = 20%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_UTL_PERIOD) RARRAY_RECORD RARRAY(1000%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Accounting Period Definition"
		SMG_WINDOW::NHELP = "UTL_MAIN_PERIOD"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 12%

		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "4"
		CLOSETYPE$(1%) = "1    1st Qtr"
		CLOSETYPE$(2%) = "2    2nd Qtr"
		CLOSETYPE$(3%) = "3    3rd Qtr"
		CLOSETYPE$(4%) = "4    4th Qtr"

		UTL_PERIOD::BEG_DATE = "00000000"
		UTL_PERIOD::AGE      = "0000"

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
		IF UTL_PERIOD.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_PERIOD.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_PERIOD = ERR
			CONTINUE 770
		END WHEN

		UTL_PERIOD.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.OPN"
		USE
			UTL_MAIN_PERIOD = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_PERIOD.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_PERIOD.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_PERIOD.CH%
		WHEN ERROR IN
			RESET #UTL_PERIOD.CH%
			GET #UTL_PERIOD.CH%, REGARDLESS
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
			"  (01)     (02)   (03)                 (04)   " + &
			"(05)                            ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  FiscYear  Cycle Description          Qtr    " + &
			"EndDate    Sequence             ", &
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
		FOR I% = 1% TO 5%
			A% = VAL%(MID("011,018,039,046,057", I% * 4% - 3%, 3%))

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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Fiscal Year\*
	!	.p
	!	The ^*Fiscal Year\* field
	!	identifies the fiscal year with which a
	!	specific period is associated.
	!	.p
	!	The format for data entry is YYYY.
	!
	! Index:
	!	.x Company Profile>Era>Period>Fiscal Year
	!	.x Profile>Company>Era>Period>Fiscal Year
	!	.x Era>Period>Fiscal Year
	!	.x Period>Fiscal Year>Era
	!	.x Fiscal Year>Period>Era
	!
	!--

			UTL_PERIOD::YEAR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				UTL_PERIOD::YEAR, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Cycle\*
	!	.p
	!	The ^*Cycle\* field
	!	contains the number of a specific cycle
	!	within a fiscal year.
	!	.p
	!	The field requires two (2) alphanumeric characters. A cycle
	!	could be defined as "01", "AA" or whatever is meaningful to
	!	the user.
	!
	! Index:
	!	.x Company Profile>Era>Cycle
	!	.x Profile>Company>Era>Cycle
	!	.x Cycle>Fiscal Year
	!	.x Fiscal Year>Cycle
	!
	!--

			UTL_PERIOD::CYCLE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";12", TEMP$, &
				UTL_PERIOD::CYCLE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.p
	!	The ^*Description\* field
	!	describes a specific period.
	!	.p
	!	The field will accommodate up to twenty (20) alphanumeric characters.
	!
	! Index:
	!	.x Company Profile>Period>Description
	!	.x Profile>Company>Period>Description
	!	.x Period>Description
	!	.x Description>Period
	!
	!--

			UTL_PERIOD::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";19", TEMP$, &
				UTL_PERIOD::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Quarter\*
	!	.p
	!	The ^*Quarter\* field
	!	contains the code quarter of the fiscal year to which the
	!	period belongs when initializing the system.
	!	.p
	!	Quarter code values are:
	!	.b
	!	.list 0,"*"
	!	.lm +15
	!	.le
	!	1 = First Quarter
	!	.le
	!	2 = Second Quarter
	!	.le
	!	3 = Third Quarter
	!	.le
	!	4 = Fourth Quarter
	!	.els
	!	.lm -15
	!	.p
	!	Pressing ^*<List Choices>\*, at this field, will cause a list of
	!	valid codes to be displayed.
	!
	! Index:
	!	.x Period>Status
	!	.x Status>Period
	!	.x Company Profile>Period Status
	!	.x Profile>Company>Period Status
	!
	!--

			UTL_PERIOD::PERIOD_STATUS = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";40", TEMP$, &
				UTL_PERIOD::PERIOD_STATUS, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) End Date\*
	!	.p
	!	The ^*End Date\* field
	!	contains the date which represents the last
	!	day of a particular period.
	!	.p
	!	The format for entry is ^*MMDDYYYY\* or ^*MMDDYY\*.
	!
	! Index:
	!	.x Company Profile>Period>Ending Date
	!	.x Profile>Company>Period>Ending Date
	!	.x Period>Ending Date
	!	.x Ending Date>Period
	!
	!--

			UTL_PERIOD::END_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";47", TEMP$, &
				UTL_PERIOD::END_DATE, MFLAG, "'E", MVALUE)

			!
			! Display aging period.
			! So, it gets displayed more often than necessary.
			!
			JUNK$ = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";58", TEMP$, &
				UTL_PERIOD::AGE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_PERIOD = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_PERIOD::YEAR = ""
			THEN
				UTL_MAIN_PERIOD = 1%
			ELSE
				IF LEN(EDIT$(UTL_PERIOD::YEAR, -1%)) <> 4%
				THEN
					UTL_MAIN_PERIOD = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Length must be four characters", 0%)
				END IF
			END IF

		CASE 2%
			IF UTL_PERIOD::CYCLE = ""
			THEN
				UTL_MAIN_PERIOD = 1%
			ELSE
				IF LEN(EDIT$(UTL_PERIOD::CYCLE, -1%)) <> 2%
				THEN
					UTL_MAIN_PERIOD = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Length must be two characters", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set UTL_PERIOD_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_PERIOD_OLD = UTL_PERIOD

	!
	! Restore UTL_PERIOD_OLD value
	!
	CASE OPT_RESETOLD
		UTL_PERIOD = UTL_PERIOD_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_PERIOD2 = UTL_PERIOD

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_PERIOD = UTL_PERIOD2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		UTL_PERIOD::ERA = UTL_ERA::ERA

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_PERIOD.CH%, &
				KEY #0% GE UTL_PERIOD::ERA + "", REGARDLESS
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
					KEY #0% GE UTL_ERA::ERA + "", &
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

			IF UTL_PERIOD::ERA = UTL_ERA::ERA
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
			UTL_PERIOD::ERA = RIGHT(MVALUE, 2%)

		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Not right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
