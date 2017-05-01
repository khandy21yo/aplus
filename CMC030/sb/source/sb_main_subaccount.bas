1	%TITLE "Sub Account Maintenance"
	%SBTTL "SB_MAIN_SUBACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_MAIN_SUBACCOUNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	This program maintains Sub Account Maintenance file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_MAIN_SUBACCOUNT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SB_MAIN_SUBACCOUNT
	!	$ DELETE SB_MAIN_SUBACCOUNT.OBJ;*
	!
	! Author:
	!
	!	02/03/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/19/96 - Kevin Handy
	!		Added two more subaccount codes: S and E.
	!		Clean up source code.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)		SB_SUBACCOUNT_CDD	SB_SUBACCOUNT
	MAP (SB_SUBACCOUNT_OLD)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_OLD
	MAP (SB_SUBACCOUNT_DEF)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_TYPE.HB"
	MAP	(SB_TYPE)	SB_TYPE_CDD	SB_TYPE

	%INCLUDE "SOURCE:[SB.OPEN]SB_CLASS.HB"
	MAP	(SB_CLASS)	SB_CLASS_CDD	SB_CLASS

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%, &
		SB_SUBACCOUNT.READONLY%

	COM (TT_SB_SUBACCOUNT) &
		STITLE$ = 30%, &
		SSTAT$(5%) = 30%, &
		RTITLE$ = 30%, &
		RSUBJ$(6%) = 30%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

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

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Subaccount Description"
		SMG_WINDOW::NHELP = "SB_MAIN_SUBACCOUNT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 8%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Subject"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%
		SMG_WINDOW::KNAME(2%) = "Class"
			SMG_WINDOW::KFIELD(2%, 0%) = 3%
			SMG_WINDOW::KFIELD(2%, 1%) = 5%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
			SMG_WINDOW::KFIELD(2%, 3%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "C      Closed"

		RTITLE$ = "Subject   Description"
		RSUBJ$(0%) = "4"
		RSUBJ$(1%) = "J       Job"
		RSUBJ$(2%) = "P       Program"
		RSUBJ$(3%) = "S       Salesman"
		RSUBJ$(4%) = "E       Equipment"

		!
		! Read Defaults
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_SUBACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_SUBACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SB_MAIN_SUBACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SB_SUBACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			SB_MAIN_SUBACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_SUBACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_SUBACCOUNT.CH%
		WHEN ERROR IN
			RESET #SB_SUBACCOUNT.CH%
			GET #SB_SUBACCOUNT.CH%, REGARDLESS
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


		DATA	03,05, "(01) Subject", &
			04,05, "(02) Subaccount #", &
			05,05, "(03) Description", &
			06,05, "(04) Subacct Type", &
			07,05, "(05) Subacct Class", &
			08,05, "(06) Onset Date", &
			09,05, "(07) Current Status", &
			10,05, "(08) Termination Date", &
			0, 0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			SB_SUBACCOUNT::SUBJECT = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;30", TEMP$, &
				SB_SUBACCOUNT::SUBJECT, MFLAG, "!", &
				MVALUE, RSUBJ$(), RTITLE$, "008"), -1%)

		CASE 2%

			SB_SUBACCOUNT::SUBACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;30",TEMP$, SB_SUBACCOUNT::SUBACCOUNT, &
				MFLAG, "'E", MVALUE)

		CASE 3%

			SB_SUBACCOUNT::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, SB_SUBACCOUNT::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 4%

			SB_SUBACCOUNT::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;30",	TEMP$, SB_SUBACCOUNT::TTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SB_MAIN_TYPE.ID, "V0") = 1%
				THEN
					SB_SUBACCOUNT::TTYPE = SB_TYPE::TTYPE
				END IF
				GOTO ReEnter
			END IF

		CASE 5%

			SB_SUBACCOUNT::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, SB_SUBACCOUNT::CLASS, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SB_MAIN_CLASS.ID, "V0") = 1%
				THEN
					SB_SUBACCOUNT::CLASS = SB_CLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

		CASE 6%

			SB_SUBACCOUNT::BDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;30", TEMP$, SB_SUBACCOUNT::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 7%

			SB_SUBACCOUNT::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;30", TEMP$, &
				SB_SUBACCOUNT::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 8%

			SB_SUBACCOUNT::EDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30",TEMP$, SB_SUBACCOUNT::EDATE, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SB_MAIN_SUBACCOUNT = 0%

		SELECT MLOOP

		CASE 2%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						SB_SUBACCOUNT::SUBJECT + &
						SB_SUBACCOUNT::SUBACCOUNT, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				SB_MAIN_SUBACCOUNT = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		CASE 4%
			IF SB_SUBACCOUNT::TTYPE <> ""
			THEN
				!
				! Is the Type defined?
				!
				SB_MAIN_SUBACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					SB_SUBACCOUNT::TTYPE, &
					SB_TYPE::DESCR, &
					"SB", SCOPE::PRG_PROGRAM, "TYPE", &
					"Type", SB_MAIN_TYPE.ID)

			END IF

		CASE 5%
			IF SB_SUBACCOUNT::CLASS <> ""
			THEN
				!
				! Is the Type defined?
				!
				SB_MAIN_SUBACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					SB_SUBACCOUNT::CLASS, &
					SB_CLASS::DESCR, &
					"SB", SCOPE::PRG_PROGRAM, "CLASS", &
					"Class", SB_MAIN_CLASS.ID)

			END IF

		END SELECT


	!
	! Set SB_SUBACCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		SB_SUBACCOUNT_OLD = SB_SUBACCOUNT

	!
	! Restore SB_SUBACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		SB_SUBACCOUNT = SB_SUBACCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SB_SUBACCOUNT_DEF = SB_SUBACCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SB_SUBACCOUNT = SB_SUBACCOUNT_DEF
	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%

			MVALUE ="  Sb SubAcct    " + &
				"Description                    " + &
				"Ty Clas O Date     S C Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "005,016,047,050,055,066,068"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SB_SUBACCOUNT::SUBJECT	+ "  " + &
				SB_SUBACCOUNT::SUBACCOUNT + " " + &
				LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
				SB_SUBACCOUNT::TTYPE + " " + &
				SB_SUBACCOUNT::CLASS + " " + &
				PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
				SB_SUBACCOUNT::SSTATUS + " " + &
				PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SB_SUBACCOUNT::SUBJECT + &
				SB_SUBACCOUNT::SUBACCOUNT, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE SB_SUBACCOUNT::SUBJECT + &
				SB_SUBACCOUNT::TTYPE + &
				SB_SUBACCOUNT::SUBACCOUNT, &
				REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE SB_SUBACCOUNT::SUBJECT + &
				SB_SUBACCOUNT::CLASS + &
				SB_SUBACCOUNT::SUBACCOUNT, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
