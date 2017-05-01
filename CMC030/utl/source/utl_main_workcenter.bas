1	%TITLE "Work Center Profile"
	%SBTTL "UTL_MAIN_WORKCENTER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_WORKCENTER(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0198
	!
	! Abstract:HELP
	!	.p
	!	The ^*Work Center Profile\* program maintains Work Center Profile.
	!
	! Index:
	!	.x Work Center Profile
	!	.x Profile>Work Center
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_WORKCENTER/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_WORKCENTER
	!	$ DELETE UTL_MAIN_WORKCENTER.OBJ;*
	!
	! Author:
	!
	!	02/17/89 - J. Shad Rydalch
	!
	! Modification history:
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
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_WORKCENTER.HB"
	MAP (UTL_WORKCENTER)	UTL_WORKCENTER_CDD	UTL_WORKCENTER
	MAP (UTL_WORKCENTER_OLD) UTL_WORKCENTER_CDD	UTL_WORKCENTER_OLD
	MAP (UTL_WORKCENTER_DEF) UTL_WORKCENTER_CDD	UTL_WORKCENTER_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_WORKCENTER) &
		UTL_WORKCENTER.CH%, &
		UTL_WORKCENTER.READONLY%

	MAP (CH_UTL_LOCATION) &
		UTL_LOCATION.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_UTL_WORKCENTER) RARRAY_RECORD RARRAY(1000%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Work Center Profile"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "UTL_MAIN_WORKCENTER"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 11%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "WorkCenter"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 8%
		SMG_WINDOW::VHPOS = 4%
		SMG_WINDOW::VVPOS = 11%

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
		IF UTL_WORKCENTER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_WORKCENTER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_WORKCENTER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_WORKCENTER = ERR
			CONTINUE 770
		END WHEN

		UTL_WORKCENTER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_WORKCENTER.OPN"
		USE
			UTL_MAIN_WORKCENTER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_WORKCENTER.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_WORKCENTER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_WORKCENTER.CH%
		WHEN ERROR IN
			RESET #UTL_WORKCENTER.CH%
			GET #UTL_WORKCENTER.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	02,05, "(01) Work Center", &
			03,05, "(02) Description", &
			04,05, "(03) Phone", &
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
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Work Center\*
	!	.p
	!	The ^*Work Center\* field will contain a user defined code for
	!	this particular Work Center.
	!	.p
	!	The field may contain four (4) alphanumeric characters.
	!
	! Index:
	!	.x Work Center>Code
	!
	!--

			UTL_WORKCENTER::WORK_CENTER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$,	UTL_WORKCENTER::WORK_CENTER, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field enters a
	!	description for the Work Center field (01).
	!	.p
	!	The field may contain twenty (20) alphanumeric characters.
	!
	! Index:
	!	.x Work Center>Description
	!
	!--

			UTL_WORKCENTER::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, UTL_WORKCENTER::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Phone\*
	!	.p
	!	The ^*Phone\* field enters the phone number for
	!	this specific work center of the company.
	!	.p
	!	The field may contain ten (10) numeric characters.
	!
	! Index:
	!	.x Work Center>Phone
	!	.x Phone>Work Center
	!
	!--

			UTL_WORKCENTER::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;25", TEMP$, UTL_WORKCENTER::PHONE, MFLAG, &
				0%, MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_WORKCENTER = 0%

	!
	! Set UTL_WORKCENTER_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_WORKCENTER_OLD = UTL_WORKCENTER

	!
	! Restore UTL_WORKCENTER_OLD value
	!
	CASE OPT_RESETOLD
		UTL_WORKCENTER = UTL_WORKCENTER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_WORKCENTER_DEF = UTL_WORKCENTER

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_WORKCENTER = UTL_WORKCENTER_DEF
		UTL_WORKCENTER::LOCATION = &
			LEFT(MVALUE, LEN(UTL_WORKCENTER::LOCATION))
		UTL_WORKCENTER::DEPT_NUM= &
			RIGHT(MVALUE, LEN(UTL_WORKCENTER::LOCATION) + 1%)

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Work   Description                   " + &
				"             Phone"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,051"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_WORKCENTER::WORK_CENTER + "   " + &
				UTL_WORKCENTER::DESCRIPTION + "   " + &
				PRNT_PHONE(UTL_WORKCENTER::PHONE, 0%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_WORKCENTER.CH%, &
				KEY #0% GE UTL_WORKCENTER::LOCATION + &
					UTL_WORKCENTER::DEPT_NUM + &
					UTL_WORKCENTER::WORK_CENTER, &
					REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF UTL_WORKCENTER::LOCATION + &
				UTL_WORKCENTER::DEPT_NUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			UTL_WORKCENTER::LOCATION = &
				LEFT(MVALUE, LEN(UTL_WORKCENTER::LOCATION))
			UTL_WORKCENTER::DEPT_NUM= &
				RIGHT(MVALUE, LEN(UTL_WORKCENTER::LOCATION) + 1%)

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
