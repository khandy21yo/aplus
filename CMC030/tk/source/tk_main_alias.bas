1	%TITLE "Programmer Alias"
	%SBTTL "TK_MAIN_ALIAS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_ALIAS(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! ID:7826
	!
	! Abstract:HELP
	!	.P
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_ALIAS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_MAIN_ALIAS
	!	$ DELETE TK_MAIN_ALIAS.OBJ;*
	!
	! Author:
	!
	!	05/24/91 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.HB"
	MAP (TK_ALIAS)		TK_ALIAS_CDD	TK_ALIAS
	MAP (TK_ALIAS_OLD)	TK_ALIAS_CDD	TK_ALIAS_OLD, TK_ALIAS2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_TK_ALIAS) &
		TK_ALIAS.CH%, &
		TK_ALIAS.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Programmer Alias"
		SMG_WINDOW::NHELP = "TK_MAIN_ALIAS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Alias"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Programmer"
		SMG_WINDOW::KFIELD(1%, 0%) = 1%
		SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

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
		IF TK_ALIAS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF TK_ALIAS.READONLY%
			GOTO 790
		END IF

		IF (TK_ALIAS.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.CRE"
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			TK_MAIN_ALIAS = ERR
			CONTINUE 770
		END WHEN

		TK_ALIAS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.OPN"
		USE
			TK_MAIN_ALIAS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		TK_ALIAS.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(TK_ALIAS.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = TK_ALIAS.CH%
		WHEN ERROR IN
			RESET #TK_ALIAS.CH%
			GET #TK_ALIAS.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display SMG_WINDOW background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	8,  10, "(01) Alias", &
			10,  10, "(02) Programmer", &
			0,   0, ""

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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!
	!	^*(01) Alias\*
	!	.p
	!
	! Index:
	!--


			TK_ALIAS::ALIAS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;33", TEMP$, &
				TK_ALIAS::ALIAS, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!
	!	^*(02) Programmer\*
	!	.p
	!
	! Index:
	!--


			TK_ALIAS::PROGRAMMER = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;33", TEMP$, &
				TK_ALIAS::PROGRAMMER, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TK_MAIN_ALIAS = 0%

		SELECT MLOOP

		CASE 1%
			IF TK_ALIAS::ALIAS = ""
			THEN
				TK_MAIN_ALIAS = 1%
			END IF

		END SELECT

	!
	! Set TK_ALIAS_OLD value
	!
20500	CASE OPT_SETOLD
		TK_ALIAS_OLD = TK_ALIAS

	!
	! Restore TK_ALIAS_OLD value
	!
	CASE OPT_RESETOLD
		TK_ALIAS = TK_ALIAS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_ALIAS2 = TK_ALIAS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_ALIAS = TK_ALIAS2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Alias                          Programmer"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "033"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = TK_ALIAS::ALIAS + " " + &
				TK_ALIAS::PROGRAMMER
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_ALIAS::ALIAS + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE TK_ALIAS::PROGRAMMER + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
