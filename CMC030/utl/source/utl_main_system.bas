1	%TITLE "System Description"
	%SBTTL "UTL_MAIN_SYSTEM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_SYSTEM(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! ID:0194
	!
	! Abstract:HELP
	!	.p
	!	This program maintains the System description file
	!	and their Abrevations.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_SYSTEM/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_SYSTEM
	!	$ DELETE UTL_MAIN_SYSTEM.OBJ;*
	!
	! Author:
	!
	!	11/06/88 - J. Shad Rydalch
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

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM)	UTL_SYSTEM_CDD	UTL_SYSTEM
	MAP (UTL_SYSTEM_OLD)	UTL_SYSTEM_CDD	UTL_SYSTEM_OLD, UTL_SYSTEM2

	MAP (CH_UTL_MAIN_SYSTEM) UTL_SYSTEM.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*******************************************************
		! Set up information
		!*******************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "System Codes"
		SMG_WINDOW::NHELP = "UTL_MAIN_SYSTEM"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "System"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Description"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (UTL_SYSTEM.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, "Unable to open UTL_SYSTEM file " + &
					NUM1$(ERR), 0%)
				UTL_MAIN_SYSTEM = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = UTL_SYSTEM.CH%

		WHEN ERROR IN
			RESET #UTL_SYSTEM.CH%
			GET #UTL_SYSTEM.CH%, REGARDLESS
		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	06,05, "(01) System", &
			08,05, "(02) Description", &
			10,05, "(03) System number", &
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
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			UTL_SYSTEM::SYSTEM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;37", TEMP$, &
				UTL_SYSTEM::SYSTEM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_SYSTEM.ID, "V0  ") = 1%)
				THEN
					UTL_SYSTEM::SYSTEM = &
						UTL_SYSTEM::SYSTEM

				END IF
				GOTO ReEnter

			END IF


		CASE 2%

			UTL_SYSTEM::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;37", TEMP$, UTL_SYSTEM::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%

			UTL_SYSTEM::SYSTEM_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;37", TEMP$, UTL_SYSTEM::SYSTEM_NUM, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SELECT MLOOP

		CASE 1%
			IF UTL_SYSTEM::SYSTEM = ""
			THEN
				UTL_MAIN_SYSTEM = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ UTL_SYSTEM::SYSTEM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_SYSTEM = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT


	!
	! Set UTL_SYSTEM_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_SYSTEM_OLD = UTL_SYSTEM

	!
	! Restore UTL_SYSTEM_OLD value
	!
	CASE OPT_RESETOLD
		UTL_SYSTEM = UTL_SYSTEM_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_SYSTEM2 = UTL_SYSTEM

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_SYSTEM = UTL_SYSTEM2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
		!	          ->12 | 12 | 1234567890123456789012345678901234567890
			MVALUE = "  Sy   Nu   Description                             "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "006,011,"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_SYSTEM::SYSTEM + "   " + &
				UTL_SYSTEM::SYSTEM_NUM + "   " + &
				UTL_SYSTEM::DESCRIPTION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_SYSTEM::SYSTEM + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE UTL_SYSTEM::DESCRIPTION + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
