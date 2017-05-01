1	%TITLE "PR Rate Range File Maintenance"
	%SBTTL "PR_MAIN_RATERANGE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_RATERANGE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988, 1989 BY
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
	!	The ^*Rate Range Table\* maintains the rate range table.
	!
	! Index:
	!	.x Rate Range Table>Maintenance
	!	.x Maintenance>Rate Range Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_RATERANGE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_RATERANGE
	!	$ DELETE PR_MAIN_RATERANGE.OBJ;*
	!
	! Author:
	!
	!	06/01/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.HB"
	MAP (PR_RATERANGE)	PR_RATERANGE_CDD	PR_RATERANGE
	MAP (PR_RATERANGE2) PR_RATERANGE_CDD PR_RATERANGE_OLD, PR_RATERANGE2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the MAIN program and
	! in PR_MAST_RATERANGE.
	!
	COM (CH_PR_RATERANGE) &
		PR_RATERANGE.CH%, &
		PR_RATERANGE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "PR Rate Range Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_RATERANGE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

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
		IF PR_RATERANGE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_RATERANGE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_RATERANGE = ERR
			CONTINUE 770
		END WHEN

		PR_RATERANGE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.OPN"
		USE
			PR_MAIN_RATERANGE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_RATERANGE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_RATERANGE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_RATERANGE.CH%
		WHEN ERROR IN
			RESET #PR_RATERANGE.CH%
			GET #PR_RATERANGE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05, 05, "(01) Location", &
			06, 05, "(02) Age", &
			07, 05, "(03) Minimum Rate", &
			08, 05, "(04) Maximum Rate", &
			0,  0, ""

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

 Reenter1:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field refers to the geographical location of the business
	!	establishment. This field is important to the setting of the rate range and
	!	must be entered in numeric form.
	!	.lm -5
	!
	! Index:
	!	.x Location>Rate Range Table
	!	.x Rate Range Table>Location
	!
	!--
			PR_RATERANGE::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;30", TEMP$, &
				PR_RATERANGE::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					PR_RATERANGE::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter1
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Age\*
	!	.b
	!	.lm +5
	!	The ^*Age\* field refers to the chronological age of the employee. This
	!	entry is important as it is the basis of the rate range.
	!	.lm -5
	!
	! Index:
	!	.x Age>Rate Range Table
	!	.x Rate Range Table>Age
	!
	!--
			PR_RATERANGE::AGE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;30", TEMP$, &
				PR_RATERANGE::AGE, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Minimum Rate\*
	!	.b
	!	.lm +5
	!	The ^*Minimum Rate\* field refers to the minimum rate that can be paid to
	!	the employee within the specified age and location parameters.
	!	.lm -5
	!
	! Index:
	!	.x Minimum Rate>Rate Range Table
	!	.x Rate Range Table>Minimum Rate
	!
	!--
			PR_RATERANGE::MIN_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;30", TEMP$, &
				PR_RATERANGE::MIN_RATE * 1.0, MFLAG, &
				"####.###", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Maximum Rate\*
	!	.p
	!	The ^*Maximum Rate\* field refers to the maximum rate that may be paid to
	!	an employee within specified age and location parameters.
	!
	! Index:
	!	.x Maximum Rate>Rate Range Table
	!	.x Rate Range Table>Maximum Rate
	!
	!--
			PR_RATERANGE::MAX_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;30", TEMP$, &
				PR_RATERANGE::MAX_RATE * 1.0, MFLAG, &
				"####.###", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_RATERANGE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_MAIN_RATERANGE = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_RATERANGE::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"PR", MLOOP, "PRG", &
				"Location", UTL_MAIN_LOCATION.ID)

		CASE 2%
20330			WHEN ERROR IN
				AGE% = VAL%(PR_RATERANGE::AGE)
			USE
				SELECT ERR
				CASE 51%, 52%
					PR_MAIN_RATERANGE = 1%
					CONTINUE ExitFunction
				END SELECT
				EXIT HANDLER
			END WHEN

			IF AGE% = 0%
			THEN
				PR_MAIN_RATERANGE = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Zero age not allowed", 1%)
			END IF

			IF MVALUE = "ADD"
			THEN
				GET #PR_RATERANGE.CH%, KEY #0% EQ &
					PR_RATERANGE::LOCATION + &
					PR_RATERANGE::AGE, REGARDLESS

				PR_MAIN_RATERANGE = 2%
			END IF

		END SELECT

	CASE OPT_DISPLAY

	!
	! Set PR_RATERANGE_OLD value
	!
20500	CASE OPT_SETOLD
		PR_RATERANGE_OLD = PR_RATERANGE

	!
	! Restore PR_RATERANGE_OLD value
	!
	CASE OPT_RESETOLD
		PR_RATERANGE = PR_RATERANGE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_RATERANGE2 = PR_RATERANGE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_RATERANGE = PR_RATERANGE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Loca   Age   Minimum Rate   " + &
				"Maximum Rate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,014,029"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE =PR_RATERANGE::LOCATION	+ "   " + &
				PR_RATERANGE::AGE &
				+ "       " + &
				FORMAT$(PR_RATERANGE::MIN_RATE, "####.###") &
				+ "       " + &
				FORMAT$(PR_RATERANGE::MAX_RATE, "####.###")

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #PR_RATERANGE.CH%, KEY #0% &
				GE PR_RATERANGE::LOCATION + &
				PR_RATERANGE::AGE, REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	RESUME ExitFunction

32767	END FUNCTION
