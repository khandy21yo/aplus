1	%TITLE "Era Description"
	%SBTTL "UTL_MAIN_ERA"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_ERA(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! ID:0160
	!
	! Abstract:HELP
	!	.p
	!	The ^*Era\* program maintains the Era Description file
	!	(the header file for the Period file).
	!
	! Index:
	!	.x Maintain>Company Profile Era
	!	.x Era>Profile>Company>Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_ERA/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_ERA
	!	$ DELETE UTL_MAIN_ERA.OBJ;*
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
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/05/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)		UTL_ERA_CDD	UTL_ERA
	MAP (UTL_ERA_OLD)	UTL_ERA_CDD	UTL_ERA_OLD
	MAP (UTL_ERA_DEF)	UTL_ERA_CDD	UTL_ERA_DEF

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_ERA) &
		UTL_ERA.CH%, &
		UTL_ERA.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Accounting Era Description"
		SMG_WINDOW::NHELP = "UTL_MAIN_ERA"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Era"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

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
		IF UTL_ERA.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_ERA.READONLY%
			GOTO 790
		END IF

		IF (UTL_ERA.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.CRE"
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_ERA = ERR
			CONTINUE 770
		END WHEN

		UTL_ERA.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.OPN"
		USE
			UTL_MAIN_ERA = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_ERA.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_ERA.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_ERA.CH%
		WHEN ERROR IN
			RESET #UTL_ERA.CH%
			GET #UTL_ERA.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	02,05, "(01) Era Code", &
			03,05, "(02) Description", &
			04,05, "(03) Initial Date", &
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

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Era Code\*
	!	.p
	!	The ^*Era Code\* field
	!	enters an Era Code which identifies a
	!	specific era definition. For example, if an era which defined
	!	the accounting periods for book depreciation were to be entered,
	!	the code might be "BK".
	!	.p
	!	The field will accommodate two (2) alphanumeric characters.
	!
	! Index:
	!	.x Era>Code
	!	.x Code>Era
	!	.x Company Profile>Era>Code
	!	.x Profile>Company>Era>Code
	!
	!--

			UTL_ERA::ERA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;27", TEMP$, &
				UTL_ERA::ERA, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a Description for a specific era.
	!	.p
	!	This field accommodates up to thirty (30) alphanumeric characters.
	!
	! Index:
	!	.x Era>Description
	!	.x Company Profile>Era>Description
	!	.x Profile>Company>Era>Description
	!
	!--

			UTL_ERA::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;27", TEMP$, &
				UTL_ERA::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Initial Date\*
	!	.p
	!	The ^*Initial Date\* field
	!	enters a date which represents the beginning
	!	of an era or the date which represents the initialization of the
	!	related system.
	!	.p
	!	The format for data entry is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Era>Initial Date
	!	.x Company Profile>Era>Initial Date
	!	.x Profile>Company>Era>Initial Date
	!
	!--

			UTL_ERA::BEG_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;27", TEMP$, &
				UTL_ERA::BEG_DATE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_ERA = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_ERA::ERA = ""
			THEN
				UTL_MAIN_ERA = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ UTL_ERA::ERA + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_ERA = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set UTL_ERA_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_ERA_OLD = UTL_ERA

	!
	! Restore UTL_ERA_OLD value
	!
	CASE OPT_RESETOLD
		UTL_ERA = UTL_ERA_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_ERA_DEF = UTL_ERA

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_ERA = UTL_ERA_DEF

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code Description          InitDate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,028,037"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_ERA::ERA + "   " + &
				UTL_ERA::DESCRIPTION + " " + &
				UTL_ERA::BEG_DATE

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_ERA::ERA + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More Menu option hidden in Mast.
	!++
	! Abstract:PERIOD
	!	^*Period\*
	!	.p
	!	The ^*Period\* option
	!	maintains the Period Description file.
	!
	! Index:
	!
	!--
