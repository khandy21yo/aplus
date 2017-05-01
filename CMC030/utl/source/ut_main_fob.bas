1	%TITLE "Maintain FOB Table"
	%SBTTL "UT_MAIN_FOB"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UT_MAIN_FOB(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The FOB Codes are maintained through the ^*Maintain FOB Table\*
	!	option.
	!	.lm -5
	!
	! Index:
	!	.x Table>FOB Codes
	!	.x FOB Codes>Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_MAIN_FOB/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UT_MAIN_FOB
	!	$ DELETE UT_MAIN_FOB.OBJ;*
	!
	!
	! Author:
	!
	!	05/31/90 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.HB"
	MAP (UTL_FOB)		UTL_FOB_CDD	UTL_FOB
	MAP (UTL_FOB_OLD)	UTL_FOB_CDD	UTL_FOB_OLD, UTL_FOB2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_FOB) &
		UTL_FOB.CH%, &
		UTL_FOB.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "FOB Code Maintenance"
		SMG_WINDOW::NHELP = "UT_MAIN_FOB"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Description"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF UTL_FOB.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_FOB.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UT_MAIN_FOB  = ERR
			CONTINUE 770
		END WHEN

		UTL_FOB.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.OPN"
		USE
			UT_MAIN_FOB = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_FOB.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_FOB.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = UTL_FOB.CH%
		WHEN ERROR IN
			RESET #UTL_FOB.CH%
			GET #UTL_FOB.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************

	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	6,  5, "(01) FOB Code", &
			8,  5, "(02) Description", &
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

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x FOB>Code
	!	^*(01) FOB Code\*
	!	.b
	!	.lm +5
	!	The ^*FOB Code\* field enters a user defined
	!	number which identifies a particular code description.
	!	.b
	!	Two (02) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Code>FOB
	!
	!--

			UTL_FOB::FOBCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;25", TEMP$, &
				UTL_FOB::FOBCODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x FOB Code>Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief description
	!	of the FOB Code entered in field (01).
	!	.b
	!	The field will accommodate forty (40) characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>FOB Code
	!
	!--

			UTL_FOB::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;25", TEMP$, &
				UTL_FOB::DESCR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		UT_MAIN_FOB = 0%

20500	CASE OPT_SETOLD
		UTL_FOB_OLD = UTL_FOB

	CASE OPT_RESETOLD
		UTL_FOB = UTL_FOB_OLD

	CASE OPT_SETDEFAULT
		UTL_FOB2 = UTL_FOB

	CASE OPT_RESETDEFAULT
		UTL_FOB = UTL_FOB2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "Code Description"

		CASE 2%
			MVALUE = "005"

		CASE 3%
			MVALUE = UTL_FOB::FOBCODE + " " + UTL_FOB::DESCR

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_FOB.CH%, &
				KEY #0% GE UTL_FOB::FOBCODE + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_FOB.CH%, &
				KEY #1% GE UTL_FOB::DESCR + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************

	ON ERROR GO BACK

	%PAGE

32767	END FUNCTION
