1	%TITLE "Maintain Carrier Table"
	%SBTTL "UT_MAIN_CARRIER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UT_MAIN_CARRIER(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Maintain Carrier Table\* option
	!	accesses the file where carriers are
	!	maintained.
	!	.lm -5
	!
	! Index:
	!	.x Table>Carrier
	!	.x Carrier>Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_MAIN_CARRIER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UT_MAIN_CARRIER
	!	$ DELETE UT_MAIN_CARRIER.OBJ;*
	!
	!
	! Author:
	!
	!	03/09/90 - Kevin Handy
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
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD	UTL_CARRIER
	MAP (UTL_CARRIER_OLD)	UTL_CARRIER_CDD	UTL_CARRIER_OLD, UTL_CARRIER2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_CARRIER) &
		UTL_CARRIER.CH%, &
		UTL_CARRIER.READONLY%

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
		SMG_WINDOW::DESCR = "Carrier Maintenance"
		SMG_WINDOW::NHELP = "UT_MAIN_CARRIER"
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
		IF UTL_CARRIER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_CARRIER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UT_MAIN_CARRIER  = ERR
			CONTINUE 770
		END WHEN

		UTL_CARRIER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.OPN"
		USE
			UT_MAIN_CARRIER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_CARRIER.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_CARRIER.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = UTL_CARRIER.CH%
		WHEN ERROR IN
			RESET #UTL_CARRIER.CH%
			GET #UTL_CARRIER.CH%, REGARDLESS
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

		DATA	6,  1, "(01) Carrier Code", &
			8,  1, "(02) Description", &
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
	!	.x Carrier>Code
	!	^*(01) Carrier Code\*
	!	.b
	!	.lm +5
	!	The ^*Carrier Code\* field enters a user defined
	!	code which identifies a particular carrier.
	!	.b
	!	Example: ^*AE\* - Air Express
	!	.b
	!	The field will accept two (02) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Code>Carrier
	!
	!--

			UTL_CARRIER::CODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				UTL_CARRIER::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Carrier>Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief description
	!	for the carrier code entered in field (01).
	!	.b
	!	Example:  AE - ^*Air Express\*
	!	.b
	!	The field will accept up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Carrier
	!
	!--

			UTL_CARRIER::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;22", TEMP$, &
				UTL_CARRIER::DESCR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		UT_MAIN_CARRIER = 0%

20500	CASE OPT_SETOLD
		UTL_CARRIER_OLD = UTL_CARRIER

	CASE OPT_RESETOLD
		UTL_CARRIER = UTL_CARRIER_OLD

	CASE OPT_SETDEFAULT
		UTL_CARRIER2 = UTL_CARRIER

	CASE OPT_RESETDEFAULT
		UTL_CARRIER = UTL_CARRIER2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "Code Description"

		CASE 2%
			MVALUE = "005"

		CASE 3%
			MVALUE = &
				UTL_CARRIER::CODE + " " + &
				UTL_CARRIER::DESCR

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_CARRIER.CH%, &
				KEY #0% GE UTL_CARRIER::CODE + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_CARRIER.CH%, &
				KEY #1% GE UTL_CARRIER::DESCR + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
