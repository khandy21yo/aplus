1	%TITLE "Purchase Order Type Definition File Maintenance"
	%SBTTL "PO_MAIN_TYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_TYPE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! any other copies thereof may not be provided or otherwise made
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
	!	.p
	!	The Purchase Order Types are determined in the ^*Maintain PO Type Table\*
	!	option in the PO Master Tables menu.
	!
	! Index:
	!	.x Table>PO Type
	!	.x PO Type>Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_TYPE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_TYPE
	!	$ DELETE PO_MAIN_TYPE.OBJ;*
	!
	! Author:
	!
	!	03/14/90 - Kevin Handy
	!
	! Modification history:
	!
	!	05/20/94 - Kevin Handy
	!		Modified so that the second key didn't look up
	!		using KEY #0%.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR NI
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.HB"
	MAP (PO_TYPE)	PO_TYPE_CDD	PO_TYPE
	MAP (PO_TYPE_OLD)	PO_TYPE_CDD	PO_TYPE_OLD, PO_TYPE2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_TYPE) &
		PO_TYPE.CH%, &
		PO_TYPE.READONLY%

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
		SMG_WINDOW::DESCR = "PO Type Maintenance"
		SMG_WINDOW::NHELP = "PO_MAIN_TYPE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Type"
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
		IF PO_TYPE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_TYPE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_TYPE  = ERR
			CONTINUE 770
		END WHEN

		PO_TYPE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.OPN"
		USE
			PO_MAIN_TYPE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_TYPE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_TYPE.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PO_TYPE.CH%
		WHEN ERROR IN
			RESET #PO_TYPE.CH%
			GET #PO_TYPE.CH%, REGARDLESS
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

		DATA	6,  1, "(01) PO Type", &
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
	!	^*(01) Purchase Order Type\*
	!	.p
	!	The ^*Purchase Order Type\* field enters a user defined
	!	number which will identify a particular type of PO.
	!	.p
	!	The field provides two (2) spaces for an alphanumeric entry.
	!
	! Index:
	!	.x PO Type
	!
	!--

			PO_TYPE::POTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				PO_TYPE::POTYPE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field enters a brief description
	!	for this particular purchase order type.
	!	.p
	!	The field will accommodate forty (40) alphanumeric characters.
	!
	! Index:
	!	.x PO Type>Description
	!	.x Description>PO Type
	!
	!--

			PO_TYPE::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;22", TEMP$, &
				PO_TYPE::DESCR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PO_MAIN_TYPE = 0%

20500	CASE OPT_SETOLD
		PO_TYPE_OLD = PO_TYPE

	CASE OPT_RESETOLD
		PO_TYPE = PO_TYPE_OLD

	CASE OPT_SETDEFAULT
		PO_TYPE2 = PO_TYPE

	CASE OPT_RESETDEFAULT
		PO_TYPE = PO_TYPE2

	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "Code Description"

		CASE 2%
			MVALUE = "005"

		CASE 3%
			MVALUE = &
				PO_TYPE::POTYPE + " " + &
				PO_TYPE::DESCR

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			WHEN ERROR IN
				FIND #PO_TYPE.CH%, &
					KEY #0% GE PO_TYPE::POTYPE + "", &
					REGARDLESS
			USE
				CONTINUE 32767 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		CASE 1%
			WHEN ERROR IN
				FIND #PO_TYPE.CH%, &
					KEY #1% GE PO_TYPE::DESCR + "", &
					REGARDLESS
			USE
				CONTINUE 32767 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	ON ERROR GO BACK

	%PAGE

32767	END FUNCTION
