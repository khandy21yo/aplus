1	%TITLE "Pacific Pride Transaction Type Maintenance"
	%SBTTL "PP_MAIN_TRANTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_TRANTYPE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1993 BY
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
	!	.b
	!	.lm +5
	!	The ^*Transaction Type Maintenance\* option
	!	maintains the file where transaction types and descriptions
	!	are stored.  They may be viewed, modified or erased according
	!	to user specifications.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_TRANTYPE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PP_MAIN_TRANTYPE
	!	$ DELETE PP_MAIN_TRANTYPE.OBJ;*
	!
	!
	! Author:
	!
	!	01/11/93 - Dan Perkins
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
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
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include Main Window
	!
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE
	MAP (PP_TRANTYPE_OLD)	PP_TRANTYPE_CDD		PP_TRANTYPE_OLD, &
							PP_TRANTYPE2
	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PP_TRANTYPE) &
		PP_TRANTYPE.CH%, &
		PP_TRANTYPE.READONLY%

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
		SMG_WINDOW::DESCR = "Transaction Type Maintenance Table"
		SMG_WINDOW::NHELP = "PP_MAIN_TRANTYPE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Tran_type"
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
		! Declare order type channels
		!
		IF PP_TRANTYPE.CH% > 0%
		THEN
			!
			! If PP_TRANTYPE is already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_TRANTYPE.READONLY%

			GOTO 790
		END IF

		!
		! Open PP_TRANTYPE (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_TRANTYPE  = ERR
			CONTINUE 770
		END WHEN

		PP_TRANTYPE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open PP_TRANTYPE for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.OPN"
		USE
			PP_MAIN_TRANTYPE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_TRANTYPE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open PP_TRANTYPE, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_TRANTYPE.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PP_TRANTYPE.CH%
		WHEN ERROR IN
			RESET #PP_TRANTYPE.CH%
			GET #PP_TRANTYPE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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

		DATA	6,  1, "(01) Transaction Type", &
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
	!	.x Transaction Type
	!	^*(01) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field is provided to enter a user
	!	defined code which will identify this type of transaction.
	!	.B
	!	This field will accommodate 2 characters.
	!	.b
	!	Example:  ^*TI\* - Normal Transaction
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_TRANTYPE::TRANTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;24", TEMP$, PP_TRANTYPE::TRANTYPE, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is provided to enter a brief description
	!	for the Transaction Type entered in field (01).
	!	.b
	!	Thirty spaces are available for the description.
	!	.b
	!	Example:  TI - ^*Normal Transaction\*
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_TRANTYPE::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;24", TEMP$, PP_TRANTYPE::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PP_MAIN_TRANTYPE = 0%

	!
	! Set PP_TRANTYPE_OLD value
	!
20500	CASE OPT_SETOLD
		PP_TRANTYPE_OLD = PP_TRANTYPE

	!
	! Restore PP_TRANTYPE_OLD value
	!
	CASE OPT_RESETOLD
		PP_TRANTYPE = PP_TRANTYPE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_TRANTYPE2 = PP_TRANTYPE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_TRANTYPE = PP_TRANTYPE2

	!
	! View the PP_TRANTYPE Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "Type Description"

		CASE 2%
			MVALUE = "005"

		CASE 3%
			MVALUE = &
				PP_TRANTYPE::TRANTYPE + " " + &
				PP_TRANTYPE::DESCRIPTION

		END SELECT
	!
	! Find the PP_TRANTYPE Record.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PP_TRANTYPE.CH%, KEY #0% GE &
				PP_TRANTYPE::TRANTYPE + "", REGARDLESS

		CASE 1%
			FIND #PP_TRANTYPE.CH%, KEY #1% &
				GE PP_TRANTYPE::DESCRIPTION + "", REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

	%PAGE

	!
	! End of Function
	!
32767	END FUNCTION
