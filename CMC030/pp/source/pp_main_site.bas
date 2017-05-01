1	%TITLE "Pacific Price Site Maintenance"
	%SBTTL "PP_MAIN_SITE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_SITE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	.lm +5
	!	This function maintains the Pacific Pride Site file.  Data
	!	in this file includes a host number, site code, site type,
	!	site name, address, city, state, zip, local sale location,
	!	foreign sale location, and foreign purchase location.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_SITE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_SITE
	!	$ DELETE PP_MAIN_SITE.OBJ;*
	!
	! Author:
	!
	!	12/29/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Added line number 20300 so error trapping would
	!		make sense.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE
	MAP (PP_SITE_OLD)	PP_SITE_CDD		PP_SITE_OLD, &
							PP_SITE2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PP_SITE) &
		PP_SITE.CH%, &
		PP_SITE.READONLY%

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

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Pacific Pride Site Maintenance"
		SMG_WINDOW::NHELP = "PP_MAIN_SITE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 11%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Host"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 13%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PP_SITE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_SITE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_SITE = ERR
			CONTINUE 770
		END WHEN

		PP_SITE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.OPN"
		USE
			PP_MAIN_SITE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_SITE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_SITE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PP_SITE.CH%
		WHEN ERROR IN
			RESET #PP_SITE.CH%

			GET #PP_SITE.CH%, REGARDLESS
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

		DATA	02,01, "(01) Host", &
			03,01, "(02) Site", &
			04,01, "(03) Site Type", &
			05,01, "(04) Site Name", &
			06,01, "(05) Address", &
			07,01, "(06) City", &
			08,01, "(07) State", &
			09,01, "(08) Zip", &
			10,01, "(09) Local Sale Location", &
			11,01, "(10) Foreign Sale Location", &
			12,01, "(11) Foreign Purchase Location", &
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
	!++
	! Abstract:FLD001
	!	^*(01) Host\*
	!	.b
	!	.lm +5
	!	The ^*Host\* field is provided to enter a number which
	!	identifies a specific host.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::HOST = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, PP_SITE::HOST, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Site\*
	!	.b
	!	.lm +5
	!	The ^*Site\* field enters a site
	!	code for the host which is entered in field (01).
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::SITE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, PP_SITE::SITE, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Site Type\*
	!	.b
	!	.lm +5
	!	The ^*Site Type\* field identifies the type of site.
	!	.b
	!	This field will accept 1 alpha-numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::STYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, PP_SITE::STYPE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Site Name\*
	!	.b
	!	.lm +5
	!	The ^*Site Name\* field contains the name of the Site referenced
	!	in field (02).
	!	.b
	!	This field will accept 30 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::SNAME = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, PP_SITE::SNAME, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Address\*
	!	.b
	!	.lm +5
	!	The ^*Address\* field enters the Site
	!	address.
	!	.b
	!	This field will accept 25 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::ADDRESS= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;17", TEMP$, PP_SITE::ADDRESS, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field is provided to enter the city in which the
	!	Site is located.
	!	.b
	!	This field will accept 15 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::CITY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;17", TEMP$, PP_SITE::CITY, &
				MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field is used to enter the state in which
	!	the Site is located.  A state must be defined in the State
	!	Master File in order to be used.
	!	.b
	!	Valid state codes may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::STATE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;17", TEMP$, PP_SITE::STATE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, "VX") = 1%
				THEN
					PP_SITE::STATE = &
						UTL_STATE::STATE
				END IF

				GOTO Reenter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field is provided to enter the zip or postal
	!	code for the area in which a Site is located.
	!	.b
	!	This field will accept 10 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::ZIP = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;17", TEMP$, PP_SITE::ZIP, &
				MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Local Sale Location\*
	!	.b
	!	.lm +5
	!	The ^*Local Sale Location\* field refers to the location number
	!	where a local sale was made.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::LOCSALE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;33", TEMP$, PP_SITE::LOCSALE, &
				MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Foreign Sale Location\*
	!	.b
	!	.lm +5
	!	The ^*Foreign Sale Location\* field refers to location number
	!	where a foreign sale was made.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::FORSALE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;33", TEMP$, PP_SITE::FORSALE, &
				MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Foreign Purchase Location\*
	!	.b
	!	.lm +5
	!	The ^*Foreign Purchase Location\* field refers to the location
	!	where a foreign purchase was made.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE::FORPUR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;33", TEMP$, PP_SITE::FORPUR, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PP_MAIN_SITE = 0%

		SELECT MLOOP

		CASE 7%
			!
			! Display the descriptions for state
			!
			PP_MAIN_SITE = FUNC_TESTENTRY(SMG_WINDOW, &
				"US" + PP_SITE::STATE, &
				UTL_STATE::DESCR, &
				"PP", MLOOP, "PROG", &
				"State", UTL_MAIN_STATE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 8%, 22%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			UTL_STATE::DESCR = &
				STRING$(LEN(UTL_STATE::DESCR), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
				"Q0" + "US" + PP_SITE::STATE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 8%, 22%, , SMG$M_BOLD)
		END IF

	!
	! Set PP_SITE_OLD value
	!
	CASE OPT_SETOLD
		PP_SITE_OLD = PP_SITE

	!
	! Restore PP_SITE_OLD value
	!
	CASE OPT_RESETOLD
		PP_SITE = PP_SITE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_SITE2 = PP_SITE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_SITE = PP_SITE2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Host Site Type Name                " + &
				"           State LocSal ForSal ForPur"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,012,017,048,054,061,068"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PP_SITE::HOST + "  " + &
				PP_SITE::SITE + "   " + &
				PP_SITE::STYPE + "    " + &
				PP_SITE::SNAME + " "    + &
				PP_SITE::STATE + "    " + &
				PP_SITE::LOCSALE + "    " + &
				PP_SITE::FORSALE + "    " + &
				PP_SITE::FORPUR

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PP_SITE::HOST + &
					PP_SITE::SITE + &
					PP_SITE::STYPE, REGARDLESS
			USE
				CONTINUE 32767 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
