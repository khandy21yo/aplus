1	%TITLE "Profile Maintenance"
	%SBTTL "UTL_MAIN_PROFILE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_PROFILE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0188
	!
	! Abstract:HELP
	!	.p
	!	^*Profile Maintenance\*
	!	.p
	!	The ^*Company Profile Maintenance\* option
	!	maintains general profile information concerning a company
	!	using CMC's software. The information will be printed on various
	!	reports as necessary.
	!
	! Index:
	!	.x Maintain>Company Profile
	!	.x Profile>Company>Maintenance
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_PROFILE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_PROFILE
	!	$ DELETE UTL_MAIN_PROFILE.OBJ;*
	!
	! Author:
	!
	!	09/09/87 - Kevin Handy
	!
	! Modification history:
	!
	!	12/01/87 - Frank Starman
	!		Add UTL_MAIN_LOCATION
	!
	!	04/05/88 - Kevin Handy
	!		Yes, It does create the set file if it does
	!		not already exist.
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/16/88 - Kevin Handy
	!		Fixed search for record 1
	!
	!	08/12/88 - Kevin Handy
	!		Modified to have device open properly in the
	!		first place so it doesn't need to be re-opened.
	!
	!	02/01/89 - Frank Starman
	!		Moved subwindows into MAST file
	!		Remove SET file
	!
	!	05/31/89 - J. Shad Rydalch
	!		Create or change SET file compny name when
	!		report title is changed
	!
	!	10/29/92 - Kevin Handy
	!		Cleaned up com for CH_UTL_PROFILE.  Broke into
	!		CH_UTL_PROFILE and CH_UTL_SET.
	!		Modified to make sure SET open in READ/WRITE mode.
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
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD	UTL_PROFILE
	MAP (UTL_PROFILE_OLD)	UTL_PROFILE_CDD	UTL_PROFILE_OLD
	MAP (UTL_PROFILE_DEF)	UTL_PROFILE_CDD	UTL_PROFILE_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)		UTL_SET_CDD	UTL_SET

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_PROFILE) &
		UTL_PROFILE.CH%, &
		UTL_PROFILE.READONLY%

	COM (CH_UTL_SET) &
		UTL_SET.CH%, &
		UTL_SET.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Company Profile "
		SMG_WINDOW::NHELP = "UTL_MAIN_PROFILE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 128%

		SMG_WINDOW::NKEYS = 0%

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
		IF UTL_PROFILE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_PROFILE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_PROFILE = ERR
			CONTINUE 770
		END WHEN

		UTL_PROFILE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		USE
			UTL_MAIN_PROFILE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_PROFILE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_PROFILE.CH%
		GOSUB 28000

800		IF (UTL_SET.CH% = 0%) OR (UTL_SET.READONLY% <> 0%)
		THEN
			CLOSE #UTL_SET.CH% IF UTL_SET.CH%
			WHEN ERROR IN
				%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.CRE"
			USE
				UTL_MAIN_PROFILE = ERR
				CONTINUE 32767
			END WHEN

			UTL_SET.READONLY% = 0%
		END IF

	!
	! Set default option
	!
	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit"

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	6,  5, "(01) Company Name For Menu", &
			7,  5, "(02) Company Name For Report", &
			9,  5, "(03) Main Office Location #", &
			10,  5, "(04) Default Location #", &
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

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Company Name For Menu\*
	!	.p
	!	The ^*Company Name For Menu\* field
	!	contains the name of the business as it will appear on all menu
	!	screens.  This is sometimes referred to as a banner display or menu banner.
	!	.p
	!	The field will accommodate up to thirty (30) alphanumeric characters.
	!
	! Index:
	!	.x Banner>Display
	!	.x Menu>Banner
	!	.x Profile>Company Menu Name
	!	.x Company Profile>Menu Name
	!	.x Menu Name>Company>Profile
	!
	!--

			UTL_PROFILE::MENU_NAME = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;35", TEMP$, UTL_PROFILE::MENU_NAME, MFLAG, &
				"'E",MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Company Name For Reports\*
	!	.p
	!	The ^*Company Name For Reports\* field
	!	contains the name of the company
	!	as it will appear on all reports throughout the system.
	!	.p
	!	The field will accommodate up to thirty (30) alphanumeric characters.
	!
	! Index:
	!	.x Company Profile>Reports Name
	!	.x Profile>Company>Reports Name
	!	.x Reports Name>Company Profile
	!
	!--

			UTL_PROFILE::REP_NAME = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;35", TEMP$, UTL_PROFILE::REP_NAME, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Main Office Location\*
	!	.p
	!	The ^*Main Office Location\* field
	!	contains the code which identifies the main office location or
	!	headquarters office of the company.
	!	.p
	!	The field will accommodate four (4) alphanumeric characters.
	!
	! Index:
	!	.x Location>Main Office
	!	.x Main Office>Location
	!	.x Company Profile>Main Office Location
	!
	!--

			UTL_PROFILE::MAINLOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;35", TEMP$, &
				UTL_PROFILE::MAINLOCATION, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Default Location\*
	!	.p
	!	The ^*Default Location\* field
	!	contains the company location designated as the default. For example,
	!	if the price of an inventory item could not be found in a designated location,
	!	the system would search for the price in the default location.
	!	.p
	!	The field will accommodate thirty (30) alphanumeric characters.
	!
	! Index:
	!	.x Company Profile>Defaul Location
	!	.x Location>Default
	!	.x Default>Location>Company Profile
	!
	!--

			UTL_PROFILE::DEFLOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;35", TEMP$, &
				UTL_PROFILE::DEFLOCATION, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Need to remove under old key, and insert under
		! (possibly) new key
		!
		CASE "Change", "Blank"

			GOTO Exit_update_set_file IF (UTL_PROFILE_OLD::REP_NAME = &
				UTL_PROFILE::REP_NAME)

20430			WHEN ERROR IN
				GET #UTL_SET.CH%, &
					KEY #0% EQ "COMPNY" + SPACE$(33%) + "NAME"
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 20440 IF ERR = 155%
				EXIT HANDLER
			END WHEN

			UTL_SET::SDATA = UTL_PROFILE::REP_NAME

20435			UPDATE #UTL_SET.CH%
			GOTO Exit_update_set_file

20440			UTL_SET::PROGRAMNAME = "COMPNY"
			UTL_SET::ITEM    = "NAME"
			UTL_SET::SYSTEM  = ""
			UTL_SET::HARD    = ""
			UTL_SET::SDATA   = UTL_PROFILE::REP_NAME

			PUT #UTL_SET.CH%

 Exit_update_set_file:

		END SELECT


	!
	! Set UTL_PROFILE_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_PROFILE_OLD = UTL_PROFILE

	!
	! Restore UTL_PROFILE_OLD value
	!
	CASE OPT_RESETOLD
		UTL_PROFILE = UTL_PROFILE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_PROFILE_DEF = UTL_PROFILE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_PROFILE = UTL_PROFILE_DEF

	END SELECT

	EXIT FUNCTION

28000	!
	! Get record if it exists
	!
	WHEN ERROR IN
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	PUT #UTL_PROFILE.CH%, RECORD 1%

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More Menu option hidden in Mast.
	!++
	! Abstract:ERA
	!	^*Era\*
	!	.p
	!	The ^*Era\* option
	!	maintains the Era Description file.
	!
	! Index:
	!
	!--
