1	%TITLE "Reason Code Account"
	%SBTTL "OE_MAIN_REASONACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_REASONACCT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.b
	!	.lm +5
	!	The ^*Reason Code Account Table\* provides a list of
	!	General Ledger Accounts associated with reason codes.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_REASONACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_REASONACCT
	!	$ DELETE OE_MAIN_REASONACCT.OBJ;*
	!
	! Author:
	!
	!	04/15/92 - Dan Perkins
	!		Copied form OE_MAIN_COMMACCT.
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	MAP (OE_REASONACCT)	OE_REASONACCT_CDD	OE_REASONACCT
	MAP (OE_REASONACCT_OLD)	OE_REASONACCT_CDD	OE_REASONACCT_OLD, &
							OE_REASONACCT2

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_REASONACCT) &
		OE_REASONACCT.CH%, &
		OE_REASONACCT.READONLY%

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION &
					MAIN_WINDOW, &
					FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Reason Code Account"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_REASONACCT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS = 2%
		SMG_WINDOW::VPOS = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS = 3%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::VHPOS = 2%
		SMG_WINDOW::VVPOS = 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Reason"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_REASONACCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_REASONACCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_REASONACCT = ERR
			CONTINUE 770
		END WHEN

		OE_REASONACCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.OPN"
		USE
			OE_MAIN_REASONACCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_REASONACCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_REASONACCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_REASONACCT.CH%
		WHEN ERROR IN
			RESET #OE_REASONACCT.CH%
			GET #OE_REASONACCT.CH%, REGARDLESS
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

		DATA	02,03, "(01) Reason Code", &
			03,03, "(02) Location", &
			04,03, "(03) Account", &
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

	!*********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!*********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Reason Code\*
	!	.b
	!	.lm +5
	!	The ^*Reason Code\* field enters a
	!	code which will be associated to the GL account number.  The field will
	!	accommodate two characters.
	!	.b
	!	Valid Reason codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REASONACCT::CREASON = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;23", TEMP$, &
				OE_REASONACCT::CREASON, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CREASON.ID, "VX") = 1%
				THEN
					OE_REASONACCT::CREASON = &
						OE_CREASON::CREASON
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(OE_MAIN_CREASON.ID, "M")
				OE_REASONACCT::CREASON = OE_CREASON::CREASON
				GOTO ReEnter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a selected
	!	location number which has been established in the Utilities
	!	system.
	!	.b
	!	The field will accept up to four characters.
	!	.b
	!	Valid location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REASONACCT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;23", TEMP$, &
				OE_REASONACCT::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					OE_REASONACCT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) GL Account _#\*
	!	.b
	!	.lm +5
	!	The ^*GL Account _#\* field enters
	!	a specific General Ledger account number.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.
	!	.b
	!	Valid account numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REASONACCT::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				OE_REASONACCT::ACCOUNT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_REASONACCT::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the data
	!
20300	CASE OPT_TESTENTRY
		OE_MAIN_REASONACCT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test OE_REASONACCT
			!
			OE_MAIN_REASONACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REASONACCT::CREASON, &
				OE_CREASON::DESCR, &
				"OE", MLOOP, "PROG", &
				"Reason ", OE_MAIN_CREASON.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CREASON::DESCR, 2%, 32%, , SMG$M_BOLD)

		CASE 2%
			!
			! Test UTL_LOCATION
			!
			OE_MAIN_REASONACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REASONACCT::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location ", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 32%, , SMG$M_BOLD)

		CASE 3%
			!
			! Test GL ACCOUNT
			!
			OE_MAIN_REASONACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REASONACCT::ACCOUNT, &
				GL_CHART::DESCR, &
				"OE", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)

		END SELECT

	!
	! Display the data
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			OE_CREASON::DESCR = STRING$(LEN( &
				OE_CREASON::DESCR), A"?"B) &
				IF  MAIN_WINDOW(OE_MAIN_CREASON.ID, "Q0" + &
					OE_REASONACCT::CREASON) <> 1%

			IF INSTR(1%, OE_REASONACCT::CREASON, "?") > 0%
			THEN
				OE_CREASON::DESCR = "Reason Overlay Mask"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CREASON::DESCR, 2%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
				OE_REASONACCT::LOCATION) <> 1%

			IF INSTR(1%, OE_REASONACCT::LOCATION, "?") > 0%
			THEN
				UTL_LOCATION::LOCNAME = "Location Overlay Mask"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					OE_REASONACCT::ACCOUNT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_REASONACCT_OLD value
	!
20500	CASE OPT_SETOLD
		OE_REASONACCT_OLD = OE_REASONACCT

	!
	! Restore OE_REASONACCT_OLD value
	!
	CASE OPT_RESETOLD
		OE_REASONACCT = OE_REASONACCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_REASONACCT2 = OE_REASONACCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_REASONACCT = OE_REASONACCT2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Reason  Location  GL Account"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,019"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = OE_REASONACCT::CREASON + "      " + &
				OE_REASONACCT::LOCATION + "      " + &
				OE_REASONACCT::ACCOUNT

		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_REASONACCT::CREASON + &
					OE_REASONACCT::LOCATION, REGARDLESS

		END SELECT

	END SELECT

28000	EXIT FUNCTION


29000	!******************************************************************
	! Trap errors
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
