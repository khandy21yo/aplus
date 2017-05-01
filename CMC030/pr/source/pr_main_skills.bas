1	%TITLE "Maintain Skills Table"
	%SBTTL "PR_MAIN_SKILLS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_SKILLS(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1996 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Maintain Skills Table\*
	!	accesses the file where valid skills are
	!	established, including related effective dates and industrial
	!	standards for both piece and hourly rates.
	!	.lm -5
	!
	! Index:
	!	.x Skills>Table
	!	.x Tables>Skills
	!
	! Option:
	!
	! Author:
	!
	!	09/09/96 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_SKILLS/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_SKILLS
	!	$ DELETE PR_MAIN_SKILLS.OBJ;*
	!
	! Modification history:
	!
	!	09/23/97 - Kevin Handy
	!		Fixed titles and descriptions to read 'skill'
	!		instead of 'operation'.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Map Statements and CDD inclusions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.HB"
	MAP	(PR_SKILLS)	PR_SKILLS_CDD	PR_SKILLS
	MAP	(PR_SKILLS2)	PR_SKILLS_CDD	PR_SKILLS_OLD, PR_SKILLS2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	!
	COM (CH_PR_SKILLS) &
		PR_SKILLS.CH%, &
		PR_SKILLS.READONLY%

	%PAGE


	!
	! Set up error handling
	!
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

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Skill Table Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_SKILLS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Skill"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Eeosort"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

20100		!
		! Declare channels
		!
		IF PR_SKILLS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_SKILLS.READONLY%
			GOTO 20190
		END IF


		!
		! Open main file (existing) for modification
		!
20150		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.CRE"
		USE
			CONTINUE 20160 IF ERR = 10%
			PR_MAIN_SKILLS = ERR
			CONTINUE 20170
		END WHEN

		PR_SKILLS.READONLY% = 0%
		GOTO 20190

20160		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.OPN"
		USE
			PR_MAIN_SKILLS = ERR
			CONTINUE 20170
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_SKILLS.READONLY% = -1%

		GOTO 20190

20170		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_SKILLS.CH%)

		EXIT FUNCTION

20190		SMG_WINDOW::CHAN  = PR_SKILLS.CH%
		WHEN ERROR IN
			RESET #PR_SKILLS.CH%
			GET #PR_SKILLS.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 5, "(01) Skill", &
			6, 5, "(02) Description", &
			7, 5, "(03) EEO Sort", &
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
	! This option is used to enter the data from the user,
	! display data, set defaults, and return the data back
	! according to MFLAG.
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
	!	^*(01) Skill\*
	!	.b
	!	.lm +5
	!	The ^*Skill\* field
	!	enters a skill name or code up to eight (8) alphanumeric
	!	characters in length.  The field is a key.  Duplicates are allowed
	!	in order to have more than one record for the same skill, but with
	!	different effective dates and different industrial standards for
	!	pieces per hour and/or hourly rates.
	!	.lm -5
	!
	! Index:
	!	.x Skill
	!
	!--

			PR_SKILLS::SKILL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;26", TEMP$, &
				PR_SKILLS::SKILL, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	Provices a short description of the skill.
	!	.lm -5
	!
	! Index:
	!	.x Skill>Description
	!
	!--
			PR_SKILLS::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;26", TEMP$, &
				PR_SKILLS::DESCRIPTION, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) EEO Sort\*
	!	.b
	!	.lm +5
	!	Used to determine the order that the skill should appear
	!	on the EEO report
	!	.lm -5
	!
	! Index:
	!	.x Skill>EEO Sort
	!	.x EEO Sort>Skill
	!
	!--
			PR_SKILLS::EEOSORT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;26", TEMP$, &
				PR_SKILLS::EEOSORT, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

		!
		! Test values
		!
20300	CASE OPT_TESTENTRY
		PR_MAIN_SKILLS = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_SKILLS::SKILL = ""
			THEN
				PR_MAIN_SKILLS = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank Skill not allowed", 1%)
			END IF

		END SELECT

		!
		! Set PR_SKILLS_OLD value
		!
20500	CASE OPT_SETOLD
		PR_SKILLS_OLD = PR_SKILLS

		!
		! Restore PR_SKILLS_OLD value
		!
	CASE OPT_RESETOLD
		PR_SKILLS = PR_SKILLS_OLD

		!
		! Set default value
		!
	CASE OPT_SETDEFAULT
		PR_SKILLS2 = PR_SKILLS

		!
		! Restore default value
		!
	CASE OPT_RESETDEFAULT
		PR_SKILLS = PR_SKILLS2

		!
		! View header
		!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Skill  Description                    " + &
				"          EEO Sort"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,050"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				PR_SKILLS::SKILL + " " + &
				PR_SKILLS::DESCRIPTION + " " + &
				PR_SKILLS::EEOSORT
		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #PR_SKILLS.CH%, &
				KEY #0% GE PR_SKILLS::SKILL + "", &
				REGARDLESS
		CASE 1%
			FIND #PR_SKILLS.CH%, &
				KEY #1% GE PR_SKILLS::EEOSORT + &
				PR_SKILLS::SKILL, &
				REGARDLESS
		END SELECT

	END SELECT

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

 ExitFunction:
32767	END FUNCTION
