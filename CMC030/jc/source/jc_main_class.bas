1	%TITLE "Job Class Maintenance"
	%SBTTL "JC_MAIN_CLASS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG JC_MAIN_CLASS(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	!	The ^*Job Class Maintenance\* program maintains the job class file.
	!	.lm -5
	!
	! Index:
	!	.x Sales Anlysis<Class<Maintain
	!	.x Maintain<Sales Anlysis<Class
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_MAIN_CLASS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN JC_MAIN_CLASS
	!	$ DELETE JC_MAIN_CLASS.OBJ;*
	!
	! Author:
	!
	!	07/02/90 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS
	MAP (JC_CLASS_OLD)	JC_CLASS_CDD		JC_CLASS_OLD, JC_CLASS2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_JC_CLASS) &
		JC_CLASS.CH%, &
		JC_CLASS.READONLY%

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

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Job Class Maintenance"
		SMG_WINDOW::NHELP = "JC_MAIN_CLASS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Class"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF JC_CLASS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF JC_CLASS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			JC_MAIN_CLASS = ERR
			CONTINUE 770
		END WHEN

		JC_CLASS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
		USE
			JC_MAIN_CLASS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		JC_CLASS.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(JC_CLASS.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = JC_CLASS.CH%
		WHEN ERROR IN
			RESET #JC_CLASS.CH%
			GET #JC_CLASS.CH%, REGARDLESS
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


		DATA	05,05, "(01) Job Class", &
			07,05, "(02) Description", &
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
	!	^*(01) Job Class\*
	!	.b
	!	.lm +5
	!	The ^*Job Class\* field enters a
	!	user defined code of up to four (4) alphanumeric characters in length.
	!	.lm -5
	!
	! Index:
	!	.x Job Class>Code
	!	.x Code>Job Class
	!
	!--

			JC_CLASS::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;32",TEMP$, JC_CLASS::CLASS, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a
	!	description for the Job Class entered in field (01).
	!	.b
	!	The field will accommodate up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Job Class
	!	.x Job Class>Description
	!
	!--

			JC_CLASS::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;32",	TEMP$, JC_CLASS::DESCR, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		JC_MAIN_CLASS = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ JC_CLASS::CLASS + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				JC_MAIN_CLASS = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

	!
	! Set JC_CLASS_OLD value
	!
20500	CASE OPT_SETOLD
		JC_CLASS_OLD = JC_CLASS

	!
	! Restore JC_CLASS_OLD value
	!
	CASE OPT_RESETOLD
		JC_CLASS = JC_CLASS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		JC_CLASS2 = JC_CLASS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		JC_CLASS = JC_CLASS2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Class   Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = JC_CLASS::CLASS + "    " + &
				JC_CLASS::DESCR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE JC_CLASS::CLASS + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
