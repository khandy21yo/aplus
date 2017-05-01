1	%TITLE "Manufacturing Order Make Master Model Attachments"
	%SBTTL "MO_MAIN_MAKE_LINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_MAKE_LINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	This program maintains Model(s) attached to the Make Master.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_MAKE_LINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_MAKE_LINE
	!	$ DELETE MO_MAIN_MAKE_LINE.OBJ;*
	!
	! Author:
	!
	!	03/01/91 - Val James Allen
	!
	! Modification history:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/20/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/21/92 - Dan Perkins
	!		Display model code description in OPT_VIEW.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.HB"
	MAP (MO_MAKELINE)	MO_MAKELINE_CDD		MO_MAKELINE
	MAP (MO_MAKELINE_OLD)	MO_MAKELINE_CDD		MO_MAKELINE_OLD, MO_MAKELINE_DEF

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	!
	! Common Statements
	!
	COM (CH_MO_MAKE) &
		MO_MAKE.CH%, &
		MO_MAKE.READONLY%

	COM (CH_MO_MAKELINE) &
		MO_MAKELINE.CH%, &
		MO_MAKELINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Models Attached to Make"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "MO_MAIN_MAKE_LINE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 6%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 1%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 10%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Model"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF MO_MAKELINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_MAKELINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_MAKE_LINE = ERR
			CONTINUE 770
		END WHEN

		MO_MAKELINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.OPN"
		USE
			MO_MAIN_MAKE_LINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_MAKELINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_MAKELINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_MAKELINE.CH%
		WHEN ERROR IN
			RESET #MO_MAKELINE.CH%
			GET #MO_MAKELINE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,05, "(01) Model Code", &
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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Model Code\*
	!	.b
	!	.lm +5
	!	The ^*Model Code\* field enters the model code
	!	you wish to attach to the Make.  The field will accommodate four (4)
	!	alphanumeric characters.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field displays a list of Model codes.
	!	.lm -5
	!
	! Index:
	!	.x Model Code
	!
	!--
			MO_MAKELINE::MODELCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;21", TEMP$, &
				MO_MAKELINE::MODELCODE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "V0") = 1%
				THEN
					MO_MAKELINE::MODELCODE = MO_MODELCODE::MODELCODE
				END IF
				GOTO ReEntry
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY

		MO_MAIN_MAKE_LINE = 0%

		SELECT MLOOP

		CASE 1%
			MO_MAIN_MAKE_LINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_MAKELINE::MODELCODE, &
				MO_MODELCODE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Model Code", MO_MAIN_MODELCODE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MODELCODE::DESCR, 2%, 35%,, &
				SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY
		IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "Q0" + MO_MAKELINE::MODELCODE) <> 1%
		THEN
			MO_MODELCODE::DESCR = STRING$(40%, A"?"B)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			MO_MODELCODE::DESCR, 2%, 35%,, SMG$M_BOLD)

	!
	! Set MO_MAKELINE_OLD value
	!
20500	CASE OPT_SETOLD
		MO_MAKELINE_OLD = MO_MAKELINE

	!
	! Restore MO_MAKELINE_OLD value
	!
	CASE OPT_RESETOLD
		MO_MAKELINE = MO_MAKELINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_MAKELINE_DEF = MO_MAKELINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_MAKELINE = MO_MAKELINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_MAKELINE::MAKE  = LEFT(MVALUE, 10%)
		MO_MAKELINE::YEAR  = MID(MVALUE, 11%, 4%)
		MO_MAKELINE::MTYPE = MID(MVALUE, 15%, 2%)
		MO_MAKELINE::MSIZE = RIGHT(MVALUE, 17%)

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE MO_MAKELINE::MAKE + &
				MO_MAKELINE::YEAR + &
				MO_MAKELINE::MTYPE + &
				MO_MAKELINE::MSIZE + &
				MO_MAKELINE::MODELCODE, REGARDLESS
		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  ModelCode Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012"

		!
		! Convert current record into text
		!
		CASE 3%
			IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "Q0" + &
				MO_MAKELINE::MODELCODE) <> 1%
			THEN
				MO_MODELCODE::DESCR = STRING$(40%, A"?"B)
			END IF

			MVALUE = MO_MAKELINE::MODELCODE + "      " + &
				MO_MODELCODE::DESCR

		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF MO_MAKELINE::MAKE + MO_MAKELINE::YEAR + &
				MO_MAKELINE::MTYPE + MO_MAKELINE::MSIZE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_MAKELINE::MAKE  = LEFT(MVALUE, 10%)
			MO_MAKELINE::YEAR  = MID(MVALUE, 11%, 4%)
			MO_MAKELINE::MTYPE = MID(MVALUE, 15%, 2%)
			MO_MAKELINE::MSIZE = RIGHT(MVALUE, 17%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
