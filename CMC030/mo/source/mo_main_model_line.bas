1	%TITLE "Manufacturing Order Model Master Options Attachments"
	%SBTTL "MO_MAIN_MODEL_LINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_MODEL_LINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains Options attached to the Model Master.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_MODEL_LINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_MODEL_LINE
	!	$ DELETE MO_MAIN_MODEL_LINE.OBJ;*
	!
	! Author:
	!
	!	03/01/91 - Val James Allen
	!
	! Modification history:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/23/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/21/92 - Dan Perkins
	!		Added option group and option description to OPT_VIEW.
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
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

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE
	MAP (MO_MODELLINE_OLD)	MO_MODELLINE_CDD	MO_MODELLINE_OLD, &
							MO_MODELLINE_DEF

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP (MO_OPTGROUP)	MO_OPTGROUP_CDD		MO_OPTGROUP

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	!
	! Common Statements
	!
	COM (CH_MO_MODEL) &
		MO_MODEL.CH%, &
		MO_MODEL.READONLY%

	COM (CH_MO_MODELLINE) &
		MO_MODELLINE.CH%, &
		MO_MODELLINE.READONLY%

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
		SMG_WINDOW::DESCR = "Options Attached to Model"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "MO_MAIN_MODEL_LINE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 6%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 10%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(1%) = "Option"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF MO_MODELLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_MODELLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_MODEL_LINE = ERR
			CONTINUE 770
		END WHEN

		MO_MODELLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.OPN"
		USE
			MO_MAIN_MODEL_LINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_MODELLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_MODELLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_MODELLINE.CH%
		WHEN ERROR IN
			RESET #MO_MODELLINE.CH%
			GET #MO_MODELLINE.CH%, REGARDLESS
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


		DATA	02,05, "(01) Option Group", &
			03,05, "(02) Option Code", &
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Option Group\*
	!	.b
	!	.lm +5
	!	The ^*Option Group\* field enters the Option
	!	group to attach to the Model.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field displays a list of Option Groups.
	!	.lm -5
	!
	! Index:
	!	.x Option Group
	!
	!--

			MO_MODELLINE::OPTGROUP = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;25", TEMP$, &
				MO_MODELLINE::OPTGROUP, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, "VX") = 1%
				THEN
					MO_MODELLINE::OPTGROUP = MO_OPTGROUP::OPTGROUP
				END IF
				GOTO ReEntry
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Option Code\*
	!	.b
	!	.lm +5
	!	The ^*Option Code\* field enters the Option
	!	Code associated the Model.
	!	.b
	!	The ^*List Choices\* key displays a list of Option Codes attached to option
	!	groups.
	!	.lm -5
	!
	! Index:
	!	.x Option Code
	!
	!--

			MO_MODELLINE::OPTN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, &
				MO_MODELLINE::OPTN, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(MO_MAIN_OPTION.ID, &
					"V0" + MO_MODELLINE::OPTGROUP) = 1%
				THEN
					MO_MODELLINE::OPTN = MO_OPTION::OPTN
				END IF
				GOTO ReEntry
			END IF

		END SELECT

20300	CASE OPT_TESTENTRY

		MO_MAIN_MODEL_LINE = 0%

		SELECT MLOOP

		CASE 1%
			IF MO_MODELLINE::OPTGROUP <> ""
			THEN
				MO_MAIN_MODEL_LINE = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MODELLINE::OPTGROUP, &
					MO_OPTGROUP::DESCR, &
					"MO", MLOOP, "PROG", &
					"Option Group", MO_MAIN_OPTGROUP.ID)
			ELSE
				MO_OPTGROUP::DESCR = ""
 !					STRING$(LEN(MO_OPTGROUP::DESCR), A" "B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTGROUP::DESCR, 2%, 35%,, SMG$M_BOLD)

		CASE 2%
			MO_MAIN_MODEL_LINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_MODELLINE::OPTGROUP + MO_MODELLINE::OPTN, &
				MO_OPTION::DESCR, &
				"MO", MLOOP, "PROG", &
				"Option", MO_MAIN_OPTION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTION::DESCR, 3%, 35%,, SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, "Q0" + MO_MODELLINE::OPTGROUP) <> 1%
			THEN
				MO_OPTGROUP::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTGROUP::DESCR, 2%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_OPTION.ID, "Q0" + MO_MODELLINE::OPTGROUP + MO_MODELLINE::OPTN) <> 1%
			THEN
				MO_OPTION::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTION::DESCR, 3%, 35%,, SMG$M_BOLD)
		END IF

	!
	! Set MO_MODELLINE_OLD value
	!
20500	CASE OPT_SETOLD
		MO_MODELLINE_OLD = MO_MODELLINE

	!
	! Restore MO_MODELLINE_OLD value
	!
	CASE OPT_RESETOLD
		MO_MODELLINE = MO_MODELLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_MODELLINE_DEF = MO_MODELLINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_MODELLINE = MO_MODELLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_MODELLINE::MODELCODE = LEFT(MVALUE, LEN(MO_MODELLINE::MODELCODE))
		MO_MODELLINE::MSIZE = MID(MVALUE, LEN(MO_MODELLINE::MODELCODE) +1%, LEN(MO_MODELLINE::MSIZE))
		MO_MODELLINE::CLASS = MID(MVALUE, LEN(MO_MODELLINE::MODELCODE) + LEN(MO_MODELLINE::MSIZE) + 1%, LEN(MO_MODELLINE::CLASS))

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE MO_MODELLINE::MODELCODE + &
				MO_MODELLINE::MSIZE + &
				MO_MODELLINE::CLASS + &
				MO_MODELLINE::OPTGROUP + &
				MO_MODELLINE::OPTN, REGARDLESS
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
			MVALUE = "  OptGroup Description                   " + &
				" OptCode Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,042,050"

		!
		! Convert current record into text
		!
		CASE 3%
			IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, "Q0" + MO_MODELLINE::OPTGROUP) <> 1%
			THEN
				MO_OPTGROUP::DESCR = STRING$(40%, A"?"B)
			END IF

			IF MAIN_WINDOW(MO_MAIN_OPTION.ID, "Q0" + MO_MODELLINE::OPTGROUP + MO_MODELLINE::OPTN) <> 1%
			THEN
				MO_OPTION::DESCR = STRING$(40%, A"?"B)
			END IF

			MVALUE = MO_MODELLINE::OPTGROUP       + "       " + &
				LEFT(MO_OPTGROUP::DESCR, 30%) + " "       + &
				MO_MODELLINE::OPTN            + "    "    + &
				LEFT(MO_OPTION::DESCR, 30%)

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

			IF LEFT(MO_MODELLINE::MODELCODE + MO_MODELLINE::MSIZE + &
				MO_MODELLINE::CLASS + MO_MODELLINE::OPTGROUP, &
				LEN(MVALUE)) = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_MODELLINE::MODELCODE = LEFT(MVALUE, LEN(MO_MODELLINE::MODELCODE))
			MO_MODELLINE::MSIZE     = MID(MVALUE, LEN(MO_MODELLINE::MODELCODE) +1%, LEN(MO_MODELLINE::MSIZE))
			MO_MODELLINE::CLASS     = MID(MVALUE, LEN(MO_MODELLINE::MODELCODE) + LEN(MO_MODELLINE::MSIZE) + 1%, LEN(MO_MODELLINE::CLASS))

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
