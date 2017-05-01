1	%TITLE "CHART OF ACCOUNTS MAINTENANCE"
	%SBTTL "GL_MAIN_TRICONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_TRICONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1995 BY
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	! ID:1002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Chart of Accounts Maintenance\* routine is used to
	!	enter and maintain the Chart of Accounts.
	!	.b
	!	In addition to account number and account description
	!	information, codes are entered which will accommodate the
	!	desired presentation in the financial statements.
	!	.lm -5
	!
	! Index:
	!	.x Control>Tri Spur
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_TRICONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_TRICONTROL
	!	$ DELETE GL_MAIN_TRICONTROL.OBJ;*
	!
	! Author:
	!
	!	06/06/95 - Kevin Handy
	!
	! Modification history:
	!
	!	06/15/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/25/95 - Kevin Handy
	!		Modified to select correct array elements.
	!
	!	02/26/96 - Kevin Handy
	!		Modifications to the way titles work
	!
	!	06/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.HB"
	MAP	(GL_TRICONTROL)	GL_TRICONTROL_CDD	GL_TRICONTROL
	MAP	(GL_TRICONTROL_OLD) GL_TRICONTROL_CDD	GL_TRICONTROL_OLD, GL_TRICONTROL2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM)	UTL_SYSTEM_CDD	UTL_SYSTEM

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_TRICONTROL) &
		GL_TRICONTROL.CH%, &
		GL_TRICONTROL.READONLY%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!***********************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!***********************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Tri-Spur Control File"
		SMG_WINDOW::NHELP = "GL_MAIN_TRICONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 52%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF GL_TRICONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_TRICONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_TRICONTROL = ERR
			CONTINUE 770
		END WHEN

		GL_TRICONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.OPN"
		USE
			GL_MAIN_TRICONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_TRICONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_TRICONTROL.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_TRICONTROL.CH%
		WHEN ERROR IN
			RESET #GL_TRICONTROL.CH%
			GET #GL_TRICONTROL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1, 1, "(01) Location", &
			1, 40, "Account", &
			1, 65, "Flag", &
			2, 1, "(02) Fld 3", &
			3, 1, "(05) Fld 4", &
			4, 1, "(08) Fld 5", &
			5, 1, "(11) <Unused>", &
			6, 1, "(14) Fld 6", &
			7, 1, "(17) Fld 9", &
			8, 1, "(20) Fld 12", &
			9, 1, "(23) Fld 15", &
			10, 1, "(26) Fld 18", &
			11, 1, "(29) Fld 21", &
			12, 1, "(32) Fld 24", &
			13, 1, "(35) Fld 27", &
			14, 1, "(38) Fld 30", &
			15, 1, "(41) Cash/Check", &
			16, 1, "(44) Credit Card", &
			17, 1, "(47) Over/Short", &
			18, 1, "(50) Labor", &
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

		FOR I% = 0% TO 16%
			XSTR$ = FORMAT$(I% * 3% + 3%, "(<0>#) ")
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, I% + 2%, 40%) &
				IF (SMG_WINDOW::HFLAG(I% * 2% + 2%) AND 2%) = 0%

			XSTR$ = FORMAT$(I% * 3% + 4%, "(<0>#) ")
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, I% + 2%, 65%) &
				IF (SMG_WINDOW::HFLAG(I% * 2% + 2%) AND 2%) = 0%

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!
	! Enter/Display/Default
	!
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Location\*
	!	.b
	!	.lm +5
	!	Specifies which location is being defined.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_TRICONTROL::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;18", TEMP$, &
				GL_TRICONTROL::LOCATION, MFLAG, "'E", MVALUE)


		CASE 2%, 5%, 8%, 11%, 14%, 17%, 20%, 23%, 26%, 29%, &
			32%, 35%, 38%, 41%, 44%, 47%, 50%

	!++
	! Abstract:FLD002
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD02"
			ITEM% = (MLOOP - 2%) / 3%

			GL_TRICONTROL::DESCRIPTION(ITEM%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";18", &
				TEMP$, &
				GL_TRICONTROL::DESCRIPTION(ITEM%), &
				MFLAG, "'LLLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 3%, 6%, 9%, 12%, 15%, 18%, 21%, 24%, 27%, 30%, &
			33%, 36%, 39%, 42%, 45%, 48%, 51%

	!++
	! Abstract:FLD003
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD03"
			ITEM% = (MLOOP - 3%) / 3%

			GL_TRICONTROL::ACCOUNT(ITEM%) = &
				ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";45", TEMP$, &
				GL_TRICONTROL::ACCOUNT(ITEM%), &
				MFLAG, "'LLLLLLLLLLLLLLLLL", MVALUE)

		CASE 4%, 7%, 10%, 13%, 16%, 19%, 22%, 25%, 28%, 31%, &
			34%, 37%, 40%, 43%, 46%, 49%, 52%

	!++
	! Abstract:FLD004
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD04"
			ITEM% = (MLOOP - 4%) / 3%

			GL_TRICONTROL::FLAG(ITEM%) = &
				ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";72", TEMP$, &
				GL_TRICONTROL::FLAG(ITEM%), &
				MFLAG, "'", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!***********************************************************
	! Test values
	!***********************************************************
	CASE OPT_TESTENTRY

		GL_MAIN_TRICONTROL = 0%

	CASE OPT_DISPLAY


	!***********************************************************
	! Test option
	!***********************************************************
	CASE OPT_TESTOPT

		GL_MAIN_TRICONTROL = 0%

20500	!***********************************************************
	! Set GL_TRICONTROL_OLD value
	!***********************************************************
	CASE OPT_SETOLD
		GL_TRICONTROL_OLD = GL_TRICONTROL

	!***********************************************************
	! Restore GL_TRICONTROL_OLD value
	!***********************************************************
	CASE OPT_RESETOLD
		GL_TRICONTROL = GL_TRICONTROL_OLD

	!***********************************************************
	! Set default value
	!***********************************************************
	CASE OPT_SETDEFAULT
		!
		! Load in defaults for chart
		!
		GL_TRICONTROL2 = GL_TRICONTROL

	!***********************************************************
	! Restore default value
	!***********************************************************
	CASE OPT_RESETDEFAULT
		GL_TRICONTROL = GL_TRICONTROL2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Location"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "014"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_TRICONTROL::LOCATION

		END SELECT

	!***********************************************************
	! Find
	!***********************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account number)
			!
			FIND #GL_TRICONTROL.CH%, &
				KEY #0% GE GL_TRICONTROL::LOCATION + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	!***********************************************************
	! End of GL_MAIN_TRICONTROL function
	!***********************************************************
	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD02
	!	^*Description\*
	!	.b
	!	.lm +5
	!	Description of the current element.
	!	If the description is blank, then the journal maintenance will
	!	ask for a description when this element is reached.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*Account\*
	!	.b
	!	.lm +5
	!	Defines the account number that will be used for this element.
	!	If the account number is left blank, then the journal maintenance
	!	will ask for an account number when this element is reached.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*Flag\*
	!	.b
	!	.lm +5
	!	Specifies a flag for this element.
	!	.b
	!	Valid flags include:
	!	.b
	!	.lm +5
	!	.list 0,"*"
	!	.le
	!	<blank> Posted as is to GL.
	!	.le
	!	*+ Posted as is to GL. (Same as blank)
	!	.le
	!	*- Negated value posted to GL.
	!	.le
	!	*L Labor amount (Posted in units field on first element)
	!	.els
	!	.lm -5
	!	.lm -5
	!
	! Index:
	!
	!--
