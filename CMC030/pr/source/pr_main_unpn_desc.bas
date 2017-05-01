1	%TITLE "PR Union/Pension Description Maintenance"
	%SBTTL "PR_MAIN_UNPN_DESC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_UNPN_DESC(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.p
	! Index:
	!	.x Union/Pension Table>Maintain
	!	.x Maintain>Union/Pension Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_UNPN_DESC/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_UNPN_DESC
	!	$ DELETE PR_MAIN_UNPN_DESC.OBJ;*
	!
	! Author:
	!
	!	12/11/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/23/02 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.HB"
	MAP (PR_UNPN_DESC)	PR_UNPN_DESC_CDD	PR_UNPN_DESC
	MAP (PR_UNPN_DESC2) PR_UNPN_DESC_CDD PR_UNPN_DESC_OLD, PR_UNPN_DESC2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the main program and
	! in PR_MAIN_UNPN_DESC.
	!
	!
	COM (CH_PR_UNPN_DESC) &
		PR_UNPN_DESC.CH%, &
		PR_UNPN_DESC.READONLY%
	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS
	DECLARE RFA TEMP_RFA

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Union/Pension Description Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_UNPN_DESC"
		SMG_WINDOW::CHAN  = PR_UNPN_DESC.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

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
		IF PR_UNPN_DESC.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_UNPN_DESC.READONLY%
			GOTO 790
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_UNPN_DESC = ERR
			CONTINUE 770
		END WHEN

		PR_UNPN_DESC.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_UNPN_DESC.OPN"
		USE
			PR_MAIN_UNPN_DESC = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_UNPN_DESC.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_UNPN_DESC.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_UNPN_DESC.CH%
		WHEN ERROR IN
			RESET #PR_UNPN_DESC.CH%
			GET #PR_UNPN_DESC.CH%, REGARDLESS
			UNLOCK	#PR_UNPN_DESC.CH%
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  5, "(01) Code", &
			2, 30, "(02) Description", &
			3,  5, "(03) Expense Acct", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Optional menu items
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " definiTion"

	!
	! Handle additional options
	!
20050	CASE OPT_MOREMENU

		!
		! Make sure there is a header
		!
		WHEN ERROR IN
			TEMP_RFA = GETRFA(PR_UNPN_DESC.CH%)
		USE
			CONTINUE 32767
		END WHEN

		SELECT EDIT$(MVALUE, -1%)

		!
		! Definitions
		!
		CASE "DEFINITION"
	!++
	! Abstract:DEFINITION
	!	^*Definition\*
	!	.p
	!	The ^*Definition\* option
	!	defines the specified Union/Pension code.
	!
	! Index:
	!	.x Definition
	!
	!--

			PR_MAIN_UNPN_DESC = &
				MAIN_JOURNAL(PR_MAIN_UNPN_DEF.ID, "")

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			PR_MAIN_UNPN_DESC = MAIN_JOURNAL(PR_MAIN_UNPN_DEF.ID, &
				"A")


		!
		! Change records
		!
		CASE "Change", "Blank", "Initialize"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_UNPN_DESC_OLD::CODE <> PR_UNPN_DESC::CODE
			THEN
				TEMP$ = PR_UNPN_DESC::CODE + ""
				PR_UNPN_DESC = PR_UNPN_DESC_OLD
				PR_MAIN_UNPN_DESC = &
					MAIN_JOURNAL(PR_MAIN_UNPN_DEF.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_UNPN_DESC = &
				MAIN_JOURNAL(PR_MAIN_UNPN_DEF.ID, "E")

		END SELECT


	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Code\*
	!	.p
	!	The ^*Code\* field enters a two (2) character
	!	alphanumeric code which will identify a specific union or pension.
	!	.p
	!	The field must contain a value. Duplicates are not allowed.
	!
	! Index:
	!	.x Union>Code
	!	.x Pension>Code
	!	.x Code>Union
	!	.x Code>Pension
	!
	!--

			PR_UNPN_DESC::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;17", TEMP$, &
				PR_UNPN_DESC::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a description of a specific union
	!	or pension.
	!	.p
	!	The field will accommodate up to thirty (30) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Description>Union
	!	.x Description>Pension
	!	.x Union>Description
	!	.x Pension>Description
	!
	!--

			PR_UNPN_DESC::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;47", TEMP$, &
				PR_UNPN_DESC::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Expense Account\*
	!	.p
	!	The ^*Expense Account\* field
	!	enters the General Ledger
	!	expense account which will be debited in the event that the employer
	!	would have a liability for contributions or assessments to a specific
	!	union or pension fund.
	!
	! Index:
	!	.x Account>Union
	!	.x Account>Pension
	!	.x Union>Account
	!	.x Pension>Account
	!
	!--

			PR_UNPN_DESC::EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;25", TEMP$, &
				PR_UNPN_DESC::EX_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_UNPN_DESC::EX_ACCT = GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_UNPN_DESC = 0%

		SELECT MLOOP

		CASE 3%
			TEMP$ = PR_UNPN_DESC::EX_ACCT

			IF EDIT$(TEMP$, -1%) <> "" AND &
				INSTR(1%, TEMP$, "?") = 0%
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_UNPN_DESC = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					TEMP$, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		END SELECT

	!
	! Set PR_UNPN_DESC_OLD value
	!
20500	CASE OPT_SETOLD
		PR_UNPN_DESC_OLD = PR_UNPN_DESC

	!
	! Restore PR_UNPN_DESC_OLD value
	!
	CASE OPT_RESETOLD
		PR_UNPN_DESC = PR_UNPN_DESC_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_UNPN_DESC2 = PR_UNPN_DESC

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_UNPN_DESC = PR_UNPN_DESC2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code        Description             " + &
				"              Expense Account    "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "010,049"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = " " + PR_UNPN_DESC::CODE + "         " + &
				PR_UNPN_DESC::DESCR + "        " + &
				PR_UNPN_DESC::EX_ACCT

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_UNPN_DESC.CH%, &
				KEY #0% GE PR_UNPN_DESC::CODE + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorry, but there is no current record", 0%)

32767	END FUNCTION
