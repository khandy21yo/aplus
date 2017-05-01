1	%TITLE "Inventory Issue Journal"
	%SBTTL "WP_MAIN_ISSJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_ISSJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	.b
	!	.lm +5
	!	The Inventory Issue Journal is maintained through the ^*WIP Order
	!	Issue Journal\* option.  Indicating the lines which have been completed or
	!	cancelled for the Order Register Master File is contained in this issue process.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Inventory Issue Journal
	!	.x Batch Number>User
	!	.x User>Batch Number
	!	.x Journal>Issue Entry Maintain
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_ISSJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_ISSJOUR
	!	$ DELETE WP_MAIN_ISSJOUR.OBJ;*
	!
	! Author:
	!
	!	07/18/91 - Craig Tanner
	!
	! Modification history:
	!
	!	08/02/91 - Craig Tanner
	!		Added the autoload feature for entering lines.
	!
	!	02/27/92 - Kevin Handy
	!		Added an "END SELECT" to the error trapping
	!		to remove syntax errors.
	!
	!	05/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/20/92 - Dan Perkins
	!		RSET requisition number.  Cleaned program code.
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/26/95 - Kevin Handy
	!		Lose common area LOCATION, which was never used.
	!		Reformat source closer to 80 columns.
	!
	!	07/16/96 - Kevin Handy
	!		Reformat source code.
	!		Lose lots of commented out code.
	!
	!	05/19/98 - Kevin Handy
	!		Increase batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)	WP_ISSJOUR_CDD	WP_ISSJOUR
	MAP (WP_ISSJOUR2)	WP_ISSJOUR_CDD	WP_ISSJOUR_OLD, WP_ISSJOUR2

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_ISSLINE.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_ISSJOUR) &
		WP_ISSJOUR.CH%

	COM (CH_WP_REQREGISTER_READ) &
		WP_REQREGISTER.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

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
		SMG_WINDOW::DESCR  = "Inventory Issue Journal " + BATCH_NO$
		SMG_WINDOW::NHELP  = "WP_MAIN_ISSJOUR"
		SMG_WINDOW::CHAN   = WP_ISSJOUR.CH%
		SMG_WINDOW::HSIZE  = 77%
		SMG_WINDOW::VSIZE  = 18%
		SMG_WINDOW::HVIEW  = 78%
		SMG_WINDOW::VVIEW  = 18%
		SMG_WINDOW::HPOS   = 2%
		SMG_WINDOW::VPOS   = 2%
		SMG_WINDOW::NITEMS = 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Req_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

690		IF WP_REQREGISTER.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
			USE
				CONTINUE 700 IF ERR = 5%
				EXIT HANDLER
			END WHEN
		END IF

700		!
		! Declare channels
		!
		IF WP_ISSJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_ISSJOUR.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_ORDERJOUR = ERR
			CONTINUE 770
		END WHEN

		WP_ISSJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.OPN"
		USE
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_ISSJOUR.READONLY% = -1%
		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_ISSJOUR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = WP_ISSJOUR.CH%
		WHEN ERROR IN
			RESET #WP_ISSJOUR.CH%
			GET #WP_ISSJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

		%PAGE

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SELECT MLOOP
		!
		! Main screen
		!
		CASE 0%

			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE( &
				SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	2,  5, "(01) Req #", &
				3,  5, "(02) Job #", &
				8,  5, "(03) Job Line", &
				9,  5, "(04) Operator", &
				4,  5, "     Status", &
				5,  5, "     Description", &
				6,  5, "     Type", &
				7,  5, "     Class", &
				0,  0, ""

			RESTORE

			READ XPOS, YPOS, XSTR$

			I% = 0%

			WHILE (XPOS <> 0%)
				I% = I% + 1%

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE( &
				SMG_WINDOW::WNUMBER)


		END SELECT

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Requisition Number\*
	!	.lm +5
	!	.b
	!	The ^*Requisition Number\* field
	!	selects a requisition number from the material requisition register.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will cause
	!	a list of valid material requisition numbers to be displayed.
	!	.b
	!	In the event that materials need to be issued for which there was no
	!	requisition processed, the issue of those materials could be accomplished by
	!	leaving this field blank.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Requisition Number
	!	.x Requisition Number>Issue Journal
	!
	!--
			WP_ISSJOUR::REQNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;23", TEMP$, WP_ISSJOUR::REQNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(WP_MAIN_REQREGISTER.ID, "VX") = 1%
				THEN
					WP_ISSJOUR::REQNUM = &
						WP_REQREGISTER::REQNUM
					WP_ISSJOUR::JOB = &
						WP_REQREGISTER::JOB
					WP_ISSJOUR::LLINE = &
						WP_REQREGISTER::LLINE
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Job Number\*
	!	.lm +5
	!	.b
	!	The ^*Job Number\* field enters the
	!	Job number for an issue record.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid Job Numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Job Number>Issue Journal
	!	.x Issue Journal>Job Number
	!
	!--
			MFLAG = MFLAG OR 1% IF WP_ISSJOUR::REQNUM <> ""

			WP_ISSJOUR::JOB  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;23", TEMP$, WP_ISSJOUR::JOB, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX" + &
					WP_ISSJOUR::REQNUM) = 1%
				THEN
					WP_ISSJOUR::JOB = JC_JOB::JOB
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Job Line\*
	!	.lm +5
	!	.b
	!	The ^*Job Line\* field specifies the
	!	job line for which material is being issued.
	!	.b
	!	In order to issue materials against a previously established materials
	!	requisition, this field must be valid in relation to a Job Line
	!	established for a materials requisition on a subject job order.  However,
	!	materials may be issued for which there has been no requisition established.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Job Line
	!	.x Job Line>Issue Journal
	!
	!--
			MFLAG = MFLAG OR 1% &
				IF WP_ISSJOUR::REQNUM <> ""

			WP_ISSJOUR::LLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;23", TEMP$, WP_ISSJOUR::LLINE, &
				MFLAG, "~L0'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Operator\*
	!	.lm +5
	!	.b
	!	The ^*Operator\* field enters an
	!	identification (name or initials) of the person who is entering the record
	!	for the materials issue.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Operator
	!	.x Operator>Issue Journal
	!
	!--
			WP_ISSJOUR::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;23", TEMP$, WP_ISSJOUR::OPERATOR, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_ISSJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Check if already in journal
			!
			IF WP_ISSJOUR::REQNUM <> ""
			THEN
				WHEN ERROR IN
					GET #WP_ISSJOUR.CH%, &
						KEY #0% EQ WP_ISSJOUR::REQNUM + "", &
						REGARDLESS
				USE
					CONTINUE 20310 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_ISSJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists in Journal", 1%)
			END IF
			GOTO ExitFunction

20310			!
			! Check if in register
			!
			WHEN ERROR IN
				GET #WP_REQREGISTER.CH%, &
					KEY #1% EQ WP_ISSJOUR::REQNUM + "", &
					REGARDLESS
			USE
				CONTINUE 20320 IF ERR = 9% OR ERR = 155%
				EXIT HANDLER
			END WHEN

			WP_ISSJOUR::JOB = WP_REQREGISTER::JOB
			WP_ISSJOUR::LLINE = WP_REQREGISTER::LLINE
			GOTO ExitFunction

20320			WP_MAIN_ISSJOUR = 1%
			CALL ENTR_3MESSAGE(SCOPE, &
				"Register Record Not Found", 1%)

		CASE 2%
			WP_MAIN_ISSJOUR = FUNC_TESTENTRY( SMG_WINDOW, &
				"J" + WP_ISSJOUR::JOB, &
				JC_JOB::SSTATUS, &
				"WP", MLOOP, "PRG", &
				"Job Number", JC_MAIN_JOB.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, &
				4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, &
				5%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, &
				6%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, &
				7%, 23%, , SMG$M_BOLD)

		CASE 4%
			!
			! Entry required in this field
			!
			IF WP_ISSJOUR::OPERATOR = ""
			THEN
				WP_MAIN_ISSJOUR = 1%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(JC_MAIN_JOB.ID, &
				"Q0J" + WP_ISSJOUR::JOB) <> 1%
			THEN
				JC_JOB::SSTATUS = ""
				JC_JOB::TTYPE = ""
				JC_JOB::CLASS = ""
				JC_JOB::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, 4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, 5%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, 6%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, 7%, 23%, , SMG$M_BOLD)
		END IF

	!
	! Set WP_ISSJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		WP_ISSJOUR_OLD = WP_ISSJOUR

	!
	! Restore WP_ISSJOUR_OLD value
	!
	CASE OPT_RESETOLD
		WP_ISSJOUR = WP_ISSJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_ISSJOUR2 = WP_ISSJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_ISSJOUR = WP_ISSJOUR2

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  ReqNumber  JobNumber  Line"

		CASE 2%
			MVALUE = "013, 024"

		CASE 3%
			MVALUE = CONV_STRING( &
				WP_ISSJOUR::REQNUM, CMC$_LEFT) + " " + &
				WP_ISSJOUR::JOB + " " + &
				WP_ISSJOUR::LLINE

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #WP_ISSJOUR.CH%, &
				KEY #0% GE WP_ISSJOUR::REQNUM + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	!	Help Errors
	!*******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:LINE
	!	^*Line\*
	!	.lm +5
	!	.b
	!	The ^*Line\* function
	!	accesses the line screen in order to record the issue of materials for
	!	a job or work order.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Line
	!	.x Line>Issue Journal
	!
	!--
