1	%TITLE "WIP Order Requisition Journal"
	%SBTTL "WP_MAIN_REQJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_REQJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*WIP Requisition Journal\* option
	!	maintains the WIP Requistion Journal.
	!	Requisition is the process for indicating the job or
	!	job lines that have been requested for issuing.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>WIP Requistion Journal
	!	.x Batch Number>User
	!	.x User>Batch Number
	!	.x Journal>Requistion
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_REQJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_REQJOUR
	!	$ DELETE WP_MAIN_REQJOUR.OBJ;*
	!
	! Author:
	!
	!	07/17/91 - Jeff Beard
	!
	! Modification history:
	!
	!	05/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	07/16/92 - Dan Perkins
	!		Removed OPT_AFTEROPT.  Cleaned program code.
	!
	!	09/25/92 - Dan Perkins
	!		Require entry in OPERATOR field.
	!
	!	10/09/92 - Dan Perkins
	!		Display Line Description and default the first
	!		note line to this description if there is one.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	05/19/98 - Kevin Handy
	!		Increate batch number from 3 to 8 characters.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Lose GetRemain (Dead Code)
	!
	!	12/01/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR
	MAP (WP_REQJOUR_OLD)	WP_REQJOUR_CDD		WP_REQJOUR_OLD
	MAP (WP_REQJOUR2)	WP_REQJOUR_CDD		WP_REQJOUR2

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	!
	! This common area must be mapped in the main program.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_REQJOUR) &
		WP_REQJOUR.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION JC_MAIN_JOB
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
		SMG_WINDOW::DESCR = "Material Requisition Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "WP_MAIN_REQJOUR"
		SMG_WINDOW::CHAN  = WP_REQJOUR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Job Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF WP_REQJOUR.CH% > 0%

		!
		! Open WP_REQJOUR
		!
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.CRE"

20040		SMG_WINDOW::CHAN  = WP_REQJOUR.CH%
		WHEN ERROR IN
			RESET	#WP_REQJOUR.CH%
			GET	#WP_REQJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

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

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	1,  5, "(01) Job #", &
				2,  5, "     Status", &
				3,  5, "     Description", &
				4,  5, "     Type", &
				5,  5, "     Class", &
				6,  5, "(02) Line", &
				7,  5, "(03) Req Date", &
				8,  5, "(04) Operator", &
				9,  5, "(05) Notes", &
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

		END SELECT

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Job Number\*
	!	.b
	!	.lm +5
	!	The ^*Job Number\* field enters
	!	a Job number for which materials are to be requisitioned.
	!	.b
	!	An entry is required in this field.  The field will accommodate up to ten (10)
	!	alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid job numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Order Number>Materials Requisition Journal
	!	.x Job Number>Materials Requisition Journal
	!	.x Materials Requisition Journal>Job Number
	!	.x Requisition Journal>Job Number
	!
	!--
			WP_REQJOUR::JOB  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;23", TEMP$, &
				WP_REQJOUR::JOB, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX") = 1%
				THEN
					WP_REQJOUR::JOB = &
						JC_JOB::JOB
				END IF
				GOTO Reenter

			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field references the various
	!	line items created when a job was entered in the Job Journal.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of the line numbers for a referenced job to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Requisition>Line
	!	.x Line>Requisition
	!
	!--
			WP_REQJOUR::LLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;23", TEMP$, &
				WP_REQJOUR::LLINE, MFLAG, "~L0'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(WP_MAIN_REGLINE.ID, "V0" + &
					WP_REQJOUR::JOB) = 1%
				THEN
					WP_REQJOUR::LLINE = &
						WP_REGLINE::LLINE
				END IF
				GOTO Reenter

			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(02) Requisition Date\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Date\*
	!	enters the date on which the requisition is to be made.
	!	.lm -5
	!
	! Index:
	!	.x Requisition>Date
	!	.x Date>Materials Requisition
	!	.x Materials Requisition>Date
	!
	!--
			WP_REQJOUR::REQDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;23", TEMP$, &
				WP_REQJOUR::REQDATE, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field
	!	enters an identification (name or initials) for the person entering a
	!	materials requisition.
	!	.lm -5
	!
	! Index:
	!	.x Requisition>Operator
	!	.x Operator>Requisition
	!	.x Materials Requisition>Operator
	!
	!--
			WP_REQJOUR::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;23", TEMP$, &
				WP_REQJOUR::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field
	!	enters, in free format, special information pertaining to a materials
	!	requisition.
	!	.b
	!	There are two (2) line of notes.  Each line will accommodate up to forty (40)
	!	characters.
	!	.b
	!	The use of this field is optional.
	!	.lm -5
	!
	! Index:
	!	.x Requisition>Notes
	!	.x Notes>Requisition
	!	.x Materials Requisition>Notes
	!
	!--
 FirstNote:
			WP_REQJOUR::NOTES(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;23", TEMP$, &
				WP_REQJOUR::NOTES(0%), MFLAG, "'E", MVALUE)

			WP_REQJOUR::NOTES(1%) = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;23", TEMP$, &
				WP_REQJOUR::NOTES(1%), MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO FirstNote

			END SELECT

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_REQJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Check for this order number in register file
			! MUST BE THERE
			!
			WP_MAIN_REQJOUR = FUNC_TESTENTRY( SMG_WINDOW, &
				"J" + WP_REQJOUR::JOB, &
				JC_JOB::SSTATUS, &
				"WP", MLOOP, "PRG", &
				"Job Number", JC_MAIN_JOB.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, 2%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, 3%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, 4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, 5%, 23%, , SMG$M_BOLD)

		CASE 2%
			IF (MVALUE = "Add")
			THEN
				WHEN ERROR IN
					GET #WP_REQJOUR.CH%, &
						KEY #0% EQ WP_REQJOUR::JOB + &
						WP_REQJOUR::LLINE, REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_REQJOUR = 2%

				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
				GOTO 32767
			END IF

20350			!
			! Check for this order number in register file
			!
			IF MAIN_WINDOW(WP_MAIN_REGLINE.ID, "Q0" + &
				WP_REQJOUR::JOB + WP_REQJOUR::LLINE) <> 1%
			THEN
				WP_REGLINE::DESCR = ""

				CALL ENTR_3MESSAGE(SCOPE, &
					"Line Number not in Register", 0%)
			ELSE
				WP_REQJOUR::NOTES(0%) = WP_REGLINE::DESCR

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					WP_REQJOUR::NOTES(0%), 9%, 23%, , SMG$M_BOLD)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE::DESCR, 6%, 35%, , SMG$M_BOLD)

		CASE 4%
			!
			! Entry required in this field
			!
			IF WP_REQJOUR::OPERATOR = ""
			THEN
				WP_MAIN_REQJOUR = 1%
			END IF

		END SELECT

	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW (JC_MAIN_JOB.ID, &
				"Q0J" + WP_REQJOUR::JOB) <> 1%
			THEN
				JC_JOB::SSTATUS = ""
				JC_JOB::TTYPE   = ""
				JC_JOB::CLASS   = ""
				JC_JOB::DESCR   = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, 2%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, 3%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, 4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, 5%, 23%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(WP_MAIN_REGLINE.ID, "Q0" + &
				WP_REQJOUR::JOB + WP_REQJOUR::LLINE) <> 1%
			THEN
				WP_REGLINE::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE::DESCR, 6%, 35%, , SMG$M_BOLD)
		END IF

	!
	! Set WP_REQJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REQJOUR_OLD = WP_REQJOUR

	!
	! Restore WP_REQJOUR_OLD value
	!
	CASE OPT_RESETOLD
		WP_REQJOUR = WP_REQJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REQJOUR2 = WP_REQJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REQJOUR = WP_REQJOUR2

		IF MFLAG = 1%
		THEN
			WP_REQJOUR::REQDATE = DATE_TODAY &
				IF WP_REQJOUR::REQDATE = ""
		END IF

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  JobNumber  Line  ReqDate    Operator"

		CASE 2%
			MVALUE = "013, 019, 030"

		CASE 3%
			MVALUE = WP_REQJOUR::JOB + "  " + &
				WP_REQJOUR::LLINE + " "  + &
				PRNT_DATE(WP_REQJOUR::REQDATE, 8%) + " "  + &
				WP_REQJOUR::OPERATOR

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #WP_REQJOUR.CH%, &
				KEY #0% GE WP_REQJOUR::JOB + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap Errors
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
	!	.b
	!	.lm +5
	!	The ^*Line\* function
	!	enters data pertaining to material lines relative to a
	!	job when it was created in the Job Journal.
	!	.lm -5
	!
	! Index:
	!	.x Material Requisition<Line
	!	.x Line>Material Requisition
	!
	!--
