1	%TITLE "Manufacturing WIP Order Buyoff Journal"
	%SBTTL "WP_MAIN_BUYOFF_01"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_BUYOFF_01(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Manufacturing WIP Order Buyoff Journal\* option
	!	maintains the WIP Order Buyoff Journal.
	!	The Buyoff process is the process for indicating the line or
	!	lines that have been completed or cancelled for the WIP order
	!	register master file.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>WIP Order Buyoff Journal
	!	.x Batch Number>User
	!	.x User>Batch Number
	!	.x Journal>Buyoff Entry Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_BUYOFF_01/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_BUYOFF_01
	!	$ DELETE WP_MAIN_BUYOFF_01.OBJ;*
	!
	! Author:
	!
	!	12/22/95 - Kevin Handy
	!		Based upon WP_MAIN_BUYOFF
	!
	! Modification history:
	!
	!	12/22/95 - Kevin Handy
	!		Add code to handle WP_BUYOFF::TOLOCATION.
	!
	!	12/26/95 - Kevin Handy
	!		Lose field WP_BUYOFF::LOCATION.
	!		Reformat source closer to 80 columns.
	!
	!	05/21/98 - Kevin Handy
	!		Increase size of batch number from 2 to 8 characters.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/05/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF)		WP_BUYOFF_CDD		WP_BUYOFF
	MAP (WP_BUYOFF2)	WP_BUYOFF_CDD		WP_BUYOFF_OLD, WP_BUYOFF2
	COM (WP_BUYOFF_PAGE)	WP_BUYOFF_CDD		WP_BUYOFF_PAGE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_BUYOFF_01LINE.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (LOCATION) &
		LOCATION$ = 4%

	COM (CH_WP_BUYOFF) &
		WP_BUYOFF.CH%

	COM (CH_WP_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_MAIN_CHART
	EXTERNAL LONG	FUNCTION JC_MAIN_JOB
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Buy Off Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "WP_MAIN_BUYOFF_01"
		SMG_WINDOW::CHAN  = WP_BUYOFF.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
			SMG_WINDOW::KNAME(0%) = "Job Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20020 IF WP_BUYOFF.CH% > 0%

		!
		! Open WP_BUYOFF
		!
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.CRE"

20020		GOTO 20050 IF SB_SUBACCOUNT.CH% > 0%

		!
		! Open SB_SUBACCOUNT
		!
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"

20050		SMG_WINDOW::CHAN  = WP_BUYOFF.CH%
		WHEN ERROR IN
			RESET #WP_BUYOFF.CH%
			GET #WP_BUYOFF.CH%, REGARDLESS
		USE
			CONTINUE 32767
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

			DATA	2,  3, "(01) Job #", &
				3,  3, "     Status", &
				4,  3, "     Description", &
				5,  3, "     Type", &
				6,  3, "     Class", &
				7,  3, "(02) WIP Account #", &
				8,  3, "(03) Operator", &
				9,  3, "(04) To Location", &
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
	!	the job number for which a buy off is to be executed.
	!	.b
	!	A valid job number is required in this field. The field will accommodate ten
	!	(10) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this field, will
	!	cause a list of valid job numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Job Number>Buy off Journal
	!	.x Buy off Journal>Job Number
	!
	!--
			WP_BUYOFF::JOB  = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;23", TEMP$, WP_BUYOFF::JOB, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX") = 1%
				THEN
					WP_BUYOFF::JOB = JC_JOB::JOB
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field enters the
	!	General Ledger work in process account which is to be credited.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid General Ledger chart of accounts to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Buy off Journal>Account
	!	.x Account>Buy off Journal
	!
	!--
			WP_BUYOFF::ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;23", TEMP$, WP_BUYOFF::ACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					WP_BUYOFF::ACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters an
	!	identification (name, initials or number) for the operator who is responsible
	!	for entering the record.
	!	.b
	!	This field must contain a value.
	!	.lm -5
	!
	! Index:
	!	.x Buy off Journal>Operator
	!	.x Operator>Buy off Journal
	!
	!--
			WP_BUYOFF::OPERATOR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;23", TEMP$, WP_BUYOFF::OPERATOR, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) To Location\*
	!	.b
	!	.lm +5
	!	Location to buy off to.
	!	.lm -5
	!
	! Index:
	!	.x Buy off Journal>Operator
	!	.x Operator>Buy off Journal
	!
	!--
			WP_BUYOFF::TOLOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;23", TEMP$, WP_BUYOFF::TOLOCATION, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					WP_BUYOFF::TOLOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				WP_BUYOFF::TOLOCATION = &
					UTL_LOCATION::LOCATION
				GOTO ReEnter

			END SELECT

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_BUYOFF_01 = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Fix WP_BUYOFF::JOB
			!
			IF WP_BUYOFF::JOB = ""
			THEN
				WP_MAIN_BUYOFF_01 = 1%
				GOTO 32767
			END IF

			!
			! Check if already in journal
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #WP_BUYOFF.CH%, &
						KEY #0% EQ WP_BUYOFF::JOB + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_BUYOFF_01 = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				GOTO 32767
			END IF

20350			!
			! Check for this order number in register file
			! MUST BE THERE
			!
			WHEN ERROR IN
				GET #SB_SUBACCOUNT.CH%, &
					KEY #0% EQ "J" + WP_BUYOFF::JOB, &
					REGARDLESS
			USE
				IF ERR = 155%
				THEN
					WP_MAIN_BUYOFF_01 = 1%
					CONTINUE 32767
				END IF
				EXIT HANDLER
			END WHEN

			LOCATION$ = JC_JOB::LOCATION
 !			WP_BUYOFF::LOCATION = JC_JOB::LOCATION

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, &
				3%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, &
				4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, &
				5%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, &
				6%, 23%, , SMG$M_BOLD)

		CASE 2%
			WP_MAIN_BUYOFF_01 = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_BUYOFF::ACCT, GL_CHART::DESCR, &
				"WP", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				7%, 45%, , SMG$M_BOLD)

		CASE 3%
			IF WP_BUYOFF::OPERATOR = ""
			THEN
				WP_MAIN_BUYOFF_01 = 1%
				GOTO 32767
			END IF

		CASE 4%
			!
			! Display the descriptions for location name
			!
			WP_MAIN_BUYOFF_01 = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_BUYOFF::TOLOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 9%, 34%, , SMG$M_BOLD)

		END SELECT


	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			JC_JOB::SSTATUS = ""
			JC_JOB::TTYPE = ""
			JC_JOB::CLASS = ""
			JC_JOB::DESCR = ""

20475			WHEN ERROR IN
				GET #SB_SUBACCOUNT.CH%, &
					KEY #0% EQ "J" + WP_BUYOFF::JOB, &
					REGARDLESS
			USE
				CONTINUE 20476 IF ERR = 155% OR 11%
				EXIT HANDLER
			END WHEN

20476			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, &
				3%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, &
				4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, &
				5%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, &
				6%, 23%, , SMG$M_BOLD)


		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				WP_BUYOFF::ACCT) <> 1%)
			THEN
				GL_CHART::DESCR = "??????????????????????????????"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				7%, 45%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = FUNC_TESTENTRY(SMG_WINDOW, &
			WP_BUYOFF::TOLOCATION, &
			UTL_LOCATION::LOCNAME, &
			"OE", MLOOP, "PROG", &
			"Location", UTL_MAIN_LOCATION.ID)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, 9%, 34%, , SMG$M_BOLD)

	!
	! Set WP_BUYOFF_OLD value
	!
20500	CASE OPT_SETOLD
		WP_BUYOFF_OLD = WP_BUYOFF

	!
	! Restore WP_BUYOFF_OLD value
	!
	CASE OPT_RESETOLD
		WP_BUYOFF = WP_BUYOFF_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_BUYOFF2 = WP_BUYOFF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_BUYOFF = WP_BUYOFF2

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  JobNumber  AccountNumber"

		CASE 2%
			MVALUE = "013"

		CASE 3%
			MVALUE = WP_BUYOFF::JOB + " " + &
				WP_BUYOFF::ACCT

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #WP_BUYOFF.CH%, &
				KEY #0% GE WP_BUYOFF::JOB + "", &
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
	!	^*Work In Process Buyoff Line Journal\*
	!	.b
	!	.lm +5
	!	Enter the number of
	!	products completed and/or cancelled for the specified journal line.
	!	.lm -5
	!
	! Index:
	!
	!--
