1	%TITLE "WIP Closing Journal"
	%SBTTL "WP_MAIN_CLOSEJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_CLOSEJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	$ BAS WP_SOURCE:WP_MAIN_CLOSEJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_CLOSEJOUR
	!	$ DELETE WP_MAIN_CLOSEJOUR.OBJ;*
	!
	! Author:
	!
	!	07/21/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	08/31/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/04/92 - Dan Perkins
	!		Rewrote code from line 27000 to 28000.
	!		Look for SB_BALANCE according to job and not period.
	!		Allow for wildcards in SB_ACCOUNT.
	!		Separated CASE PMAT, RMAT into separate cases.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/18/95 - Kevin Handy
	!		Clean up unecessary external statements.
	!
	!	09/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/21/98 - Kevin Handy
	!		Increase batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/15/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR
	MAP (WP_CLOSEJOUR_OLD)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR_OLD
	MAP (WP_CLOSEJOUR2)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR2
	COM (WP_CLOSEJOUR_ONE)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR_ONE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_CLOSEJOURLINE.
	!
	COM (CH_WP_CLOSEJOUR) &
		WP_CLOSEJOUR.CH%

	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8%

	COM (CH_WP_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	COM (TT_WP_MAIN_CLOSEJOUR) &
		STITLE$ = 30%, &
		SSTAT$(3%) = 30%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION JC_MAIN_JOB
	EXTERNAL LONG	FUNCTION WP_WRIT_VARIANCE

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
		SMG_WINDOW::DESCR = "WIP Closing Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "WP_MAIN_CLOSEJOUR"
		SMG_WINDOW::CHAN  = WP_CLOSEJOUR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
			SMG_WINDOW::KNAME(0%) = "Job_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		STITLE$ = "Group   Description"
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "E      Equipment"
		SSTAT$(2%) = "P      Parts"
		SSTAT$(3%) = "       Not Specified"

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		IF WP_CLOSEJOUR.CH% <= 0%
		THEN
			!
			! Open WP_CLOSEJOUR
			!
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.CRE"

		END IF

20020		IF SB_SUBACCOUNT.CH% <= 0%
		THEN
			!
			! Open SB_SUBACCOUNT
			!
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		END IF

20050		SMG_WINDOW::CHAN = WP_CLOSEJOUR.CH%

		WHEN ERROR IN
			RESET #WP_CLOSEJOUR.CH%
			GET #WP_CLOSEJOUR.CH%
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
				7,  3, "(02) Closing Date", &
				8,  3, "(03) Operator", &
				9,  3, "(04) Var Group", &
				10,  14, "            Burden       Labor      Parts     RawMat      Total", &
				11, 14, "STD", &
				12, 14, "Actual", &
				0,   0, ""

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
	!	The ^*Job Number\* field enters the job number of
	!	the job which is to be closed.
	!	.b
	!	An entry is required in this field. The field will accommodate up to ten (10)
	!	alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this field, will
	!	cause a list of valid job numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Order Number
	!
	!--
			WP_CLOSEJOUR::JOB = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;23", TEMP$, &
				WP_CLOSEJOUR::JOB, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX") = 1%
				THEN
					WP_CLOSEJOUR::JOB = JC_JOB::JOB
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Closing Date
	!	^*(02) Closing Date\*
	!	.b
	!	.lm +5
	!	The ^*Closing Date\* field enters the date a job is closed.
	!	.b
	!	The system date will automatically be defaulted to todays date and can be
	!	overridden by entering the correct date and pressing ^*<Return>\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CLOSEJOUR::CLOSEDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;23", TEMP$, WP_CLOSEJOUR::CLOSEDATE, &
				MFLAG, "'E", MVALUE)


		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the name, initials or
	!	identification number of the person responsible for entering a record.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Closing>Operator
	!
	!--
			WP_CLOSEJOUR::OPERATOR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;23", TEMP$, WP_CLOSEJOUR::OPERATOR, &
				MFLAG, "'E", MVALUE)


		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Var Group\*
	!	.lm +5
	!	.b
	!	The ^*Variance Group\* field enters the variance group.
	!	.LM -5
	!
	! Index:
	!	.x Variance Group
	!	.x Close Job>Variance Group
	!
	!--
			WP_CLOSEJOUR::VARFLAG = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;23", TEMP$, WP_CLOSEJOUR::VARFLAG, &
				MFLAG, "'E", MVALUE, SSTAT$(), &
				STITLE$, "007"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_CLOSEJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Fix WP_CLOSEJOUR::JOB
			!
			IF WP_CLOSEJOUR::JOB = ""
			THEN
				WP_MAIN_CLOSEJOUR = 1%
				GOTO 32767
			END IF

			!
			! Check if already in journal
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #WP_CLOSEJOUR.CH%, &
						KEY #0% EQ WP_CLOSEJOUR::JOB + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_CLOSEJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
				GOTO 32767
			END IF

20350			!
			! Check for this order number in register file
			! MUST BE THERE
			!
			WHEN ERROR IN
				GET #SB_SUBACCOUNT.CH%, &
					KEY #0% EQ "J" + WP_CLOSEJOUR::JOB, &
					REGARDLESS
			USE
				IF ERR = 155%
				THEN
					WP_MAIN_CLOSEJOUR = 1%
					CONTINUE 32767
				END IF
				EXIT HANDLER
			END WHEN

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::SSTATUS, 3%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, 4%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::TTYPE, 5%, 23%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::CLASS, 6%, 23%, , SMG$M_BOLD)

		CASE 4%
			IF (MVALUE = "ADD")
			THEN

				SCOPE::PRG_ITEM = "CONFIRM"

				IF WP_CLOSEJOUR::VARFLAG <> ""
				THEN
					WP_CLOSEJOUR_ONE::VARFLAG = &
						ENTR_3YESNO(SCOPE, &
						SMG_WINDOW::WNUMBER, &
						"", &
						"Confirm closing now - then press <Do> ", &
						"Y", 0%, "", "")

					GOTO ExitFunction &
						IF WP_CLOSEJOUR_ONE::VARFLAG <> "Y"
				ELSE
					WP_CLOSEJOUR_ONE::VARFLAG = "Y"
				END IF

				CALL ENTR_3MESSAGE(SCOPE, &
					"Testing and calculating", 1% + 16%)

				SELECT WP_WRIT_VARIANCE(WP_CLOSEJOUR, TEXT$)

				CASE CMC$_TERMINATED

					CALL ENTR_3MESSAGE(SCOPE, TEXT$, 0%)
					MLOOP = 1%
					WP_MAIN_CLOSEJOUR = 1%

				END SELECT

				ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + &
					WP_CLOSEJOUR::ACTLABOR + &
					WP_CLOSEJOUR::ACTPARTS + &
					WP_CLOSEJOUR::ACTRAWMAT

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::ACTBURDEN, "###,###.##"), &
					12%, 22%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::ACTLABOR, " ###,###.##"), &
					12%, 33%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::ACTPARTS, " ###,###.##"), &
					12%, 44%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::ACTRAWMAT, " ###,###.##"), &
					12%, 55%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(ACT_TOTAL, " ###,###.##"), &
					12%, 66%, , SMG$M_BOLD)

				STD_TOTAL = WP_CLOSEJOUR::STDBURDEN + WP_CLOSEJOUR::STDLABOR + &
					WP_CLOSEJOUR::STDPARTS + WP_CLOSEJOUR::STDRAWMAT

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::STDBURDEN, "###,###.##"), &
					11%, 22%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::STDLABOR, " ###,###.##"), &
					11%, 33%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::STDPARTS, " ###,###.##"), &
					11%, 44%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(WP_CLOSEJOUR::STDRAWMAT, " ###,###.##"), &
					11%, 55%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(STD_TOTAL, " ###,###.##"), &
					11%, 66%, , SMG$M_BOLD)

				CALL ENTR_3MESSAGE(SCOPE, "", 1%)
				SCOPE::PRG_ITEM = "Add"
			END IF

		END SELECT

	CASE OPT_DISPLAY

		JC_JOB::SSTATUS = ""
		JC_JOB::TTYPE = ""
		JC_JOB::CLASS = ""
		JC_JOB::DESCR = ""

20475		WHEN ERROR IN
			GET #SB_SUBACCOUNT.CH%, &
				KEY #0% EQ "J" + WP_CLOSEJOUR::JOB, &
				REGARDLESS
		USE
			CONTINUE 20476 IF ERR = 155% OR 11%
			EXIT HANDLER
		END WHEN

20476		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_JOB::SSTATUS, 3%, 23%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_JOB::DESCR, 4%, 23%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_JOB::TTYPE, 5%, 23%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_JOB::CLASS, 6%, 23%, , SMG$M_BOLD)

		STD_TOTAL = WP_CLOSEJOUR::STDBURDEN + &
			WP_CLOSEJOUR::STDLABOR + &
			WP_CLOSEJOUR::STDPARTS + WP_CLOSEJOUR::STDRAWMAT

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::STDBURDEN, "###,###.##"), &
			11%, 22%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::STDLABOR, " ###,###.##"), &
			11%, 33%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::STDPARTS, " ###,###.##"), &
			11%, 44%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::STDRAWMAT, " ###,###.##"), &
			11%, 55%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(STD_TOTAL, " ###,###.##"), &
			11%, 66%, , SMG$M_BOLD)

		ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + WP_CLOSEJOUR::ACTLABOR + &
			WP_CLOSEJOUR::ACTPARTS + WP_CLOSEJOUR::ACTRAWMAT

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::ACTBURDEN, "###,###.##"), &
			12%, 22%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::ACTLABOR, " ###,###.##"), &
			12%, 33%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::ACTPARTS, " ###,###.##"), &
			12%, 44%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(WP_CLOSEJOUR::ACTRAWMAT, " ###,###.##"), &
			12%, 55%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(ACT_TOTAL, " ###,###.##"), &
			12%, 66%, , SMG$M_BOLD)

	!
	! Set WP_CLOSEJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		WP_CLOSEJOUR_OLD = WP_CLOSEJOUR

	!
	! Restore WP_CLOSEJOUR_OLD value
	!
	CASE OPT_RESETOLD
		WP_CLOSEJOUR = WP_CLOSEJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_CLOSEJOUR2 = WP_CLOSEJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_CLOSEJOUR = WP_CLOSEJOUR2

		IF MFLAG = 1%
		THEN
			WP_CLOSEJOUR::CLOSEDATE = DATE_TODAY &
				IF WP_CLOSEJOUR::CLOSEDATE = ""
		END IF

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  JobNumber  "

		CASE 2%
			MVALUE = "013"

		CASE 3%
			MVALUE = WP_CLOSEJOUR::JOB

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #WP_CLOSEJOUR.CH%, &
				KEY #0% GE WP_CLOSEJOUR::JOB + "", &
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
	!	The Work In Process Buyoff Line Journal enters
	!	the number of products completed and/or cancelled for the specified
	!	journal line.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:PRINT_REPORT
	!	^*Print__report\*
	!	.lm +5
	!	.b
	!	The ^*Print__report\* function
	!	prints header and line item detail in reference to closed jobs.
	!	.lm -5
	!
	! Index:
	!	.x Print>Closing Journal
	!	.x Closing>Journal>Print
	!
	!--
	!+-+-+
	!++
	! Abstract:WIP_ACCOUNTS
	!	^*Wip__accounts\*
	!	.lm +5
	!	.b
	!	The ^* Wip__accounts\* function
	!	enters the WIP accounts.
	!	.lm -5
	!
	! Index:
	!	.x Work in Process>Accounts
	!
	!--
	!+-+-+
	!++
	! Abstract:VARIANCES
	!	^*varianceS\*
	!	.lm +5
	!	.b
	!	The ^*varianceS\* field
	!	views or edits variance classifications.
	!	.lm -5
	!
	! Index:
	!	.x Variances
	!	.x Labor>Variances
	!	.x Burden>Variances
	!	.x Parts>Variances
	!	.x Raw Material>Variances
	!
	!--
