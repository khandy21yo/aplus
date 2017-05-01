1	%TITLE "Manufacturing WIP Order Entry Journal"
	%SBTTL "WP_MAIN_JOBJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_JOBJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	.B
	!	.LM +5
	!	The Work In Process Order Entry Journal is entered and maintained through the
	!	^*Manufacturing WIP Order Entry Journal\* option.
	!	.LM -5
	!
	! Index:
	!	.x Maintain>WIP Order Entry Journal
	!	.x Batch Number>User
	!	.x User Batch Number
	!	.x Journal>Entry Maintain
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_JOBJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_JOBJOUR
	!	$ DELETE WP_MAIN_JOBJOUR.OBJ;*
	!
	! Author:
	!
	!	05/25/91 - Val James Allen ESQ.
	!
	! Modification history:
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key feature.
	!
	!	03/17/92 - Dan Perkins
	!		Moved field descriptons two to left
	!		to make display look better.
	!
	!	05/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	07/15/92 - Dan Perkins
	!		Removed OPT_AFTEROPT.  Reworked code.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/14/96 - Kevin Handy
	!		Lose trash in documentation.
	!		Reformat source code.
	!
	!	05/18/98 - Kevin Handy
	!		Increase the size of the batch number from
	!		2 characters to 8.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB
	MAP (WP_JOB_ONE)	WP_JOB_CDD		WP_JOB_ONE
	MAP (WP_JOB_OLD)	WP_JOB_CDD		WP_JOB_OLD
	MAP (WP_JOB_HOLD)	WP_JOB_CDD		WP_JOB_HOLD
	COM (WP_JOB_DEF)	WP_JOB_CDD		WP_JOB2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_ORDERLINE.
	!
	COM (CH_WP_JOB) &
		WP_JOB.CH%, SB_SUBACCOUNT.CH%

	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_CONTROL) &
		WP_CONTROL.CH%

	COM (CH_WP_ORDERLINE) &
		WP_ORDERLINE.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	!
	! Declare data
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
		SMG_WINDOW::DESCR  = "WIP Order Entry Journal " + BATCH_NO$
		SMG_WINDOW::NHELP  = "WP_MAIN_JOBJOUR"
		SMG_WINDOW::CHAN   = WP_JOB.CH%
		SMG_WINDOW::HSIZE  =  78%
		SMG_WINDOW::VSIZE  =  18%
		SMG_WINDOW::HVIEW  = 130%
		SMG_WINDOW::VVIEW  =  18%
		SMG_WINDOW::HPOS   =   2%
		SMG_WINDOW::VPOS   =   2%
		SMG_WINDOW::NITEMS =  10%

		SMG_WINDOW::FLAGS  =   0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   =  2%
		SMG_WINDOW::LVPOS   =  5%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Job_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "job_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 1%
			SMG_WINDOW::KFIELD(1%, 2%) = 3%
		SMG_WINDOW::KNAME(2%) = "job_Class"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 1%
			SMG_WINDOW::KFIELD(2%, 2%) = 4%

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF WP_JOB.CH% > 0%

		!
		! Open WP_JOB
		!
		%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.CRE"

20040		SMG_WINDOW::CHAN  = WP_JOB.CH%
		WHEN ERROR IN
			RESET #WP_JOB.CH%
			GET #WP_JOB.CH%, REGARDLESS
		USE
			CONTINUE 20050 IF ERR = 11%
			EXIT HANDLER
		END WHEN

20050		GOTO ExitFunction IF SB_SUBACCOUNT.CH% > 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
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

			DATA	1,  1, "(01) Job #", &
				2,  1, "(02) Description", &
				3,  1, "(03) Type", &
				4,  1, "(04) Class", &
				5,  1, "(05) Job Date", &
				6,  1, "(06) Location", &
				7,  1, "(07) Operator", &
				8,  1, "(08) Reference #", &
				9,  1, "(09) Notes", &
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
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
	!	The ^*Job Number\* field
	!	enters the Job Number for a job or work order.
	!	.b
	!	An entry is required in this field. The field will accommodate up to ten (10)
	!	alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will cause
	!	a list of current jobs in the register file to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Job Number
	!
	!--
			WP_JOB::JOB = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;19", TEMP$, &
				WP_JOB::JOB, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX") = 1%
				THEN
					WP_JOB::JOB = JC_JOB::JOB
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(JC_MAIN_JOB.ID, "M")
				WP_JOB::JOB = JC_JOB::JOB
				GOTO ReEnter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Job Description\*
	!	.b
	!	.lm +5
	!	The ^*Job Description\* field
	!	enters a description of a job or work order.
	!	.b
	!	This field will accommodate up to forty (40) characters.
	!	.lm -5
	!
	! Index:
	!	.x Job Description
	!
	!--
			WP_JOB::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;19", TEMP$, &
				WP_JOB::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Job Type\*
	!	.b
	!	.lm +5
	!	The ^*Job Type\* field enters
	!	a code which identifies the type of job being entered.
	!	.b
	!	Job Types are user created in the Job Type Table.
	!	.b
	!	This field accommodates up to two (2) characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid job types to be displayed.
	!	.b
	!	Pressing ^*<F17>\* while the cursor is located at this field will allow the
	!	addition of a Job Type record in the Job Type file without having to exit the
	!	Job Order journal.
	!	.lm -5
	!
	! Index:
	!	.x Job Type
	!
	!--
			WP_JOB::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;19", TEMP$, &
				WP_JOB::TTYPE, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(JC_MAIN_TYPE.ID, "V0") = 1%
				THEN
					WP_JOB::TTYPE = JC_TYPE::TTYPE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(JC_MAIN_TYPE.ID, "M")
				WP_JOB::TTYPE = JC_TYPE::TTYPE
				GOTO ReEnter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Job Class\*
	!	.b
	!	.lm +5
	!	The ^*Job Class\* field
	!	specifies the Job Class for an order.
	!	.b
	!	The Job Classes are user created in the Job Class Table.
	!	.b
	!	This field will accommodate up to four (4) characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid job class codes to be displayed.
	!	.b
	!	Pressing ^*<F17>\* while the cursor is located at this field will allow an
	!	addition of a Job Class record to the Job Class file without having to exit
	!	the Job Order journal.
	!	.lm -5
	!
	! Index:
	!	.x Job Class
	!
	!--
			WP_JOB::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;19", TEMP$, &
				WP_JOB::CLASS, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(JC_MAIN_CLASS.ID, "V0") = 1%
				THEN
					WP_JOB::CLASS = JC_CLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(JC_MAIN_CLASS.ID, "M")
				WP_JOB::CLASS = JC_CLASS::CLASS
				GOTO ReEnter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Job Date\*
	!	.lm +5
	!	.b
	!	The ^*Job Date\* field contains the date a
	!	job or work order is entered in the system.
	!	.b
	!	The system will automatically default to todays date.  The date may be
	!	overridden by entering the correct date and pressing ^*Return\*.
	!	.b
	!	The format for entering this field is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Job Date
	!
	!--
			WP_JOB::BDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;19", TEMP$, WP_JOB::BDATE, MFLAG, &
				"'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Location\*
	!	.lm +5
	!	.b
	!	The ^*Location\* field enters
	!	a user defined code to identify the company location at which a job will be
	!	produced.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause valid location choices to be displayed.
	!	.b
	!	Pressing ^*<F17>\* while the cursor is located at this field will allow an
	!	addition of a Location record in the company profile without having to exit
	!	the Job Order journal.
	!	.lm -5
	!
	! Index:
	!	.x Location
	!
	!--
			WP_JOB::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;19", TEMP$, WP_JOB::LOCATION, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					WP_JOB::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				WP_JOB::LOCATION = UTL_LOCATION::LOCATION
				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Operator\*
	!	.lm +5
	!	.b
	!	The ^*Operator\* field enters
	!	the name or initials of the person entering a job in the journal.
	!	.b
	!	The use of this field is optional.
	!	.b
	!	This field will accommodate up to ten (10) characters.
	!	.lm -5
	!
	! Index:
	!	.x Operator
	!
	!--
			WP_JOB::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;19", TEMP$, &
				WP_JOB::OPERATOR, MFLAG, &
				"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Reference _#\*
	!	.lm +5
	!	.b
	!	The ^*Reference _#\* field
	!	specifies a reference number such as a purchase order number or some other
	!	document authorizing the creation of a job or work order.
	!	.b
	!	The use of this field is optional.
	!	.b
	!	This field will accommodate up to sixteen (16) alphanumerical characters.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			WP_JOB::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;19", TEMP$, WP_JOB::REFNO, MFLAG, &
				"'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Notes\*
	!	.lm +5
	!	.b
	!	The ^*Notes\* field enters free
	!	formated notes or comments in reference to an order.
	!	.b
	!	There are two lines available for the notes.  Each line can contain up to
	!	forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Notes
	!
	!--
 FirstNote:
			WP_JOB::NOTES(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;19", TEMP$, WP_JOB::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			WP_JOB::NOTES(1%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;19", TEMP$, WP_JOB::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_JOBJOUR = 0%

		SELECT MLOOP

		CASE 1%
			IF WP_JOB::JOB = ""
			THEN
				WP_MAIN_JOBJOUR = 1%
				GOTO 32767
			END IF

			!
			! Check if already in journal
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #WP_JOB.CH%, &
						KEY #0% EQ WP_JOB::JOB + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_JOBJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
				GOTO 32767
			END IF

20350			!
			! Check for this order number already in register file
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					FIND #SB_SUBACCOUNT.CH%, &
						KEY #0% EQ "J" + WP_JOB::JOB, &
						REGARDLESS
				USE
					CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
					EXIT HANDLER
				END WHEN

				PRG_HOLD$ = SCOPE::PRG_ITEM

				SCOPE::PRG_ITEM = "ORDEX"

	! ABSTRACT:ORDEX
	! .p
	! You have entered a order number that already exists in the WIP order
	! register file, if you choose to use this number then the lines that
	! you enter will be appended to the order that exists in the Register
	! file sequencially.  The header information may be changed and will
	! be printed as changed but will not be carried forward to the register
	! header.
	!
				INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
					"", "Order already in Register - Enter Y to append - then press <Do> ", "Y", 0%, "", "")

				SCOPE::PRG_ITEM = PRG_HOLD$

				IF INP$ = "Y"
				THEN
					GOSUB LoadFromRegister
					GOTO 32767
				ELSE
					WP_MAIN_JOBJOUR = 1%
				END IF
			END IF

		CASE 3%
			!
			! Display the description for type
			!
			WP_MAIN_JOBJOUR = FUNC_TESTENTRY( SMG_WINDOW, &
				WP_JOB::TTYPE, &
				JC_TYPE::DESCR, &
				"WP", MLOOP, "PRG", &
				"Job Type", JC_MAIN_TYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_TYPE::DESCR, &
				3%, 34%, , SMG$M_BOLD)

		CASE 4%
			!
			! Display the description for class
			!
			WP_MAIN_JOBJOUR = FUNC_TESTENTRY( SMG_WINDOW, &
				WP_JOB::CLASS, &
				JC_CLASS::DESCR, &
				"WP", MLOOP, "PRG", &
				"Class", JC_MAIN_CLASS.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_CLASS::DESCR, &
				4%, 34%, , SMG$M_BOLD)

		CASE 6%
			!
			! Display the descriptions for location name
			!
			WP_MAIN_JOBJOUR = FUNC_TESTENTRY( SMG_WINDOW, &
				WP_JOB::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"WP", MLOOP, "PRG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				6%, 34%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			!
			! Display the description for type
			!
			JC_TYPE::DESCR = STRING$(LEN(JC_TYPE::DESCR), A"?"B) &
				IF MAIN_WINDOW(JC_MAIN_TYPE.ID, "Q0" + &
				WP_JOB::TTYPE) <> 1%

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					JC_TYPE::DESCR, &
					3%, 34%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display the description for class
			!
			JC_CLASS::DESCR = STRING$(LEN(JC_CLASS::DESCR), A"?"B) &
				IF MAIN_WINDOW(JC_MAIN_CLASS.ID, "Q0" + &
				WP_JOB::CLASS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_CLASS::DESCR, &
				4%, 34%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + WP_JOB::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				6%, 34%, , SMG$M_BOLD)
		END IF

	!
	! Set WP_JOB_OLD value
	!
20500	CASE OPT_SETOLD
		WP_JOB_OLD = WP_JOB

	!
	! Restore WP_JOB_OLD value
	!
	CASE OPT_RESETOLD
		WP_JOB = WP_JOB_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_JOB2 = WP_JOB

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_JOB = WP_JOB2

		IF MFLAG = 1%
		THEN
			WP_JOB::BDATE = DATE_TODAY IF WP_JOB::BDATE = ""

			GOSUB GetRec IF WP_JOB::JOB = ""
		END IF

	!
	! View the Record.
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "  Order Number   Date        Type  Class Location Reference Number Description"

		CASE 2%
			MVALUE = "017,029,035,041,050,067"

		CASE 3%
			MVALUE = WP_JOB::JOB + "     " + &
				PRNT_DATE(WP_JOB::BDATE, 8%) + "  " + &
				WP_JOB::TTYPE + "    " + &
				WP_JOB::CLASS + "  " + &
				WP_JOB::LOCATION + "     " + &
				WP_JOB::REFNO + " " + &
				WP_JOB::DESCR

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #WP_JOB.CH%, &
				KEY #0% GE WP_JOB::JOB + "", &
				REGARDLESS

		CASE 1%
			FIND #WP_JOB.CH%, &
				KEY #1% GE WP_JOB::TTYPE + &
				WP_JOB::JOB, REGARDLESS

		CASE 2%
			FIND #WP_JOB.CH%, KEY #2% GE WP_JOB::CLASS + &
				WP_JOB::JOB, REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

 GetRec:
	!
	! Open WP_CONTROL
	!
20800	IF WP_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.CRE"
		USE
			CONTINUE 20850 IF ERR = 9% OR ERR = 155%
			EXIT HANDLER
		END WHEN
	END IF

	!
	! Get the WP_CONTROL record
	!
	WHEN ERROR IN
		GET #WP_CONTROL.CH%, RECORD 1%
		V% = FUNC_INCREMENT(WP_CONTROL::ORDNUM)
		UPDATE #WP_CONTROL.CH%
	USE
		CONTINUE 20850 IF ERR = 9% OR ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO AssgNumber

20850	!
	! Set up a control file record if none exists
	!
	WP_CONTROL::ORDNUM      = "0000000001"
	WP_CONTROL::PURGDATE    = ""
	WP_CONTROL::STATUS_FLAG = "0"
	WP_CONTROL::REQNUM      = "0000000001"

	PUT #WP_CONTROL.CH%

 AssgNumber:
	WP_JOB::JOB = WP_CONTROL::ORDNUM

	RETURN

20860
 LoadFromRegister:
	GET #SB_SUBACCOUNT.CH%

	IF JC_JOB::JOB <> WP_JOB::JOB
	THEN
		WP_MAIN_JOBJOUR = 1%
		RETURN
	END IF

	WP_JOB::DESCR    = JC_JOB::DESCR
	WP_JOB::TTYPE    = JC_JOB::TTYPE
	WP_JOB::CLASS    = JC_JOB::CLASS
	WP_JOB::BDATE    = JC_JOB::BDATE
	WP_JOB::LOCATION = JC_JOB::LOCATION
	WP_JOB::OPERATOR = JC_JOB::OPERATOR
	WP_JOB::REFNO    = JC_JOB::REFNO

	RETURN

29000	!*******************************************************************
	! Help Errors
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
	!	.LM +5
	!	.B
	!	The ^*Line\* function accesses
	!	the lines of the job or work order. The lines contain the information as to
	!	type of line, the operation or product description, cost, and quantity.
	!	.LM -5
	!
	! Index:
	!	.x Lines>Work Order
	!	.x Lines>Job Order
	!	.x Job Order>Lines
	!	.x Work Order>Lines
	!
	!--
	!+-+-+
	!++
	! Abstract:ORDEX
	!	.p
	!	You have entered a order number that already exists in the WIP order
	!	register file, if you choose to use this number then the lines that
	!	you enter will be appended to the order that exists in the Register
	!	file sequencially.  The header information may be changed and will
	!	be printed as changed but will not be carried forward to the register
	!	header.
	!
	! Index:
	!
	!--
