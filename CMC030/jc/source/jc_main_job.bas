1	%TITLE "Job Master File"
	%SBTTL "JC_MAIN_JOB"
	%IDENT "V3.6a Calico"

	FUNCTION LONG JC_MAIN_JOB(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	All job descriptions including each new job is entered and maintained through
	!	the ^*Job Master File\*.
	!	.lm -5
	!
	! Index:
	!	.x Sub Account Maintenance
	!	.x Maintenance>Sub Account
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_MAIN_JOB/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN JC_MAIN_JOB
	!	$ DELETE JC_MAIN_JOB.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James "The hook" Allen
	!
	! Modification history:
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key option.
	!
	!	12/10/91 - Kevin Handy
	!		Removed "~L0" business on job number, since
	!		posts don't add 0's to job number, and it
	!		shows existing records out of sequence.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/13/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/23/93 - Kevin Handy
	!		Added "P" (purge) to list of possible status because
	!		it's being used in the file anyway.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/08/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB
	MAP (JC_JOB_OLD)	JC_JOB_CDD		JC_JOB_OLD
	MAP (JC_JOB_DEF)	JC_JOB_CDD		JC_JOB_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_JC_JOB) &
		SB_SUBACCOUNT.CH%, &
		JC_JOB.READONLY%

	COM (TT_JC_JOB) &
		STITLE$ = 30%, &
		SSTAT$(5%) = 30%

	!
	! Default Subject
	!
	DEF_SUBJECT$ = "J"

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Job Costing Order Maintenance"
		SMG_WINDOW::NHELP = "JC_MAIN_JOB"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 14%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Job"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Class"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 4%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "4"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "C      Close"
		SSTAT$(4%) = "P      Purge"

		!
		! Read Defaults
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_SUBACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF JC_JOB.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			JC_MAIN_JOB = ERR
			CONTINUE 770
		END WHEN

		JC_JOB.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			JC_MAIN_JOB = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		JC_JOB.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_SUBACCOUNT.CH%
		WHEN ERROR IN
			RESET #SB_SUBACCOUNT.CH%
			!GET #SB_SUBACCOUNT.CH%, REGARDLESS
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


		DATA	03,05, "(01) Job Number", &
			04,05, "(02) Description", &
			05,05, "(03) Job Type", &
			06,05, "(04) Job Class", &
			07,05, "(05) Open Date", &
			08,05, "(06) Current Status", &
			09,05, "(07) Close Date", &
			10,05, "(08) Location", &
			11,05, "(09) Ref #", &
			12,05, "(10) Operator", &
			13,05, "(11) Batch", &
			14,05, "(12) Post Time", &
			15,05, "(13) Post Date", &
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
	!	^*(01) Job Number\*
	!	.b
	!	.lm +5
	!	The ^*Job Number\* field reflects the job number of a job which has been
	!	posted from a job order journal.
	!	.b
	!	^*Do not edit this field in a register record.\*
	!	.lm -5
	!
	! Index:
	!	.x Job Number>Register
	!	.x Number>Job
	!
	!--
			JC_JOB::JOB = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;30",TEMP$, JC_JOB::JOB, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field reflects the description of a job as it was
	!	established in a job order journal.
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.b
	!	This field in a register record could be edited.
	!	.lm -5
	!
	! Index:
	!	.x Description>Job Maintenance Screen
	!	.x Job Maintenance Screen>Description
	!
	!--
			JC_JOB::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;30",	TEMP$, JC_JOB::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Job Type\*
	!	.b
	!	.lm +5
	!	The ^*Job Type\* field reflects the job type as originally established for
	!	a job in a job order journal.
	!	.lm -5
	!
	! Index:
	!	.x Job Type
	!
	!--
			JC_JOB::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, JC_JOB::TTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(JC_MAIN_TYPE.ID, "V0") = 1%
				THEN
					JC_JOB::TTYPE = JC_TYPE::TTYPE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(JC_MAIN_TYPE.ID, "M")
				JC_JOB::TTYPE = JC_TYPE::TTYPE
				GOTO ReEnter
			END IF


		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Job Class\*
	!	.b
	!	.lm +5
	!	The ^*Job Class\* field reflects the job class which
	!	was originally established for a job in a job order journal.
	!	.lm -5
	!
	! Index:
	!	.x Job Class
	!
	!--
			JC_JOB::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, JC_JOB::CLASS, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(JC_MAIN_CLASS.ID, "V0") = 1%
				THEN
					JC_JOB::CLASS = JC_CLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(JC_MAIN_CLASS.ID, "M")
				JC_JOB::CLASS = JC_CLASS::CLASS
				GOTO ReEnter
			END IF


		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Open Date\*
	!	.b
	!	.lm +5
	!	The ^*Open Date\* field reflects the date a job was opened
	!	in a job order journal.
	!	.lm -5
	!
	! Index:
	!	.x Open Date
	!
	!--
			JC_JOB::BDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, JC_JOB::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Current Status\*
	!	.b
	!	.lm +5
	!	The ^*Current Status\* field will indicate if the job is "Active", "Inactive"
	!	or "Closed".
	!	.b
	!	Valid codes for this field are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.TE
	!	*I\* - Inactive
	!	.te
	!	^*C\* - Closed
	!	.te
	!	*P - Purged
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Current Status
	!	.x Status>Current
	!
	!--
			JC_JOB::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				JC_JOB::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Close Date\*
	!	.b
	!	.lm +5
	!	The ^*Close Date\* field indicates the date a job was
	!	closed.
	!	.lm -5
	!
	! Index:
	!	.x Close Date
	!
	!--
			JC_JOB::EDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, JC_JOB::EDATE, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field indicates the company location
	!	at which a job or work order is to take place or be processed.
	!	.lm -5
	!
	! Index:
	!	.x Location>Job Description Maintenance
	!	.x Job Description Maintenance>Location
	!
	!--
			JC_JOB::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, &
				JC_JOB::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					JC_JOB::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				JC_JOB::LOCATION = UTL_LOCATION::LOCATION
				GOTO ReEnter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Reference _#\*
	!	.B
	!	.LM +5
	!	The ^*Reference _#\* field reflects the reference number
	!	as it was established in a Job Order journal.  If no reference number was
	!	entered when a job was established in a Job Order journal, this field will be
	!	blank.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			JC_JOB::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;30", TEMP$, &
				JC_JOB::REFNO, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field indicates the person who originally
	!	entered the job in a Job Order journal.
	!	.lm -5
	!
	! Index:
	!	.x Operator>Job Master File
	!	.x Job Master File>Operator
	!
	!--
			JC_JOB::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, &
				JC_JOB::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field indicates the six character
	!	system assigned batch number created when the Job Order journal (in which a job
	!	was entered) was posted.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!	.x Number>Batch
	!
	!--
			JC_JOB::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;30",TEMP$, JC_JOB::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Post Time\*
	!	.b
	!	.lm +5
	!	The ^*Post Time\* field contains the time of day
	!	the Job Order journal in which a job was created was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Time
	!	.x Time>Post
	!
	!--
			JC_JOB::POST_TIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;30",TEMP$, JC_JOB::POST_TIME, MFLAG, &
				"'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Post Date\*
	!	.b
	!	.lm +5
	!	The ^*Post Date\* field contains the date a Job Order journal
	!	in which a job was created was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Date
	!	.x Date>Post
	!
	!--
			JC_JOB::POST_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;30",TEMP$, JC_JOB::POST_DATE, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		JC_MAIN_JOB = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						JC_JOB::SUBJECT + &
						JC_JOB::JOB, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				JC_MAIN_JOB = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		CASE 3%
			!
			! Display the description for type
			!
			JC_MAIN_JOB = FUNC_TESTENTRY(SMG_WINDOW, &
				JC_JOB::TTYPE, &
				JC_TYPE::DESCR, &
				"JC", MLOOP, "PROG", &
				"Type", JC_MAIN_TYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_TYPE::DESCR, &
				5%, 40%, , SMG$M_BOLD)

		CASE 4%
			!
			! Display the description for class
			!
			JC_MAIN_JOB = FUNC_TESTENTRY(SMG_WINDOW, &
				JC_JOB::CLASS, &
				JC_CLASS::DESCR, &
				"JC", MLOOP, "PROG", &
				"Class", JC_MAIN_CLASS.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_CLASS::DESCR, &
				6%, 40%, , SMG$M_BOLD)

		CASE 8%
			!
			! Display the descriptions for location name
			!
			JC_MAIN_JOB = FUNC_TESTENTRY(SMG_WINDOW, &
				JC_JOB::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"JC", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				10%, 40%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			!
			! Display the description for type
			!
			JC_TYPE::DESCR = STRING$(LEN(JC_TYPE::DESCR), A"?"B) &
				IF MAIN_WINDOW(JC_MAIN_TYPE.ID, &
				"Q0" + JC_JOB::TTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_TYPE::DESCR, &
				5%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display the description for class
			!
			JC_CLASS::DESCR = STRING$(LEN(JC_CLASS::DESCR), A"?"B) &
				IF MAIN_WINDOW(JC_MAIN_CLASS.ID, &
				"Q0" + JC_JOB::CLASS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_CLASS::DESCR, &
				6%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + JC_JOB::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				10%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set JC_JOB_OLD value
	!
20500	CASE OPT_SETOLD
		JC_JOB_OLD = JC_JOB

	!
	! Restore JC_JOB_OLD value
	!
	CASE OPT_RESETOLD
		JC_JOB = JC_JOB_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		JC_JOB_DEF = JC_JOB

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		JC_JOB = JC_JOB_DEF
		JC_JOB::SUBJECT = DEF_SUBJECT$

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  JobNumber  Description            " + &
				"           Ty Clas OpenDate   S CloseDate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,047,050,055,066,068"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = JC_JOB::JOB + " " + &
				LEFT(JC_JOB::DESCR, 33%) + " " + &
				JC_JOB::TTYPE + " " + &
				JC_JOB::CLASS + " " + &
				PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
				JC_JOB::SSTATUS + " " + &
				PRNT_DATE(JC_JOB::EDATE, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE JC_JOB::SUBJECT + &
				JC_JOB::JOB, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE JC_JOB::SUBJECT + &
				JC_JOB::TTYPE + &
				JC_JOB::JOB, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE JC_JOB::SUBJECT + &
				JC_JOB::CLASS + &
				JC_JOB::JOB, REGARDLESS
		END SELECT

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
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ DEF_SUBJECT$, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
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
						KEY #0% GE JC_JOB::SUBJECT + &
						JC_JOB::JOB, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE JC_JOB::SUBJECT + &
						JC_JOB::TTYPE + &
						JC_JOB::JOB, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 2%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #2% GE JC_JOB::SUBJECT + &
						JC_JOB::CLASS + &
						JC_JOB::JOB, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
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
			SMG_WINDOW::CURREC = 0% &
				IF JC_JOB::SUBJECT = DEF_SUBJECT$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More menu option hidden in Mast.
	!++
	! Abstract:LINES
	!	^*Line\*
	!	.lm +5
	!	.b
	!	The ^*Line\* function
	!	accesses the line information related to each job.
	!	.lm -5
	!
	! Index:
	!	.x Line>Register
	!	.x Register>Line
	!
	!--

	!+-+-+
	!++
	! Abstract:BUDGET
	!	^*Budget\*
	!	.b
	!	.lm +5
	!	The ^*Budget\* option assigns a
	!	budget for each period of each job.
	!	.lm -5
	!
	! Index:
	!	.x Budget
	!
	!--
