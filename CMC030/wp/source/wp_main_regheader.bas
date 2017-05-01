1	%TITLE "Manufacturing WIP Order Register Maintenance"
	%SBTTL "WP_MAIN_REGHEADER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_REGHEADER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Manufacturing WIP Order Register Maintenance\* option maintains
	!	Orders in the register file.
	!	.b
	!	^*CAUTION - DO NOT USE THIS PROCESS OR CHANGE ANY RECORDS
	!	IN THE FILE USING THIS METHOD!!!!!!\*
	!	.lm -5
	!
	! Index:
	!	.x WIP Order Register Maintenance
	!	.x Maintenance>WIP Order Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_REGHEADER/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_REGHEADER
	!	$ DELETE WP_MAIN_REGHEADER.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James "The hook" Allen
	!
	! Modification history:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!		add '--' to end comment section.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGHEADER.HB"
	MAP (SB_SUBACCOUNT)	WP_REGHEADER_CDD		WP_REGHEADER
	MAP (WP_REGHEADER_OLD)	WP_REGHEADER_CDD		WP_REGHEADER_OLD
	MAP (WP_REGHEADER_DEF)	WP_REGHEADER_CDD		WP_REGHEADER_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD		SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD		UTL_LOCATION

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_WP_REGHEADER) &
		SB_SUBACCOUNT.CH%, &
		WP_REGHEADER.READONLY%

	COM (TT_WP_REGHEADER) &
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

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Manufacturing WIP Order Register"
		SMG_WINDOW::NHELP = "WP_MAIN_REGHEADER"
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
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "C      Close"

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
				IF WP_REGHEADER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_REGHEADER = ERR
			CONTINUE 770
		END WHEN

		WP_REGHEADER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			WP_MAIN_REGHEADER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_REGHEADER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_SUBACCOUNT.CH%
		WHEN ERROR IN
			RESET #SB_SUBACCOUNT.CH%
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!***************************************************************
	! Display the background
	!***************************************************************
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

	!***************************************************************
	! Enter/Display/Default
	!***************************************************************
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
	!	The ^*Job Number\* field enters a number which
	!	will reference a particular job.
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.b
	!	^*NOTE:\* ^*Do not duplicate this number with account or customer numbers.\*
	!	.lm -5
	!
	! Index:
	!	.x Job Number
	!	.x Number>Job
	!
	!--
			WP_REGHEADER::JOB = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;30",TEMP$, WP_REGHEADER::JOB, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	of the Job Number entered in field (01).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Job Maintenance Screen
	!	.x Job Maintenance Screen>Description
	!
	!--
			WP_REGHEADER::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;30",	TEMP$, WP_REGHEADER::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Job Type\*
	!	.b
	!	.lm +5
	!	The ^*Job Type\* field enters a two character code
	!	which will identify a particular job type.
	!	.lm -5
	!
	! Index:
	!	.x Job Type
	!
	!--
			WP_REGHEADER::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, WP_REGHEADER::TTYPE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Job Class\*
	!	.b
	!	.lm +5
	!	The ^*Job Class\* field enters the classification
	!	for each job.
	!	.lm -5
	!
	! Index:
	!	.x Job Class
	!
	!--
			WP_REGHEADER::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, WP_REGHEADER::CLASS, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Open Date\*
	!	.b
	!	.lm +5
	!	The ^*Open Date\* field enters the date in which the job began.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Open Date
	!
	!--
			WP_REGHEADER::BDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, WP_REGHEADER::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Current Status\*
	!	.b
	!	.lm +5
	!	The ^*Current Status\* field will indicate if the job is "Active"
	!	or "Closed".
	!	.b
	!	Valid codes for this field are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	A = Active
	!	.le
	!	C = Closed
	!	.els
	!	.lm -5
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field
	!	will provide a list of valid status codes.
	!	.lm -5
	!
	! Index:
	!	.x Current Status
	!	.x Status>Current
	!
	!--
			WP_REGHEADER::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				WP_REGHEADER::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Close Date\*
	!	.b
	!	.lm +5
	!	The ^*Close Date\* field enters the date in which the
	!	job ends.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Close Date
	!
	!--
			WP_REGHEADER::EDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, WP_REGHEADER::EDATE, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field
	!	enters a user defined code to identify
	!	a company location.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Location>Number
	!
	!--
			WP_REGHEADER::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, &
				WP_REGHEADER::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					WP_REGHEADER::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO ReEnter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Reference Number\*
	!	.b
	!	.lm +5
	!	The ^*Reference Number\* field will be used to enter
	!	the order source.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			WP_REGHEADER::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;30", TEMP$, &
				WP_REGHEADER::REFNO, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters
	!	the name of the operator entering the order.
	!	.lm -5
	!
	! Index:
	!	.x Operator
	!
	!--
			WP_REGHEADER::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, &
				WP_REGHEADER::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Batch\*
	!	.b
	!	.lm +5
	!	The Batch field is to be entered with the batch number of this
	!	transaction.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--
			WP_REGHEADER::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;30",TEMP$, WP_REGHEADER::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Post Time\*
	!	.b
	!	.lm +5
	!	The Post Time field enters what time
	!	this particular product will be posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Time
	!
	!--
			WP_REGHEADER::POST_TIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;30",TEMP$, WP_REGHEADER::POST_TIME, MFLAG, &
				"'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Post Date\*
	!	.b
	!	.lm +5
	!	The Post Date field enters when
	!	this particular product will be posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Date
	!
	!--
			WP_REGHEADER::POST_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;30",TEMP$, WP_REGHEADER::POST_DATE, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		WP_MAIN_REGHEADER = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						WP_REGHEADER::SUBJECT + &
						WP_REGHEADER::JOB, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				WP_MAIN_REGHEADER = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

	!
	! Set WP_REGHEADER_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REGHEADER_OLD = WP_REGHEADER

	!
	! Restore WP_REGHEADER_OLD value
	!
	CASE OPT_RESETOLD
		WP_REGHEADER = WP_REGHEADER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REGHEADER_DEF = WP_REGHEADER

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REGHEADER = WP_REGHEADER_DEF
		WP_REGHEADER::SUBJECT = DEF_SUBJECT$

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
			MVALUE = WP_REGHEADER::JOB + " " + &
				LEFT(WP_REGHEADER::DESCR, 33%) + " " + &
				WP_REGHEADER::TTYPE + " " + &
				WP_REGHEADER::CLASS + " " + &
				PRNT_DATE(WP_REGHEADER::BDATE, 8%) + " " + &
				WP_REGHEADER::SSTATUS + " " + &
				PRNT_DATE(WP_REGHEADER::EDATE, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_REGHEADER::SUBJECT + &
				WP_REGHEADER::JOB, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE WP_REGHEADER::SUBJECT + &
				WP_REGHEADER::TTYPE + &
				WP_REGHEADER::JOB, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE WP_REGHEADER::SUBJECT + &
				WP_REGHEADER::CLASS + &
				WP_REGHEADER::JOB, REGARDLESS
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
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			SMG_WINDOW::CURREC = 0% &
				IF WP_REGHEADER::SUBJECT = DEF_SUBJECT$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More menu option hidden in Mast.
	!++
	! Abstract:LINES
	!	^*Lines\*
	!	.B
	!	.LM +5
	!	The ^*Lines\* option accesses
	!	the attached lines for this job.
	!	.LM -5
	!
	! Index:
	!	.x Lines
	!
	!--
