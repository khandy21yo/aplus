1	%TITLE "Equipment Ledger Master File"
	%SBTTL "EL_MAIN_EQUIPMENT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG EL_MAIN_EQUIPMENT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	All equipment descriptions including each new piece of
	!	equipment is entered and maintained through the
	!	^*Equipment Master File\*.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_MAIN_EQUIPMENT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN EL_MAIN_EQUIPMENT
	!	$ DELETE EL_MAIN_EQUIPMENT.OBJ;*
	!
	! Author:
	!
	!	10/14/92 - Dan Perkins
	!
	! Modification history:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/13/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/26/94 - Frank F. Starman
	!		Replace EL type and category with PD type and category
	!
	!	12/11/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/30/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[EL.OPEN]EL_EQUIPMENT.HB"
	MAP (SB_SUBACCOUNT)	EL_EQUIPMENT_CDD	EL_EQUIPMENT
	MAP (EL_EQUIPMENT_OLD)	EL_EQUIPMENT_CDD	EL_EQUIPMENT_OLD
	MAP (EL_EQUIPMENT_DEF)	EL_EQUIPMENT_CDD	EL_EQUIPMENT_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)		PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.HB"
	MAP (PD_CATEGORY)		PD_CATEGORY_CDD		PD_CATEGORY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_EL_EQUIPMENT) &
		SB_SUBACCOUNT.CH%, &
		EL_EQUIPMENT.READONLY%

	COM (TT_EL_EQUIPMENT) &
		STITLE$ = 30%, &
		SSTAT$(5%) = 30%

	!
	! Default Subject
	!
	DEF_SUBJECT$ = "E"

	!
	! External functions
	!
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

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Equipment Ledger Maintenance"
		SMG_WINDOW::NHELP = "EL_MAIN_EQUIPMENT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 10%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Eqnum"
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
				IF EL_EQUIPMENT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EL_MAIN_EQUIPMENT = ERR
			CONTINUE 770
		END WHEN

		EL_EQUIPMENT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			EL_MAIN_EQUIPMENT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		EL_EQUIPMENT.READONLY% = -1%

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


		DATA	03,05, "(01) Equip Number", &
			04,05, "(02) Description", &
			05,05, "(03) Eq Type", &
			06,05, "(04) Eq Category", &
			07,05, "(05) Open Date", &
			08,05, "(06) Current Status", &
			09,05, "(07) Close Date", &
			10,05, "(08) Location", &
			11,05, "(09) Ref #", &
			12,05, "(10) Operator", &
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
	!	^*(01) Equipment Number\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Number\* field enters a number which
	!	will reference a particular piece of equipment.
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.b
	!	^*Note:  Do not duplicate this number with account or customer
	!	numbers.\*
	!	.lm -5
	!
	! Index:
	!
	!--
			EL_EQUIPMENT::EQNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;30",TEMP$, EL_EQUIPMENT::EQNUM, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	of the Equipment Number entered in field (01).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			EL_EQUIPMENT::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;30",	TEMP$, EL_EQUIPMENT::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Equipment Type\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Type\* field enters the two character
	!	code which will identify this Equipment type.  Pressing ^*List Choices\*
	!	while the cursor is located at this field will provide a
	!	list of valid Equipment type choices.
	!	.lm -5
	!
	! Index:
	!	.x Equipment Type
	!
	!--
			EL_EQUIPMENT::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, EL_EQUIPMENT::TTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "V0") = 1%
				THEN
					EL_EQUIPMENT::TTYPE = PD_PRODTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "M")
				EL_EQUIPMENT::TTYPE = PD_PRODTYPE::CODE
				GOTO ReEnter
			END IF


		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Equipment Category\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Category\* field enters the classification
	!	for each piece of equipment.
	!	.b
	!	Pressing ^*List Choices\* will display a list of valid
	!	equipment class codes.
	!	.lm -5
	!
	! Index:
	!
	!--
			EL_EQUIPMENT::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, EL_EQUIPMENT::CLASS, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "V0") = 1%
				THEN
					EL_EQUIPMENT::CLASS = PD_CATEGORY::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "M")
				EL_EQUIPMENT::CLASS = PD_CATEGORY::CODE
				GOTO ReEnter
			END IF


		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Open Date\*
	!	.b
	!	.lm +5
	!	The ^*Open Date\* field is provided to enter the date the
	!	Equipment began.
	!	.b
	!	The format for entry is MMDDYY or MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Open Date
	!
	!--
			EL_EQUIPMENT::BDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, EL_EQUIPMENT::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Current Status\*
	!	.b
	!	.lm +5
	!	The ^*Current Status\* field will indicate if the Equipment
	!	is "Active" or "Closed".
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.TE
	!	^*I\* - Inactive
	!	.te
	!	^*C\* - Closed
	!	.end table
	!	This field requires an entry.  Pressing ^*List Choices\*
	!	will display a list of valid status codes.
	!	.lm -5
	!
	! Index:
	!	.x Current Status
	!	.x Status>Current
	!
	!--
			EL_EQUIPMENT::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				EL_EQUIPMENT::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Close Date\*
	!	.b
	!	.lm +5
	!	The ^*Close Date\* field enters the date
	!	the Equipment ends.
	!	.b
	!	The format for entry is MMDDYY or MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Close Date
	!
	!--
			EL_EQUIPMENT::EDATE = ENTR_3DATE(SCOPE,  SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, EL_EQUIPMENT::EDATE, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field is provided to enter a
	!	location code pertaining to equipment in this record.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			EL_EQUIPMENT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, &
				EL_EQUIPMENT::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					EL_EQUIPMENT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				EL_EQUIPMENT::LOCATION = UTL_LOCATION::LOCATION
				GOTO ReEnter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Reference _#\*
	!	.B
	!	.LM +5
	!	The ^*Reference _#\* field contains the number which posting
	!	in the General Ledger will be completed under.  An example of a
	!	Reference number is the invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			EL_EQUIPMENT::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;30", TEMP$, &
				EL_EQUIPMENT::REFNO, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field is provided to enter the code which
	!	will identify the person responsible for a particular entry.
	!	.b
	!	The field will accommodate an entry of ten (10) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			EL_EQUIPMENT::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, &
				EL_EQUIPMENT::OPERATOR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		EL_MAIN_EQUIPMENT = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						EL_EQUIPMENT::SUBJECT + &
						EL_EQUIPMENT::EQNUM, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				EL_MAIN_EQUIPMENT = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		CASE 3%
			!
			! Display the description for type
			!
			EL_MAIN_EQUIPMENT = FUNC_TESTENTRY(SMG_WINDOW, &
				EL_EQUIPMENT::TTYPE, &
				PD_PRODTYPE::DESCRIPTION, &
				"EL", MLOOP, "PROG", &
				"Type", PD_MAIN_PRODTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 5%, 40%, , SMG$M_BOLD)

		CASE 4%
			!
			! Display the description for class
			!
			EL_MAIN_EQUIPMENT = FUNC_TESTENTRY(SMG_WINDOW, &
				EL_EQUIPMENT::CLASS, &
				PD_CATEGORY::DESCRIPTION, &
				"EL", MLOOP, "PROG", &
				"Class", PD_MAIN_CATEGORY.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_CATEGORY::DESCRIPTION, 6%, 40%, , SMG$M_BOLD)

		CASE 8%
			!
			! Display the descriptions for location name
			!
			EL_MAIN_EQUIPMENT = FUNC_TESTENTRY(SMG_WINDOW, &
				EL_EQUIPMENT::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"EL", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 10%, 40%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			!
			! Display the description for type
			!
			PD_PRODTYPE::DESCRIPTION = &
				STRING$(LEN(PD_PRODTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
					EL_EQUIPMENT::TTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 5%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display the description for class
			!
			PD_CATEGORY::DESCRIPTION = &
				STRING$(LEN(PD_CATEGORY::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "Q0" + &
					EL_EQUIPMENT::CLASS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_CATEGORY::DESCRIPTION, 6%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"Q0" + EL_EQUIPMENT::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 10%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set EL_EQUIPMENT_OLD value
	!
20500	CASE OPT_SETOLD
		EL_EQUIPMENT_OLD = EL_EQUIPMENT

	!
	! Restore EL_EQUIPMENT_OLD value
	!
	CASE OPT_RESETOLD
		EL_EQUIPMENT = EL_EQUIPMENT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		EL_EQUIPMENT_DEF = EL_EQUIPMENT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		EL_EQUIPMENT = EL_EQUIPMENT_DEF
		EL_EQUIPMENT::SUBJECT = DEF_SUBJECT$

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Eq_Number  Description            " + &
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
			MVALUE = EL_EQUIPMENT::EQNUM + " " + &
				LEFT(EL_EQUIPMENT::DESCR, 33%) + " " + &
				EL_EQUIPMENT::TTYPE + " " + &
				EL_EQUIPMENT::CLASS + " " + &
				PRNT_DATE(EL_EQUIPMENT::BDATE, 8%) + " " + &
				EL_EQUIPMENT::SSTATUS + " " + &
				PRNT_DATE(EL_EQUIPMENT::EDATE, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE EL_EQUIPMENT::SUBJECT + &
				EL_EQUIPMENT::EQNUM, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE EL_EQUIPMENT::SUBJECT + &
				EL_EQUIPMENT::TTYPE + &
				EL_EQUIPMENT::EQNUM, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE EL_EQUIPMENT::SUBJECT + &
				EL_EQUIPMENT::CLASS + &
				EL_EQUIPMENT::EQNUM, REGARDLESS

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
						KEY #0% GE EL_EQUIPMENT::SUBJECT + &
						EL_EQUIPMENT::EQNUM, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE EL_EQUIPMENT::SUBJECT + &
						EL_EQUIPMENT::TTYPE + &
						EL_EQUIPMENT::EQNUM, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 2%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #2% GE EL_EQUIPMENT::SUBJECT + &
						EL_EQUIPMENT::CLASS + &
						EL_EQUIPMENT::EQNUM, REGARDLESS
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
				IF EL_EQUIPMENT::SUBJECT = DEF_SUBJECT$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
