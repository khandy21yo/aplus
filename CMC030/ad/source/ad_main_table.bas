1	%TITLE "Optional Depreciation Table"
	%SBTTL "AD_MAIN_TABLE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_TABLE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
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
	!	The ^*Optional Depreciation Tables\* option
	!	creates a "class life" percentage table, as they pertain to
	!	the ACRS depreciation method.
	!	.lm -5
	!
	! Index:
	!	.x Optional Depreciation>Tables
	!	.x Tables>Optional Depreciation
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_TABLE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_TABLE
	!	$ DELETE AD_MAIN_TABLE.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	08/13/96 - Kevin Handy
	!		Change 'CASE = x' to 'CASE x'.
	!		Reformat source code.
	!		Lose commented out code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.HB"
	MAP (AD_TABLE)	AD_TABLE_CDD	AD_TABLE
	MAP (AD_TABLE2)	AD_TABLE_CDD	AD_TABLE_OLD, AD_TABLE2

	!
	! This common area must be mapped in both the main program and
	! in AD_MAIN_TABLE.
	!
	COM (CH_AD_TABLE) &
		AD_TABLE.CH%

	COM (TABLE_AD_TABLE) &
		TABLETITLE$ = 40%, &
		OPTTAB$(2%) = 40%

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Depreciation Optional Tables"
		SMG_WINDOW::NHELP = "AD_MAIN_TABLE"
		SMG_WINDOW::CHAN  = AD_TABLE.CH%
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%


		TABLETITLE$ = "Dim   Description"
		OPTTAB$(0%) = "2"
		OPTTAB$(1%) = "1    One Dimensional Optional Table"
		OPTTAB$(2%) = "2    Two Dimensional Optional Table"

20010		GOTO 20040 IF AD_TABLE.CH% > 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.CRE"

20040		SMG_WINDOW::CHAN  = AD_TABLE.CH%

		WHEN ERROR IN
			RESET #AD_TABLE.CH%
			GET #AD_TABLE.CH%, REGARDLESS
			UNLOCK #AD_TABLE.CH%
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		!
		! Main screen
		!
		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	02,  05, "(01) Table Code", &
				03,  05, "(02) Recovery Period", &
				04,  05, "(03) Effective Date", &
				05,  05, "(04) Table Dimension", &
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
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Table noteS"

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Table
		!
		CASE "TABLE"
	!++
	! Abstract:TABLE
	!	^*Table\*
	!	.b
	!	.lm +5
	!	The ^*Table\* function accesses the One
	!	Dimensional or Two Dimensional Percentage Tables.
	!	.b
	!	One Dimensional ACRS Tables are used for personal property. Two Dimensional
	!	ACRS Tables are used for real property.
	!	.b
	!	A One Dimensional Tables refers to the percentage of the adjusted basis to
	!	be depreciated for the depreciation year regardless of the service date.
	!	.b
	!	A Two Dimensional Table refers to th percentage of the adjusted basis for the
	!	year, but also for a month based on the service date.
	!	.lm -5
	!
	! Index:
	!	.x Table>Optional Depreciation
	!
	!--

			SELECT AD_TABLE::DIMEN
			CASE "1"
				AD_MAIN_TABLE = &
					MAIN_JOURNAL(AD_MAIN_TABLEONE.ID, "")

			CASE "2"
				AD_MAIN_TABLE = &
					MAIN_JOURNAL(AD_MAIN_TABLETWO.ID, "")

			END SELECT

		!
		! Display notes
		!
		CASE "NOTES"
	!++
	! Abstract:NOTES
	!
	! Index:
	!
	!--
			CALL READ_DEVICE("AD_TABLE",AD_TABLE.DEV$, STAT%)
			ST% = LIBR_EXTRACTVAR( &
				TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
				LAST_NOTE$, &
				TRM$(AD_TABLE::OPTTABLE))

			LAST_NOTE$ = ENTR_NOTE(SMG_WINDOW::WNUMBER, &
				"8;2", TEMP$, &
				LAST_NOTE$, 64%, "10;77", MVALUE)

			ST% = LIBR_INSERTTEXT( &
				TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
				LAST_NOTE$, &
				TRM$(AD_TABLE::OPTTABLE))

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF AD_TABLE_OLD::OPTTABLE + &
				AD_TABLE_OLD::EFFDATE + &
				AD_TABLE_OLD::YEARS <> &
				AD_TABLE::OPTTABLE + AD_TABLE::EFFDATE + &
				AD_TABLE::YEARS
			THEN
				TEMP$ = AD_TABLE::OPTTABLE + &
					AD_TABLE::EFFDATE + &
					AD_TABLE::YEARS + ""
				AD_TABLE = AD_TABLE_OLD

				SELECT AD_TABLE::DIMEN
				CASE "1"
					AD_MAIN_TABLE = &
						MAIN_JOURNAL(AD_MAIN_TABLEONE.ID, "C" + TEMP$)
				CASE "2"
					AD_MAIN_TABLE = &
						MAIN_JOURNAL(AD_MAIN_TABLETWO.ID, "C" + TEMP$)
				END SELECT

				CALL READ_DEVICE("AD_TABLE", &
					AD_TABLE.DEV$, STAT%)
				ST% = LIBR_EXTRACTVAR( &
					TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
					LAST_NOTE$, &
					TRM$(AD_TABLE::OPTTABLE))

				ST% = LIBR_DELETE( &
					TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
					TRM$(AD_TABLE_OLD::OPTTABLE))

				ST% = LIBR_INSERTTEXT( &
					TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
					LAST_NOTE$, &
					TRM$(AD_TABLE::OPTTABLE))

			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AD_MAIN_TABLE = &
				MAIN_JOURNAL(AD_MAIN_TABLEONE.ID, "E")

			AD_MAIN_TABLE = &
				MAIN_JOURNAL(AD_MAIN_TABLETWO.ID, "E")

			CALL READ_DEVICE("AD_TABLE", AD_TABLE.DEV$, STAT%)
			ST% = LIBR_DELETE( &
				TRM$(AD_TABLE.DEV$) + "AD_TABLE", &
				TRM$(AD_TABLE::OPTTABLE))

		END SELECT


	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 Reenter:	SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Table Code\*
	!	.b
	!	.lm +5
	!	The ^*Table Code\* field enters
	!	the code indicating the type of table which will be created.
	!	.b
	!	There are four (4) spaces available for an alphanumeric
	!	entry.
	!	.lm -5
	!
	! Index:
	!	.x Table Code
	!	.x Code>Table
	!	.x Table Code>Asset Type Description
	!
	!--

			AD_TABLE::OPTTABLE  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;31", TEMP$, &
				AD_TABLE::OPTTABLE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Recovery Period\*
	!	.b
	!	.lm +5
	!	The ^*Recovery Period\* enters how long it will take to
	!	depreciate the item fully or to the salvage value.
	!	.lm -5
	!
	! Index:
	!	.x Recovery Period>Optional Depreciation Tables
	!	.x Optional Depreciation Tables>Recovery Period
	!
	!--

			YEARS = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;31", TEMP$, &
				VAL(EDIT$(AD_TABLE::YEARS, -1%)), &
				MFLAG, "<0>#.#", MVALUE)

			AD_TABLE::YEARS = FORMAT$(YEARS, "<0>#.#")

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field contains the date of when the table is
	!	effective for use.
	!	.lm -5
	!
	! Index:
	!	.x Effective Date>Optional Depreciation Tables
	!	.x Optional Depreciation Tables>Effective Date
	!
	!--

			AD_TABLE::EFFDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;31", TEMP$, &
				AD_TABLE::EFFDATE, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Table Dimension\*
	!	.b
	!	.lm +5
	!	The ^*Table Dimension\* field enters the number of
	!	dimensions the table will contain. Valid choices are:
	!	.table 3,25
	!	.te
	!	^*1\* - Same percentage for every year no matter when depreciation was started.
	!	.te
	!	^*2\* - Assigns different percentage to each year depending on the starting date
	!	of the depreciation.
	!	.end table
	!
	! Index:
	!	.x Table Dimension>Optional Depreciation Tables
	!	.x Optional Depreciation Tables>Table Dimension
	!
	!--

			AD_TABLE::DIMEN = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;31", TEMP$, AD_TABLE::DIMEN, &
				MFLAG, "'E", MVALUE, OPTTAB$(), &
				TABLETITLE$, "005")

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		AD_MAIN_TABLE = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_TABLE::OPTTABLE = ""
			THEN
				AD_MAIN_TABLE = 1%
			END IF

		CASE 3%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #AD_TABLE.CH%, &
						KEY #0% EQ AD_TABLE::OPTTABLE+ &
						AD_TABLE::YEARS + &
						AD_TABLE::EFFDATE, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_TABLE = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)

			END IF

		END SELECT

20500	CASE OPT_SETOLD
		AD_TABLE_OLD = AD_TABLE

	CASE OPT_RESETOLD
		AD_TABLE = AD_TABLE_OLD

	CASE OPT_SETDEFAULT
		AD_TABLE2 = AD_TABLE

	CASE OPT_RESETDEFAULT
		AD_TABLE = AD_TABLE2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  Code   EffDate    Years"

		CASE 2%

			MVALUE = "009,020"

		CASE 3%

			MVALUE = AD_TABLE::OPTTABLE + " " + &
				PRNT_DATE(AD_TABLE::EFFDATE, 8%) + " " + &
				AD_TABLE::YEARS

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_TABLE.CH%, KEY #0% GE AD_TABLE::OPTTABLE + &
				AD_TABLE::YEARS + &
				AD_TABLE::EFFDATE, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
