1	%TITLE "Units of Production Journal"
	%SBTTL "AD_MAIN_JOURNAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_JOURNAL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Journal Entry\* option enters
	!	and maintains Depreciation Units. Each Journal File is assigned
	!	a user batch number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Journal>Entry Maintain
	!	.x Maintain>Journal Entry
	!	.x Maintain>Depreciation Units
	!	.x Journal>Entry Maintain
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_JOURNAL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_JOURNAL
	!	$ DELETE AD_MAIN_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	12/10/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/13/89 - Kevin Handy
	!		Modified DATE_DAYCODE to return long.
	!		Modified DATE_DAYOFWEEK to enter long.
	!
	!	04/20/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Reformat to V3.6 coding standards
	!		Defined AD_MAIN_UNITS in AD_WINDOW.INC.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!		Lose extra '&' on line before a 'END IF' line.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.HB"
	MAP (AD_JOURNAL)	AD_JOURNAL_CDD		AD_JOURNAL
	MAP (AD_JOURNAL_1)	AD_JOURNAL_CDD		AD_JOURNAL_OLD, &
							AD_JOURNAL2

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	!
	! This common area must be mapped in both the main program and
	! in AD_MAIN_JOURNAL.
	!
	COM (CH_AD_JOURNAL) &
		BATCH_NO$ = 2%, &
		AD_JOURNAL.CH%

	COM (CH_AD_UNITS) &
		AD_UNITS.CH%

	COM (CH_AD_UNITS2) &
		AD_UNITS2.CH%

	COM (TT_RM_JOURNAL) &
		DAY_WEEK$(7%) = 9%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AD_FUNC_COPYBATCH
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Units Depreciation Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "AD_MAIN_JOURNAL"
		SMG_WINDOW::CHAN  = AD_JOURNAL.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Object"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		DAY_WEEK$(1%) = "Monday   "
		DAY_WEEK$(2%) = "Tuesday  "
		DAY_WEEK$(3%) = "Wednesday"
		DAY_WEEK$(4%) = "Thursday "
		DAY_WEEK$(5%) = "Friday   "
		DAY_WEEK$(6%) = "Saturday "
		DAY_WEEK$(7%) = "Sunday   "

20010		GOTO 20040 IF AD_JOURNAL.CH% > 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

		%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.CRE"

20040		SMG_WINDOW::CHAN  = AD_JOURNAL.CH%

		WHEN ERROR IN
			RESET #AD_JOURNAL.CH%
			GET #AD_JOURNAL.CH%, REGARDLESS
			UNLOCK #AD_JOURNAL.CH%
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

			DATA	1,  1, "(01) Object", &
				2,  1, "(02) Date", &
				4,  1, "(03) Stationman", &
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
		MVALUE = MVALUE + " Units copY "

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Units transaction entry
		!
		CASE "UNITS"
	!++
	! Abstract:UNITS
	!	^*Units\*
	!	.b
	!	.lm +5
	!	The ^*Units\* field
	!	enters how many units for each object has been used.
	!	.lm -5
	!
	! Index:
	!	.x Units
	!
	!--
			AD_MAIN_JOURNAL = MAIN_JOURNAL(AD_MAIN_UNITS.ID, "")

		!
		! Copy batch template
		!
		CASE "COPY"
	!++
	! Abstract:COPY
	!	^*Copy\*
	!	.b
	!	.lm +5
	!	The ^*Copy\* makes a copy of one
	!	selected batch number and object.
	!	.b
	!	This option can be used to create a template (list of all assets using the
	!	units of production method) under any batch number.
	!	.b
	!	The ^*Copy\* option will create a worksheet for the assets and only quantities
	!	of production will need to be input.
	!	.b
	!	After ^*Copy\* has been selected, the system will prompt for a Journal Batch
	!	Number. That Journal Batch Number will be used to copy from, consequently it
	!	must be a valid Batch Number. If it is not valid, an error message stating
	!	that the "File does not exist", will appear.
	!	.b
	!	When the system prompts for an Object Codes, the Wildcarding technique may
	!	be used.
	!	.lm -5
	!
	! Index:
	!	.x Copy
	!
	!--
			AD_MAIN_JOURNAL = AD_FUNC_COPYBATCH

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
			IF AD_JOURNAL_OLD::DEP_OBJECT + &
				AD_JOURNAL_OLD::ACTION_DATE <> &
				AD_JOURNAL::DEP_OBJECT + AD_JOURNAL::ACTION_DATE
			THEN
				TEMP$ = AD_JOURNAL::DEP_OBJECT + &
					AD_JOURNAL::ACTION_DATE + ""
				AD_JOURNAL = AD_JOURNAL_OLD

				AD_MAIN_JOURNAL = &
					MAIN_JOURNAL(AD_MAIN_UNITS.ID, "C" + TEMP$)
			END IF
		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AD_MAIN_JOURNAL = MAIN_JOURNAL(AD_MAIN_UNITS.ID, "E") &


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
	!	^*(01) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field enters the object code
	!	from the Object Description File.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate one (01) alphanumeric character.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located
	!	at this field will display a list of valid codes.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!
	!--

			AD_JOURNAL::DEP_OBJECT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;20", TEMP$, &
				AD_JOURNAL::DEP_OBJECT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
					AD_JOURNAL::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date the Units
	!	were produced.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Journal Entry
	!	.x Journal Entry>Date
	!
	!--

			AD_JOURNAL::ACTION_DATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;20", TEMP$, &
				AD_JOURNAL::ACTION_DATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Stationman\*
	!	.b
	!	.lm +5
	!	The ^*Stationman\* field enters the name or
	!	other information to identify the person responsible for
	!	a transaction.
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Stationman
	!
	!--

			AD_JOURNAL::STATIONMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;20", TEMP$, &
				AD_JOURNAL::STATIONMAN, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		AD_MAIN_JOURNAL = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is input defined?
			!
			AD_MAIN_JOURNAL = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_JOURNAL::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AR", MLOOP, "OBJ", &
				"Object", AD_MAIN_OBJECT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_OBJECT::DESCRIPTION, 1%, 32%, , SMG$M_BOLD)

		CASE 2%
			IF AD_JOURNAL::ACTION_DATE = ""
			THEN
				AD_MAIN_JOURNAL = 1%
			ELSE
				WEEK_DAY$ = &
					DAY_WEEK$(DATE_DAYOFWEEK(DATE_DAYCODE(AD_JOURNAL::ACTION_DATE)))

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					WEEK_DAY$, 2%, 32%, , SMG$M_BOLD)

				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AD_JOURNAL.CH%, &
							KEY #0% EQ AD_JOURNAL::DEP_OBJECT + &
							AD_JOURNAL::ACTION_DATE, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_JOURNAL = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END IF

			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display extra stuff
		!
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DISPLAYNAME$ = STRING$(20%, 63%)
			DISPLAYNAME$ = AD_OBJECT::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, &
				"Q0" + AD_JOURNAL::DEP_OBJECT) = 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DISPLAYNAME$, 1%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			WEEK_DAY$ = STRING$(9%, 63%)
			WEEK_DAY$ = &
				DAY_WEEK$(DATE_DAYOFWEEK(DATE_DAYCODE(AD_JOURNAL::ACTION_DATE))) &
				IF EDIT$(AD_JOURNAL::ACTION_DATE, -1%) <> ""
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WEEK_DAY$, 2%, 32%, , SMG$M_BOLD)
		END IF

20500	CASE OPT_SETOLD
		AD_JOURNAL_OLD = AD_JOURNAL

	CASE OPT_RESETOLD
		AD_JOURNAL = AD_JOURNAL_OLD

	CASE OPT_SETDEFAULT
		AD_JOURNAL2 = AD_JOURNAL

	CASE OPT_RESETDEFAULT
		AD_JOURNAL = AD_JOURNAL2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  Object Date"

		CASE 2%

			MVALUE = "009"

		CASE 3%

			MVALUE = AD_JOURNAL::DEP_OBJECT + "      " + &
				PRNT_DATE(AD_JOURNAL::ACTION_DATE, 8%)

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_JOURNAL.CH%, &
				KEY #0% GE AD_JOURNAL::DEP_OBJECT + &
				AD_JOURNAL::ACTION_DATE, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
