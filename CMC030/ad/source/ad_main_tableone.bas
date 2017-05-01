1	%TITLE "One Dimensional Depreciation Table"
	%SBTTL "AD_MAIN_TABLEONE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_TABLEONE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Depreciation Table\* creates the One Dimensional
	!	Percentage table.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_TABLEONE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_TABLEONE
	!	$ DELETE AD_MAIN_TABLEONE.OBJ;*
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
	!		Update source code to V3.6 standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Use integer in #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.HB"
	MAP (AD_TABLEONE)	AD_TABLEONE_CDD	AD_TABLEONE
	MAP (AD_TABLEONE_OLD)	AD_TABLEONE_CDD	AD_TABLEONE_OLD, &
		AD_TABLEONE2, AD_TABLEONE3

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.HB"
	MAP (AD_TABLE)	AD_TABLE_CDD	AD_TABLE

	COM (CH_AD_TABLE) &
		AD_TABLE.CH%

	COM (CH_AD_TABLEONE) &
		AD_TABLEONE.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA
	END RECORD

	MAP (TT_AD_TABLEONE) RARRAY_RECORD RARRAY(1000%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "One dimensional optional table"
		SMG_WINDOW::NHELP = "AD_MAIN_TABLEONE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 12%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Get info required for main file
		!
		GOTO 750 IF AD_TABLEONE.CH% > 0%

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLEONE.CRE"

750		SMG_WINDOW::CHAN  = AD_TABLEONE.CH%

		WHEN ERROR IN
			RESET #AD_TABLEONE.CH%
			GET #AD_TABLEONE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)        (02)", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Yr    Percentage", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "### "), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 2%
			A% = VAL%(MID("007,019", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Year\*
	!	.b
	!	.lm +5
	!	The ^*(01) Year\* field is used to indicate a
	!	year of depreciation.
	!	.b
	!	The format for entry is two (2) numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Year>Depreciation Table
	!	.x Depreciation Table>Year
	!
	!--

			DEP_YEAR = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				VAL(EDIT$(AD_TABLEONE::DEP_YEAR, -1%)), &
				MFLAG, "<0>#", MVALUE)

			AD_TABLEONE::DEP_YEAR = FORMAT$(DEP_YEAR, "<0>#")

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Percentage\* field indicates the percentage
	!	to be depreciated from the adjusted basis.
	!	.b
	!	The format for entry is XXX.XX.
	!	.lm -5
	!
	! Index:
	!	.x Percentage>Depreciation Table
	!	.x Depreciation Table>Percentage
	!
	!--

			AD_TABLEONE::PERCENTAGE = &
				FUNC_ROUND(ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", TEMP$, &
				AD_TABLEONE::PERCENTAGE * 0.01, MFLAG, &
				"##.##", MVALUE) * 100%, 2%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AD_MAIN_TABLEONE = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_TABLEONE::DEP_YEAR = ""
			THEN
				AD_MAIN_TABLEONE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AD_TABLEONE.CH%, &
							KEY #0% EQ AD_TABLEONE::OPTTABLE + &
							AD_TABLEONE::YEARS + &
							AD_TABLEONE::EFFDATE + &
							AD_TABLEONE::DEP_YEAR, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_TABLEONE = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END IF

			END IF

		END SELECT

	! Set AD_TABLEONE_OLD value
	!
20500	CASE OPT_SETOLD
		AD_TABLEONE_OLD = AD_TABLEONE

	!
	! Restore AD_TABLEONE_OLD value
	!
	CASE OPT_RESETOLD
		AD_TABLEONE = AD_TABLEONE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_TABLEONE2 = AD_TABLEONE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_TABLEONE = AD_TABLEONE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!

		AD_TABLEONE::OPTTABLE	= AD_TABLE::OPTTABLE
		AD_TABLEONE::YEARS	= AD_TABLE::YEARS
		AD_TABLEONE::EFFDATE	= AD_TABLE::EFFDATE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_TABLEONE.CH%, &
				KEY #0% GE AD_TABLEONE::OPTTABLE + &
					AD_TABLEONE::YEARS + &
					AD_TABLEONE::EFFDATE, REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE AD_TABLE::OPTTABLE + &
					AD_TABLE::YEARS + &
					AD_TABLE::EFFDATE, &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF AD_TABLEONE::OPTTABLE = AD_TABLE::OPTTABLE AND &
				AD_TABLEONE::YEARS = AD_TABLE::YEARS AND &
				AD_TABLEONE::EFFDATE = AD_TABLE::EFFDATE
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			AD_TABLEONE::OPTTABLE = MID(MVALUE, 2%, 6%)
			AD_TABLEONE::YEARS    = MID(MVALUE, 8%, 4%)
			AD_TABLEONE::EFFDATE  = RIGHT(MVALUE, 12%)
		!
		! Print descriptions in journal window.
		!
 !		CASE 7%
			! Nop right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
