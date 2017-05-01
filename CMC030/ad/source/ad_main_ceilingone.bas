1	%TITLE "One Dimensional Ceiling Table"
	%SBTTL "AD_MAIN_CEILINGONE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CEILINGONE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the One Dimensional Ceiling
	!	table.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CEILINGONE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CEILINGONE
	!	$ DELETE AD_MAIN_CEILINGONE.OBJ;*
	!
	! Author:
	!
	!	09/07/88 - Frank Starman
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update source to v3.6 standards.
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
	!	08/22/97 - Kevin Handy
	!		Change 'val(' to 'val%('
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.HB"
	MAP (AD_CEILINGONE)	AD_CEILINGONE_CDD	AD_CEILINGONE
	MAP (AD_CEILINGONE_OLD)	AD_CEILINGONE_CDD	AD_CEILINGONE_OLD, &
		AD_CEILINGONE2

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING

	COM (CH_AD_CEILING) &
		AD_CEILING.CH%

	COM (CH_AD_CEILINGONE) &
		AD_CEILINGONE.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_AD_CEILINGONE) RARRAY_RECORD RARRAY(100%)! Allocate for 100

	!
	! External functions
	!

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "One Dimensional Ceiling Table"
		SMG_WINDOW::NHELP = "AD_MAIN_CEILINGONE"
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
		GOTO 750 IF AD_CEILINGONE.CH% > 0%

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.CRE"

750		SMG_WINDOW::CHAN  = AD_CEILINGONE.CH%

		WHEN ERROR IN
			RESET #AD_CEILINGONE.CH%
			GET #AD_CEILINGONE.CH%, REGARDLESS
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
			"  Yr    CeilAmount", &
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
			DEP_YEAR = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				VAL%(EDIT$(AD_CEILINGONE::DEP_YEAR, -1%)), &
				MFLAG, "<0>#", MVALUE)

			AD_CEILINGONE::DEP_YEAR = FORMAT$(DEP_YEAR, "<0>#")

		CASE 2%

			AD_CEILINGONE::CEILING = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";13", TEMP$, &
				AD_CEILINGONE::CEILING, MFLAG, &
				"##,###", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AD_MAIN_CEILINGONE = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_CEILINGONE::DEP_YEAR = ""
			THEN
				AD_MAIN_CEILINGONE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AD_CEILINGONE.CH%, &
							KEY #0% EQ AD_CEILINGONE::OPTTABLE + &
							AD_CEILINGONE::EFFDATE + &
							AD_CEILINGONE::DEP_YEAR, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_CEILINGONE = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END IF

			END IF

		END SELECT

	! Set AD_CEILINGONE_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CEILINGONE_OLD = AD_CEILINGONE

	!
	! Restore AD_CEILINGONE_OLD value
	!
	CASE OPT_RESETOLD
		AD_CEILINGONE = AD_CEILINGONE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CEILINGONE2 = AD_CEILINGONE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CEILINGONE = AD_CEILINGONE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!

		AD_CEILINGONE::OPTTABLE		= AD_CEILING::CEILTABLE
		AD_CEILINGONE::EFFDATE		= AD_CEILING::EFFDATE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_CEILINGONE.CH%, &
				KEY #0% GE AD_CEILINGONE::OPTTABLE + &
				AD_CEILINGONE::EFFDATE, &
				REGARDLESS
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
					KEY #0% GE AD_CEILING::CEILTABLE + &
					AD_CEILING::EFFDATE, &
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


			IF AD_CEILINGONE::OPTTABLE = AD_CEILING::CEILTABLE AND &
				AD_CEILINGONE::EFFDATE = AD_CEILING::EFFDATE
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
			RARRAY(MFLAG)::LINRFA	= GETRFA(SMG_WINDOW::CHAN)

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
			AD_CEILINGONE::OPTTABLE = MID(MVALUE, 2%, 6%)
			AD_CEILINGONE::EFFDATE	= MID(MVALUE, 8%, 8%)

		!
		! Print descriptions in journal window.
		!
		CASE 7%
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
