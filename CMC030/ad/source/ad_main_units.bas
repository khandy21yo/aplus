1	%TITLE "Actual Units Production"
	%SBTTL "AD_MAIN_UNITS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_UNITS(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	^*The Actual Units\* enters the number
	!	of units which have been used for an asset on a particular date.
	!	.lm -5
	!
	! Index:
	!	.x Units
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_UNITS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_UNITS
	!	$ DELETE AD_MAIN_UNITS.OBJ;*
	!
	! Author:
	!
	!	12/11/87 - Frank F. Starman
	!
	! Modification history:
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
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integera for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.HB"
	MAP (AD_UNITS)		AD_UNITS_CDD		AD_UNITS
	MAP (AD_UNITS_OLD)	AD_UNITS_CDD		AD_UNITS_OLD, &
							AD_UNITS2, AD_UNITS3

	%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.HB"
	MAP (AD_JOURNAL)	AD_JOURNAL_CDD		AD_JOURNAL

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	COM (CH_AD_JOURNAL) &
		BATCH_NO$ = 2%, &
		AD_JOURNAL.CH%

	COM (CH_AD_UNITS) &
		AD_UNITS.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_AD_UNITS) RARRAY_RECORD RARRAY(1000%)	! Allocate for 1000

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Actual Production Units"
		SMG_WINDOW::NHELP = "AD_MAIN_UNITS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Get info required for main file
		!
		GOTO 750 IF AD_UNITS.CH% > 0%

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.CRE"

750		SMG_WINDOW::CHAN  = AD_UNITS.CH%

		WHEN ERROR IN
			RESET #AD_UNITS.CH%
			GET #AD_UNITS.CH%, REGARDLESS
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
			"  (01)              (02)                      " + &
			"                                ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Asset#        Quantity                      " + &
			"                                ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			"                                              " + &
			"              ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 2%
			A% = VAL%(MID("013,025", I% * 4% - 3%, 3%))

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
	!	^*(01) Asset _#\*
	!	.b
	!	.lm +5
	!	The ^*Asset Number\* field enters an
	!	asset number from the Asset Master.
	!	.b
	!	An entry in this field is required. This field may
	!	contain up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Number
	!	.x Number>Asset
	!
	!--

			AD_UNITS::ASSET_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AD_UNITS::ASSET_NUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, "V0") = 1%
				THEN
					AD_UNITS::ASSET_NUM = &
						AD_35ASSET::ASSET_NUM
				END IF
				GOTO Reentry
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field enters the number of
	!	units which have been used on the particular date of this
	!	particular asset.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--


			AD_UNITS::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";15",TEMP$, 1.0 * AD_UNITS::QUANTITY, &
				MFLAG, "#,###,###", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AD_MAIN_UNITS = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_UNITS = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_UNITS::ASSET_NUM, &
				AD_35ASSET::DESCRIPTION, &
				"AR", MLOOP, "ASSET", &
				"Asset number", AD_MAIN_ASSET.ID)

		END SELECT

	!
	! Set AD_UNITS_OLD value
	!
20500	CASE OPT_SETOLD
		AD_UNITS_OLD = AD_UNITS

	!
	! Restore AD_UNITS_OLD value
	!
	CASE OPT_RESETOLD
		AD_UNITS = AD_UNITS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_UNITS2 = AD_UNITS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_UNITS = AD_UNITS2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		AD_UNITS::DEP_OBJECT  = AD_JOURNAL::DEP_OBJECT
		AD_UNITS::ACTION_DATE= AD_JOURNAL::ACTION_DATE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_UNITS.CH%, &
				KEY #0% GE AD_UNITS::DEP_OBJECT + &
					AD_UNITS::ACTION_DATE, &
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
					KEY #0% GE AD_JOURNAL::DEP_OBJECT + &
					AD_JOURNAL::ACTION_DATE, &
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

			IF AD_UNITS::DEP_OBJECT = AD_JOURNAL::DEP_OBJECT AND &
				AD_UNITS::ACTION_DATE = AD_JOURNAL::ACTION_DATE
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
			AD_UNITS::DEP_OBJECT  = MID(MVALUE, 2%, 1%)
			AD_UNITS::ACTION_DATE = RIGHT(MVALUE, 3%)
		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
