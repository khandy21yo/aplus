1	%TITLE "Two Dimensional Ceiling Table"
	%SBTTL "AD_MAIN_CEILINGTWO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CEILINGTWO(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the Two dimensional ceiling
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
	!	$ BAS AD_SOURCE:AD_MAIN_CEILINGTWO/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CEILINGTWO
	!	$ DELETE AD_MAIN_CEILINGTWO.OBJ;*
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
	!		Update source code to V3.6 standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit to scope::scope_exit
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Keivn Handy
	!		Use integer for #key
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.HB"
	MAP (AD_CEILINGTWO)	AD_CEILINGTWO_CDD	AD_CEILINGTWO
	MAP (AD_CEILINGTWO_OLD)	AD_CEILINGTWO_CDD	AD_CEILINGTWO_OLD, &
		AD_CEILINGTWO2

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING

	COM (CH_AD_CEILINGTWO) &
		AD_CEILINGTWO.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_AD_CEILINGTWO) RARRAY_RECORD RARRAY(100%)	! Allocate for 100

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Two Dimensional Ceiling Table"
		SMG_WINDOW::NHELP = "AD_MAIN_CEILINGTWO"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 13%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::NITEMS= 13%
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
		GOTO 750 IF AD_CEILINGTWO.CH% > 0%

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGTWO.CRE"

750		SMG_WINDOW::CHAN  = AD_CEILINGTWO.CH%

		WHEN ERROR IN
			RESET #AD_CEILINGTWO.CH%
			GET #AD_CEILINGTWO.CH%, REGARDLESS
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
			"  (01)  (02)  (03)  (04)  (05)  (06)  (07)  (0" + &
			"8)  (09)  (10)  (11)  (12)  (13)", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Yr      01    02    03    04    05    06    " + &
			"07    08    09    10    11    12", &
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
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			"                                              " + &
			"               ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 12%
			A% = VAL%(MID("007,013,019,025,031,037,043," + &
				"049,055,061,067,073", I% * 4% - 3%, 3%))

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
				VAL%(EDIT$(AD_CEILINGTWO::DEP_YEAR, -1%)), &
				MFLAG, "<0>#", MVALUE)

			AD_CEILINGTWO::DEP_YEAR = FORMAT$(DEP_YEAR, "<0>#")

		CASE 2%

			AD_CEILINGTWO::CEILING(0%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";08", &
				TEMP$,AD_CEILINGTWO::CEILING(0%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 3%

			AD_CEILINGTWO::CEILING(1%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", &
				TEMP$,AD_CEILINGTWO::CEILING(1%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 4%

			AD_CEILINGTWO::CEILING(2%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";20", &
				TEMP$,AD_CEILINGTWO::CEILING(2%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 5%

			AD_CEILINGTWO::CEILING(3%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";26", &
				TEMP$,AD_CEILINGTWO::CEILING(3%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 6%

			AD_CEILINGTWO::CEILING(4%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";32", &
				TEMP$,AD_CEILINGTWO::CEILING(4%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 7%

			AD_CEILINGTWO::CEILING(5%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";38", &
				TEMP$,AD_CEILINGTWO::CEILING(5%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 8%

			AD_CEILINGTWO::CEILING(6%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";44", &
				TEMP$,AD_CEILINGTWO::CEILING(6%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 9%

			AD_CEILINGTWO::CEILING(7%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";50", &
				TEMP$,AD_CEILINGTWO::CEILING(7%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 10%

			AD_CEILINGTWO::CEILING(8%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";56", &
				TEMP$,AD_CEILINGTWO::CEILING(8%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 11%

			AD_CEILINGTWO::CEILING(9%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";62", &
				TEMP$,AD_CEILINGTWO::CEILING(9%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 12%

			AD_CEILINGTWO::CEILING(10%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";68", &
				TEMP$,AD_CEILINGTWO::CEILING(10%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		CASE 13%

			AD_CEILINGTWO::CEILING(11%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";74", &
				TEMP$,AD_CEILINGTWO::CEILING(11%) * 0.01, &
				MFLAG, "#####", MVALUE) * 100%

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AD_MAIN_CEILINGTWO = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_CEILINGTWO::DEP_YEAR = ""
			THEN
				AD_MAIN_CEILINGTWO = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AD_CEILINGTWO.CH%, &
							KEY #0% EQ AD_CEILINGTWO::OPTTABLE + &
							AD_CEILINGTWO::EFFDATE + &
							AD_CEILINGTWO::DEP_YEAR, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_CEILINGTWO = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END IF

			END IF

		END SELECT

	! Set AD_CEILINGTWO_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CEILINGTWO_OLD = AD_CEILINGTWO

	!
	! Restore AD_CEILINGTWO_OLD value
	!
	CASE OPT_RESETOLD
		AD_CEILINGTWO = AD_CEILINGTWO_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CEILINGTWO2 = AD_CEILINGTWO

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CEILINGTWO = AD_CEILINGTWO2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!

		AD_CEILINGTWO::OPTTABLE  = AD_CEILING::CEILTABLE
		AD_CEILINGTWO::EFFDATE   = AD_CEILING::EFFDATE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_CEILINGTWO.CH%, &
				KEY #0% GE AD_CEILINGTWO::OPTTABLE + &
				AD_CEILINGTWO::EFFDATE, &
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

			IF AD_CEILINGTWO::OPTTABLE = AD_CEILING::CEILTABLE AND &
				AD_CEILINGTWO::EFFDATE = AD_CEILING::EFFDATE
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
			AD_CEILINGTWO::OPTTABLE  = MID(MVALUE, 2%, 6%)
			AD_CEILINGTWO::EFFDATE   = MID(MVALUE, 8%, 8%)
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
