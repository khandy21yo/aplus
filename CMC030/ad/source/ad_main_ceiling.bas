1	%TITLE "Ceiling Tables"
	%SBTTL "AD_MAIN_CEILING"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CEILING(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Ceiling Tables\*, used mostly for luxury cars,
	!	sets a maximum depreciation for each year. This limit usually causes the
	!	depreciation to last longer.
	!	.lm -5
	!
	! Index:
	!	.x Ceiling>Tables
	!	.x Tables>Ceiling
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CEILING/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CEILING
	!	$ DELETE AD_MAIN_CEILING.OBJ;*
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
	!		Update for V3.6 source code standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer in #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING
	MAP (AD_CEILING_1)	AD_CEILING_CDD	AD_CEILING_OLD, AD_CEILING2

	!
	! This common area must be mapped in both the main program and
	! in AD_MAIN_CEILING.
	!
	COM (CH_AD_CEILING) &
		AD_CEILING.CH%

	COM (TABLE_AD_CEILING) &
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
		SMG_WINDOW::DESCR = "Cost Recovery Ceiling Tables"
		SMG_WINDOW::NHELP = "AD_MAIN_CEILING"
		SMG_WINDOW::CHAN  = AD_CEILING.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%)  = 2%


		TABLETITLE$ = "Code  Description"
		OPTTAB$(0%) = "2"
		OPTTAB$(1%) = "1    One Dimensional Ceiling Table"
		OPTTAB$(2%) = "2    Two Dimensional Ceiling Table"

20010		GOTO 20040 IF AD_CEILING.CH% > 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.CRE"

20040		SMG_WINDOW::CHAN  = AD_CEILING.CH%

		WHEN ERROR IN
			RESET #AD_CEILING.CH%
			GET #AD_CEILING.CH%, REGARDLESS
			UNLOCK #AD_CEILING.CH%
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

			DATA	02, 05, "(01) Table Code", &
				03, 05, "(02) Effective Date", &
				04, 05, "(03) Table Dimension", &
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
		MVALUE = MVALUE + " Table "

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
	!	The ^*Table\* function accesses the Two
	!	Dimensional Percentage Tables which are used for real property.
	!	.b
	!	A Two Dimensional Table refers to the percentage of the adjusted basis for
	!	the user, but also for a month based on the service date.
	!	.lm -5
	!
	! Index:
	!
	!--

			SELECT AD_CEILING::DIMEN

			CASE "1"
				AD_MAIN_CEILING = &
				MAIN_JOURNAL(AD_MAIN_CEILINGONE.ID, "")

			CASE "2"
				AD_MAIN_CEILING = &
				MAIN_JOURNAL(AD_MAIN_CEILINGTWO.ID, "")

			END SELECT

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
			IF AD_CEILING_OLD::CEILTABLE + &
				AD_CEILING_OLD::EFFDATE <> &
				AD_CEILING::CEILTABLE + AD_CEILING::EFFDATE
			THEN
				TEMP$ = AD_CEILING::CEILTABLE + &
					AD_CEILING::EFFDATE + ""
				AD_CEILING = AD_CEILING_OLD

				SELECT AD_CEILING::DIMEN
				CASE "1"
					AD_MAIN_CEILING = &
						MAIN_JOURNAL(AD_MAIN_CEILINGONE.ID, "C" + TEMP$)
				CASE "2"
					AD_MAIN_CEILING = &
						MAIN_JOURNAL(AD_MAIN_CEILINGTWO.ID, "C" + TEMP$)
				END SELECT
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AD_MAIN_CEILING = MAIN_JOURNAL(AD_MAIN_CEILINGONE.ID, &
				"E") &

			AD_MAIN_CEILING = MAIN_JOURNAL(AD_MAIN_CEILINGTWO.ID, &
				"E") &


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
	!	The ^*Table Code\* field contains the code for the ceiling table which
	!	identifies which table is being used.
	!	.lm -5
	!
	! Index:
	!	.x Table Code>Cost Recovery Ceiling Table
	!	.x Cost Recovery Ceiling Table>Table Code
	!
	!--

			AD_CEILING::CEILTABLE  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;31", TEMP$, &
				AD_CEILING::CEILTABLE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field contains the date the table is
	!	effective for use.
	!	.lm -5
	!
	! Index:
	!	.x Effective Date>Cost Recovery Ceiling Table
	!	.x Cost Recovery Ceiling Table>Effective Date
	!
	!--

			AD_CEILING::EFFDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;31", TEMP$, &
				AD_CEILING::EFFDATE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Table Dimension\*
	!	.b
	!	.lm +5
	!	The ^*Table Dimension\* field enters the number of
	!	dimensions the table will contain. Valid choices are:
	!	.b
	!	.lm 10
	!	.list 0,"*"
	!	.le
	!	1 - Same percentage for every year no matter when depreciation was started.
	!	.le
	!	2 - Assigns different percentage to each year depending on the starting date
	!	of the depreciation.
	!	.els
	!	Valid entries may be viewed by pressing ^*List Choices\*.
	!
	! Index:
	!	.x Table Dimension>Cost Recovery Ceiling Tables
	!	.x Cost Recovery Ceiling Tables>Table Dimension
	!
	!--

			AD_CEILING::DIMEN = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;31", TEMP$, AD_CEILING::DIMEN, &
				MFLAG, "'E", MVALUE, OPTTAB$(), &
				TABLETITLE$, "005")

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		AD_MAIN_CEILING = 0%

		SELECT MLOOP

		CASE 1%

			IF AD_CEILING::CEILTABLE = ""
			THEN
				AD_MAIN_CEILING = 1%
			END IF

		CASE 2%

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #AD_CEILING.CH%, &
						KEY #0% EQ AD_CEILING::CEILTABLE + &
						AD_CEILING::EFFDATE, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_CEILING = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)

			END IF

		END SELECT

20500	CASE OPT_SETOLD
		AD_CEILING_OLD = AD_CEILING

	CASE OPT_RESETOLD
		AD_CEILING = AD_CEILING_OLD

	CASE OPT_SETDEFAULT
		AD_CEILING2 = AD_CEILING

	CASE OPT_RESETDEFAULT
		AD_CEILING = AD_CEILING2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  Code   EffDate    "

		CASE 2%

			MVALUE = "009,020"

		CASE 3%

			MVALUE = AD_CEILING::CEILTABLE + " " + &
				PRNT_DATE(AD_CEILING::EFFDATE, 8%)

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AD_CEILING.CH%, &
				KEY #0% GE AD_CEILING::CEILTABLE + &
				AD_CEILING::EFFDATE, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
