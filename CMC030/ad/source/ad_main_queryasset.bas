1	%TITLE "Asset Depreciation Query"
	%SBTTL "AD_MAIN_QUERYASSET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_QUERYASSET(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Asset Depreciation Query\* program scans Asset depreciation query.
	!	.lm -5
	!
	! Index:
	!	.x Asset Depreciation Query
	!	.x Query>Asset Depreciation
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_QUERYASSET/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_QUERYASSET
	!	$ DELETE AD_MAIN_QUERYASSET.OBJ;*
	!
	! Author:
	!
	!	09/15/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/25/92 - Kevin Handy
	!		Changed "EXTERNALREAL" to "EXTERNAL REAL" to remove
	!		syntax errors from someones undocumented and
	!		untested changes.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer fro #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION	AD_OUTP_FIXHISTORY
	EXTERNAL LONG FUNCTION	AD_OUTP_DEPRECIATION


	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)		AD_35ASSET_CDD	AD_35ASSET
	MAP (AD_35ASSET_OLD)	AD_35ASSET_CDD	AD_35ASSET_OLD, AD_35ASSET2
	MAP (AD_35ASSET_SCAN)	AD_35ASSET_CDD	AD_35ASSET_SCAN

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_35ASSET) &
		AD_35ASSET.CH%, &
		AD_35ASSET.READONLY%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION
	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Examine Asset Depreciation"
		SMG_WINDOW::NHELP = "AD_MAIN_QUERYASSET"
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 13%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Asset_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "asset_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Service_date"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 7%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "Location"
			SMG_WINDOW::KFIELD(3%, 0%) = 3%
			SMG_WINDOW::KFIELD(3%, 1%) = 4%
			SMG_WINDOW::KFIELD(3%, 2%) = 5%
			SMG_WINDOW::KFIELD(3%, 3%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		!
		! Load in the defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AD_35ASSET.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_35ASSET.READONLY%
			GOTO 790
		END IF

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
		USE
			AD_MAIN_QUERYASSET = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_35ASSET.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_35ASSET.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_35ASSET.CH%
		WHEN ERROR IN
			RESET #AD_35ASSET.CH%
			GET #AD_35ASSET.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!***********************************************************************
	! More options
	!***********************************************************************

	CASE OPT_OPTLIST
		MVALUE = "Find Next Restore Help eXit Dep_history proJected_dep "

	!**********************************************************************
	! Handle additional options
	!**********************************************************************

	CASE OPT_MOREMENU

		AD_35ASSET_SCAN = AD_35ASSET
		SELECT EDIT$(MVALUE, -1%)

		!
		! Asset Depreciation
		!
		!CASE "DEPRECIATION"
		!	AD_MAIN_QUERYASSET = &
		!		MAIN_JOURNAL(AD_MAIN_DEPRECIATION.ID, "")
		!
		! Retired Assets
		!
		!CASE "RETIRED"
		!	AD_MAIN_QUERYASSET = &
		!		MAIN_WINDOW(AD_MAIN_RETIRED.ID, "")

		!
		! Depreciation History
		!
		CASE "DEP_HISTORY"
	!++
	! Abstract:DEP_HISTORY
	!	^*Depreciation History\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation History\* option shows what was
	!	depreciated every period in the past for the desired objects. The
	!	^*Depreciation History\* report contains the following fields:
	!	.table 3,25
	!	.te
	!	Class
	!	.te
	!	Description
	!	.te
	!	Property Type
	!	.te
	!	Method
	!	.te
	!	Optional Table
	!	.te
	!	Recovery Period
	!	.te
	!	First Year Convention
	!	.te
	!	Disposal Year Convention
	!	.te
	!	Ceiling Table
	!	.te
	!	Salvage Flag
	!	.te
	!	Bonus Flag
	!	.te
	!	Investment Tax Credit Flag
	!	.te
	!	Depreciated Object
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Depreciated History
	!
	!--

			AD_MAIN_QUERYASSET = AD_OUTP_FIXHISTORY(AD_35ASSET)
		!
		! Projected Depreciation
		!
		CASE "PROJECTED_DEP"
	!++
	! Abstract:PROJECTED_DEP
	!	^*Projected Depreciation\*
	!	.b
	!	.lm +5
	!	The ^*Projected Depreciation\* option displays how the
	!	desired object will depreciate in the future, how it will depreciate from
	!	service date to retire date, and the disposal date. This report contains
	!	the following fields:
	!	.table 3,25
	!	.te
	!	Class
	!	.te
	!	Description
	!	.te
	!	Property Type
	!	.te
	!	Method
	!	.te
	!	Optional Table
	!	.te
	!	Recovery Period
	!	.te
	!	First Year Convention
	!	.te
	!	Disposal Year Convention
	!	.te
	!	Ceiling Table
	!	.te
	!	Salvage Flag
	!	.te
	!	Bonus Flag
	!	.te
	!	Investment Tax Credit Flag
	!	.end table
	!
	! Index:
	!	.x Projected Depreciation
	!
	!--
			AD_MAIN_QUERYASSET = AD_OUTP_DEPRECIATION(AD_35ASSET)

		END SELECT

		AD_35ASSET = AD_35ASSET_SCAN

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,05, "Asset #", &
			03,05, "Description", &
			05,05, "Asset Type", &
			06,05, "Location #", &
			07,05, "Department #", &
			08,05, "Serial #", &
			09,05, "Service Date", &
			10,05, "Cost", &
			11,05, "Salvage", &
			12,05, "Section 179", &
			13,05, "ITC Amount", &
			14,05, "ITC Reduce", &
			15,05, "Life in Units", &
			16,05, "Retired Date", &
			17,05, "Proceeds", &
			18,05, "Notes", &
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


20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			AD_35ASSET::ASSET_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;25", TEMP$, &
				AD_35ASSET::ASSET_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 2%

			AD_35ASSET::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, &
				AD_35ASSET::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		CASE 3%

			AD_35ASSET::ASSET_TYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;25", TEMP$, &
				AD_35ASSET::ASSET_TYPE, MFLAG, "'E", &
				MVALUE)

		CASE 4%

			AD_35ASSET::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;25", TEMP$, &
				AD_35ASSET::LOCATION, MFLAG, "'E", &
				MVALUE)

		CASE 5%

			AD_35ASSET::DEPT_NUM= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;25", TEMP$, &
				AD_35ASSET::DEPT_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 6%

			AD_35ASSET::SERIAL_NUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;25", TEMP$, &
				AD_35ASSET::SERIAL_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 7%

			AD_35ASSET::SERVDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;25", TEMP$, &
				AD_35ASSET::SERVDATE, MFLAG, "'E", &
				MVALUE)

		CASE 8%

			AD_35ASSET::COST = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;25",TEMP$, AD_35ASSET::COST, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 9%

			AD_35ASSET::SALVAGE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25",TEMP$, AD_35ASSET::SALVAGE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 10%

			AD_35ASSET::BONUS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;25",TEMP$, AD_35ASSET::BONUS, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 11%

			AD_35ASSET::ITC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;25",TEMP$, AD_35ASSET::ITC, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 12%
			AD_35ASSET::ITCREDUCE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;25",TEMP$, AD_35ASSET::ITCREDUCE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 13%
			AD_35ASSET::UNITS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;25",TEMP$, AD_35ASSET::UNITS, MFLAG, &
				"##,###,###.#", MVALUE)

		CASE 14%

			AD_35ASSET::RET_DATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;25", TEMP$, &
				AD_35ASSET::RET_DATE, MFLAG, "'E", &
				MVALUE)

		CASE 15%

			AD_35ASSET::PROCEEDS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;25",TEMP$, AD_35ASSET::PROCEEDS, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 16%

			AD_35ASSET::NOTES = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;25", TEMP$,	AD_35ASSET::NOTES, MFLAG, &
				"'E", MVALUE)
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Asset #    Description          " + &
			"Tp Loc  Dept   Serial#            " + &
			"  ServDate       Cost    Salvage      Bonus        ITC"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,034,037,042,049,070,079,090,101,112"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_35ASSET::ASSET_NUM + " " + &
				LEFT(AD_35ASSET::DESCRIPTION, 20%) + " " + &
				AD_35ASSET::ASSET_TYPE + " " + &
				AD_35ASSET::LOCATION + " " + &
				AD_35ASSET::DEPT_NUM + " " + &
				AD_35ASSET::SERIAL_NUM + " " + &
				PRNT_DATE(AD_35ASSET::SERVDATE, 6%) + " " + &
				FORMAT$(AD_35ASSET::COST, "#######.##") + " " + &
				FORMAT$(AD_35ASSET::SALVAGE, "#######.##") + " " + &
				FORMAT$(AD_35ASSET::BONUS, "#######.##") + " " + &
				FORMAT$(AD_35ASSET::ITC, "#######.##")

		END SELECT

	!***********************************************************************
	! Find
	!***********************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_35ASSET::ASSET_NUM + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_35ASSET::ASSET_TYPE + &
					AD_35ASSET::ASSET_NUM, &
				REGARDLESS
		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE AD_35ASSET::SERVDATE + &
					AD_35ASSET::ASSET_NUM, &
				REGARDLESS

		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE AD_35ASSET::LOCATION + &
					AD_35ASSET::DEPT_NUM + &
					AD_35ASSET::ASSET_NUM, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
