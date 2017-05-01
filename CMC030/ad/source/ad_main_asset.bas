1	%TITLE "Asset Description"
	%SBTTL "AD_MAIN_ASSET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_ASSET(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Asset Decription\* screen enters a new asset
	!	and elects the depreciation object in the Depreciation Description File.
	!	.b
	!	The following information is to be added relative to each asset:
	!	.B
	!	.LM 15
	!	.list 0,"*"
	!	.le
	!	Asset _#
	!	.le
	!	Description
	!	.le
	!	Asset Type
	!	.le
	!	Location _#
	!	.le
	!	Department _#
	!	.le
	!	Serial _#
	!	.le
	!	Service Date
	!	.le
	!	Cost
	!	.le
	!	Salvage
	!	.le
	!	Section 179
	!	.le
	!	ITC Amount
	!	.le
	!	ITC Reduce
	!	.le
	!	Life in Units
	!	.els
	!	.LM -15
	!
	! Index:
	!	.x Asset>Description File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_ASSET/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_ASSET
	!	$ DELETE AD_MAIN_ASSET.OBJ;*
	!
	! Author:
	!
	!	12/01/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	2/24/91 - Craig Tanner
	!		Upgraded to use AD_35ASSET only, Instead of AD_ASSET
	!		along with AD_RETIRED.
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key option.
	!
	!	04/17/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update to version 3.6 standards
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer in #key
	!
	!	06/15/98 - Kevin Handy
	!		Make sure that the location is set up before
	!		the call to UTL_DEPARTMENT
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose an excessive number of %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/07/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION	FUNC_TESTENTRY

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)		AD_35ASSET_CDD	AD_35ASSET
	MAP (AD_35ASSET_OLD)	AD_35ASSET_CDD	AD_35ASSET_OLD, AD_35ASSET2

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE)		AD_ASSTYPE_CDD	AD_ASSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)		UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT)		UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_35ASSET) &
		AD_35ASSET.CH%, &
		AD_35ASSET.READONLY%

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "Asset Description"
		SMG_WINDOW::NHELP = "AD_MAIN_ASSET"
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 16%

		SMG_WINDOW::NKEYS = 5%
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
		SMG_WINDOW::KNAME(4%) = "Description"
			SMG_WINDOW::KFIELD(4%, 0%) = 1%
			SMG_WINDOW::KFIELD(4%, 1%) = 2%

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

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_ASSET = ERR
			CONTINUE 770
		END WHEN

		AD_35ASSET.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
		USE
			AD_MAIN_ASSET = ERR
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

		DATA	02,05, "(01) Asset #", &
			03,05, "(02) Description", &
			05,05, "(03) Asset Type", &
			06,05, "(04) Location #", &
			07,05, "(05) Department #", &
			08,05, "(06) Serial #", &
			09,05, "(07) Service Date", &
			10,05, "(08) Cost", &
			11,05, "(09) Salvage", &
			12,05, "(10) Section 179", &
			13,05, "(11) ITC Amount", &
			14,05, "(12) ITC Reduce", &
			15,05, "(13) Life in Units", &
			16,05, "(14) Retired Date", &
			17,05, "(15) Proceeds", &
			18,05, "(16) Notes", &
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

	!++
	! Abstract:FLD001
	!	^*(01) Asset _#\*
	!	.b
	!	.lm +5
	!	The ^*Asset Number\* field enters an asset
	!	number unique to the asset to which the number is assigned.
	!	Duplicates are not allowed.
	!	.b
	!	An entry in this field is required.  This field may contain
	!	up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Number
	!	.x Number>Asset
	!
	!--


			AD_35ASSET::ASSET_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;25", TEMP$, &
				AD_35ASSET::ASSET_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	for a specific asset.
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Description
	!
	!--


			AD_35ASSET::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, &
				AD_35ASSET::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Asset Type\*
	!	.b
	!	.lm +5
	!	The ^*Asset Type\* field enters a code established in
	!	the Asset Type Description Table.
	!	.b
	!	The field will contain two (2) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will provide a list of valid Asset Types.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Type
	!	.x Type>Asset
	!
	!--


			AD_35ASSET::ASSET_TYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;25", TEMP$, &
				AD_35ASSET::ASSET_TYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AD_MAIN_ASSTYPE.ID, "V0  ") = 1%)
				THEN
					AD_35ASSET::ASSET_TYPE = &
						AD_ASSTYPE::ASSET_TYPE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(AD_MAIN_ASSTYPE.ID, "M")
				AD_35ASSET::ASSET_TYPE = &
						AD_ASSTYPE::ASSET_TYPE
				GOTO ReEnter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a
	!	^*Location _#\* from the company profile file for a specific asset.
	!	.b
	!	The field will contain up to four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will provide a list of valid locations, including the name
	!	and address information concerning each location.
	!	.lm -5
	!
	! Index:
	!	.x Location>Asset Description
	!
	!--


			AD_35ASSET::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;25", TEMP$, &
				AD_35ASSET::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0  ") = 1%)
				THEN
					AD_35ASSET::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				AD_35ASSET::LOCATION = &
						UTL_LOCATION::LOCATION
				GOTO ReEnter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Department _#\*
	!	.b
	!	.lm +5
	!	The ^*Department _#\* field enters a Department Number
	!	established in the company profile file. The Location Number and
	!	Department Number combination identifies the specific location of an
	!	asset.
	!	.b
	!	The field will accommodate six (6) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will provide a list of valid Department Numbers and also a
	!	description and the name of the supervisor for each specific department.
	!	.lm -5
	!
	! Index:
	!	.x Department>Asset Description
	!	.x Asset Description>Department
	!
	!--


			AD_35ASSET::DEPT_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;25", TEMP$, &
				AD_35ASSET::DEPT_NUM, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				UTL_LOCATION::LOCATION = AD_35ASSET::LOCATION
				UTL_DEPARTMENT::LOCATION = AD_35ASSET::LOCATION
				IF MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, &
					"V0" + AD_35ASSET::LOCATION) = 1%
				THEN
					AD_35ASSET::DEPT_NUM = &
						UTL_DEPARTMENT::DEPT_NUM
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, &
					"M0" + AD_35ASSET::LOCATION)
				AD_35ASSET::DEPT_NUM = &
					UTL_DEPARTMENT::DEPT_NUM

				GOTO ReEnter
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Serial _#\*
	!	.b
	!	.lm +5
	!	The ^*Serial _#\* field enters the Serial number
	!	of a specific asset.
	!	.b
	!	The field will accommodate twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Serial Number
	!	.x Number>Serial
	!
	!--


			AD_35ASSET::SERIAL_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;25", TEMP$, &
				AD_35ASSET::SERIAL_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Service Date\*
	!	.b
	!	.lm +5
	!	The ^*Service Date\* field enters the date an
	!	asset is placed into service.
	!	.b
	!	The format for entry is MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Service Date>Asset Description
	!	.x Asset Description>Service Date
	!
	!--


			AD_35ASSET::SERVDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;25", TEMP$, &
				AD_35ASSET::SERVDATE, MFLAG, "'E", &
				MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Cost\*
	!	.b
	!	.lm +5
	!	The ^*Cost\* field contains the actual purchase price
	!	paid for the asset.
	!	.b
	!	The field will accommodate a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Cost
	!
	!--


			AD_35ASSET::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;25",TEMP$, AD_35ASSET::COST, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Salvage\*
	!	.B
	!	.lm +5
	!	The ^*Salvage\* field enters the estimated salvage
	!	value of the asset at the end of its useful life.
	!	.b
	!	The field may contain an amount as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Salvage
	!	.x Life>Useful
	!	.x Useful Life
	!
	!--


			AD_35ASSET::SALVAGE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25",TEMP$, AD_35ASSET::SALVAGE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Section 179\*
	!	.b
	!	.lm +5
	!	The ^*Section 179\* field enters the additional
	!	first year depreciation allowed on a qualified asset.
	!	.b
	!	The field will contain an amount as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Section 179
	!
	!--


			AD_35ASSET::BONUS = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;25",TEMP$, AD_35ASSET::BONUS, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Investment Tax Credit Amount\*
	!	.b
	!	.lm +5
	!	The ^*Investment Tax Credit Amount\* field enters the
	!	amount of Investment Tax Credit taken on a qualified asset.
	!	.b
	!	The field will accommodate an amount as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Investment Tax Credit Amount
	!	.x Investment Tax Credit
	!
	!--


			AD_35ASSET::ITC = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;25",TEMP$, AD_35ASSET::ITC, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Investment Tax Credit Reduce\*
	!	.b
	!	.lm +5
	!	The ^*Investment Tax Credit Reduce\* field enters the
	!	amount the depreciation basis has been reduced where applicable.
	!	.lm -5
	!
	! Index:
	!	.x Investment Tax Credit Reduce>Asset Master
	!	.x Asset Master>Investment Tax Credit Reduce
	!
	!--

			AD_35ASSET::ITCREDUCE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;25",TEMP$, AD_35ASSET::ITCREDUCE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Life in Units\*
	!	.b
	!	.lm +5
	!	The ^*Life in Units\* field enters the number of units
	!	a machine can produce during its life.  An entry in this field is necessary
	!	when depreciation by units or use is the method practiced.
	!	.lm -5
	!
	! Index:
	!	.x Life in Units>Asset Master
	!	.x Asset Master>Life in Units
	!
	!--

			AD_35ASSET::UNITS = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;25",TEMP$, AD_35ASSET::UNITS, MFLAG, &
				"##,###,###.#", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Retired Date\*
	!	.b
	!	.lm +5
	!	The ^*Retired Date\* field enters the date the asset
	!	was sold or scrapped.
	!	.lm -5
	!
	! Index:
	!	.x Retired Date>Retired
	!	.x Retired>Retired Date
	!
	!--


			AD_35ASSET::RET_DATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;25", TEMP$, &
				AD_35ASSET::RET_DATE, MFLAG, "'E", &
				MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Proceeds\*
	!	.b
	!	.lm +5
	!	The ^*Proceeds\* field enters the revenue gained by
	!	retirement of the asset if it is sold.
	!	.lm -5
	!
	! Index:
	!	.x Proceeds>Retired
	!	.x Retired>Proceeds
	!
	!--


			AD_35ASSET::PROCEEDS = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;25",TEMP$, AD_35ASSET::PROCEEDS, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters important information concerning
	!	the retirement of the asset.
	!	.lm -5
	!
	! Index:
	!	.x Notes>Retired
	!	.x Retired>Notes
	!
	!--


			AD_35ASSET::NOTES = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;25", TEMP$,	AD_35ASSET::NOTES, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!***********************************************************************
	! Test values
	!***********************************************************************
20300	CASE OPT_TESTENTRY
		AD_MAIN_ASSET = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_35ASSET::ASSET_NUM = ""
			THEN
				AD_MAIN_ASSET = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_35ASSET::ASSET_NUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_ASSET = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
				END IF
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			AD_MAIN_ASSET = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_35ASSET::ASSET_TYPE, &
				AD_ASSTYPE::DESCRIPTION, &
				"AD", MLOOP, "PROG", &
				"Asset Type", AD_MAIN_ASSTYPE.ID)

		CASE 4%
			!
			! Is the input defined?
			!
			AD_MAIN_ASSET = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_35ASSET::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"AD", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

		CASE 5%
			!
			! Is the input defined?
			!
			AD_MAIN_ASSET = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_35ASSET::LOCATION + AD_35ASSET::DEPT_NUM, &
				UTL_DEPARTMENT::DESCRIPTION, &
				"AD", MLOOP, "PROG", &
				"Department", UTL_MAIN_DEPARTMENT.ID)

		END SELECT

20500	!******************************************************************
	! Set AD_35ASSET_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		AD_35ASSET_OLD = AD_35ASSET


	!******************************************************************
	! Restore AD_35ASSET_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		AD_35ASSET = AD_35ASSET_OLD


	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		AD_35ASSET2 = AD_35ASSET


	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		AD_35ASSET = AD_35ASSET2

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
				KEY #3% GE AD_35ASSET::LOCATION + &
					AD_35ASSET::DEPT_NUM + &
					AD_35ASSET::ASSET_NUM, &
				REGARDLESS

		CASE 4%
			FIND #SMG_WINDOW::CHAN, &
				KEY #4% GE AD_35ASSET::DESCRIPTION + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:DEPRECIATION
	!	^*Depreciation\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation\* function
	!	selects the different Objects to depreciate this asset.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation>Function
	!
	!--

	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:RETIRED
	!	^*Retired\*
	!	.b
	!	.lm +5
	!	The ^*Retired\* option enters
	!	information concerning the selling or scrapping of the assets.
	!	.lm -5
	!
	! Index:
	!	.x Retired>Function
	!
	!--
