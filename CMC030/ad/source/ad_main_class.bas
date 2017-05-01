1	%TITLE "Asset Class Description"
	%SBTTL "AD_MAIN_CLASS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CLASS(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The Asset Class Description\* table contains an abbreviated IRS class
	!	description table. This table helps determine the asset classification in
	!	order to establish the recovery period.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Classification
	!	.x Classification>Asset
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CLASS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CLASS
	!	$ DELETE AD_MAIN_CLASS.OBJ;*
	!
	! Author:
	!
	!	08/12/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to new standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_Exit
	!
	!	10/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CLASS.HB"
	MAP (AD_CLASS)		AD_CLASS_CDD	AD_CLASS
	MAP (AD_CLASS_OLD)	AD_CLASS_CDD	AD_CLASS_OLD, AD_CLASS2

	MAP (CH_AD_MAIN_CLASS) AD_CLASS.CH%

	COM (CH_AD_CLASS_DEV) STRING AD_CLASS.DEV = 64%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Asset Class Description"
		SMG_WINDOW::NHELP = "AD_MAIN_CLASS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 5%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Class"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Description"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_CLASS.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_CLASS.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open AD_CLASS file " + NUM1$(ERR), 0%)
				AD_MAIN_CLASS = 1%
				CONTINUE 27000
			END WHEN

		END IF

710		SMG_WINDOW::CHAN  = AD_CLASS.CH%

	WHEN ERROR IN
		RESET #AD_CLASS.CH%
		GET #AD_CLASS.CH%, REGARDLESS
	USE
		CONTINUE 27000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	04,05, "(01) Asset Class", &
			05,05, "(02) Description", &
			06,05, "(03) Class Life", &
			07,05, "(04) General Rec Periods", &
			08,05, "(05) Alternative Rec Periods", &
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
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = MVALUE + " noTes"

	!
	! Direction
	!
	CASE OPT_MOREMENU

		!
		! Display notes
		!
		ST% = LIBR_EXTRACTVAR( &
			TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
			LAST_NOTE$, &
			TRM$(AD_CLASS::ASSCLASS))

		LAST_NOTE$ = ENTR_NOTE(SMG_WINDOW::WNUMBER, &
			"8;1", TEMP$, &
			LAST_NOTE$, 64%, "11;78", MVALUE)

		ST% = LIBR_INSERTTEXT( &
			TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
			LAST_NOTE$, &
			TRM$(AD_CLASS::ASSCLASS))


	!
	! Enter/Display/Default
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
	!	^*(01) Asset Class\*
	!	.b
	!	.lm +5
	!	The ^*Asset Class\* field contains the category in which the asset falls
	!	for depreciation. How long the asset is depreciated depends on this class
	!	distinction.
	!	.lm -5
	!
	! Index:
	!	.x Asset Class>Asset Class Description
	!	.x Asset Class Description>Asset Class
	!
	!--


			AD_CLASS::ASSCLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;37", TEMP$, &
				AD_CLASS::ASSCLASS, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description of the
	!	asset class entered in field (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Asset Classification
	!	.x Asset Classification>Description
	!
	!--


			AD_CLASS::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;37", TEMP$, AD_CLASS::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Class Life\*
	!	.b
	!	.lm +5
	!	The ^*Class Life\* field contains the life of the assigned class and provides
	!	the length of depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Class Life>Asset Class Depreciation
	!	.x Asset Class Depreciation>Class Life
	!
	!--


			AD_CLASS::LIFE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;37", TEMP$, AD_CLASS::LIFE * 0.1, &
				MFLAG, "##.#", MVALUE) * 10%

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) General Recovery Periods\*
	!	.b
	!	.lm +5
	!	The ^*General Recovery Periods\* field enters the recovery
	!	periods that would normally be used in selecting the time period for which
	!	the asset will be depreciated.
	!	.lm -5
	!
	! Index:
	!	.x General Recovery Periods
	!
	!--

			AD_CLASS::GDS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;37", TEMP$, AD_CLASS::GDS * 0.1, &
				MFLAG, "##.#", MVALUE) * 10%

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Alternative Recovery Periods\*
	!	.b
	!	.lm +5
	!	The ^*Alternate Recovery Periods\* field enters the
	!	recovery which would be used if the alternate method of depreciation for
	!	tax was being calculated.
	!	.lm -5
	!
	! Index:
	!	.x Alternative Recovery Periods
	!
	!--

			AD_CLASS::ADS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;37", TEMP$, AD_CLASS::ADS * 0.1, &
				MFLAG, "##.#", MVALUE) * 10%

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_CLASS = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_CLASS::ASSCLASS = ""
			THEN
				AD_MAIN_CLASS = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_CLASS::ASSCLASS + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_CLASS = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Need to remove under old key, and insert under
		! (possibly) new key
		!
		CASE "Change", "Blank", "Initialize"

		IF AD_CLASS::ASSCLASS <> AD_CLASS_OLD::ASSCLASS
		THEN
			ST% = LIBR_EXTRACTVAR( &
				TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
				LAST_NOTE$, &
				TRM$(AD_CLASS::ASSCLASS))

			ST% = LIBR_DELETE( &
				TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
				TRM$(AD_CLASS_OLD::ASSCLASS))

			ST% = LIBR_INSERTTEXT( &
				TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
				LAST_NOTE$, &
				TRM$(AD_CLASS::ASSCLASS))

		END IF

		!
		! Need to remove text
		!
		CASE "Erase"

			ST% = LIBR_DELETE( &
				TRM$(AD_CLASS.DEV$) + "AD_CLASS", &
				TRM$(AD_CLASS::ASSCLASS))

		END SELECT


	!
	! Set AD_CLASS_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CLASS_OLD = AD_CLASS

	!
	! Restore AD_CLASS_OLD value
	!
	CASE OPT_RESETOLD
		AD_CLASS = AD_CLASS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CLASS2 = AD_CLASS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CLASS = AD_CLASS2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  AssetClass Description                   " + &
				"           Life GRecPer ARecPer"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,054,059,067"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_CLASS::ASSCLASS + "     " + &
				AD_CLASS::DESCRIPTION + " " + &
				FORMAT$(AD_CLASS::LIFE * 0.1, "##.#") + " " + &
				FORMAT$(AD_CLASS::GDS * 0.1, "   ##.#") + " " + &
				FORMAT$(AD_CLASS::ADS * 0.1, "   ##.#")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_CLASS::ASSCLASS + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_CLASS::DESCRIPTION + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
