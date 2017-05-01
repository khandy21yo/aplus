1	%TITLE "Asset Depreciation Scan"
	%SBTTL "AD_MAIN_DEPRECIATIONSCAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_DEPRECIATIONSCAN(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION,LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	This program maintains the Asset depreciation file.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_DEPRECIATIONSCAN/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_DEPRECIATIONSCAN
	!	$ DELETE AD_MAIN_DEPRECIATIONSCAN.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Frank F. Starman
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
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_exit
	!
	!	10/02/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Change ASCII("X") to A"X"B
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
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION
	MAP (AD_DEPRECIATION_OLD) AD_DEPRECIATION_CDD	AD_DEPRECIATION_OLD, &
		AD_DEPRECIATION2

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD		AD_DEPCLASS

	MAP (CH_AD_DEPRECIATION) AD_DEPRECIATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION	FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Asset Depreciation "
		SMG_WINDOW::CURREC= - 2%
		SMG_WINDOW::NHELP = "AD_MAIN_DEPRECIATIONSCAN"
		SMG_WINDOW::HSIZE = 73%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 3%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Asset_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Object"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_DEPRECIATION.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open AD_DEPRECIATION file " + NUM1$(ERR), 0%)
				AD_MAIN_DEPRECIATIONSCAN = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_DEPRECIATION.CH%

		WHEN ERROR IN
			RESET #AD_DEPRECIATION.CH%
			GET #AD_DEPRECIATION.CH%, REGARDLESS
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


		DATA	04,05, "(01) Asset #", &
			05,05, "(02) Object", &
			06,05, "(03) Dep Class", &
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
	!	^*(01) Asset Number\*
	!	.b
	!	.lm +5
	!	The ^*Asset Number\* field enters an
	!	asset number unique to the asset to which the number is
	!	assigned. Duplicates are not allowed.
	!	.b
	!	An entry in this field is required. This field may contain
	!	up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset Number>Record
	!	.x Record>Asset Number
	!
	!--

			AD_DEPRECIATION::ASSET_NUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;25", TEMP$, &
				AD_DEPRECIATION::ASSET_NUM, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AD_MAIN_ASSET.ID, "V0  ") = 1%)
				THEN
					AD_DEPRECIATION::ASSET_NUM = &
						AD_35ASSET::ASSET_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field enables the user to print a report
	!	including a selected object.
	!	.lm -5
	!
	! Index:
	!	.x Object>Record
	!	.x Record>Object
	!
	!--

			AD_DEPRECIATION::DEP_OBJECT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;25", TEMP$, &
				AD_DEPRECIATION::DEP_OBJECT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
				AD_DEPRECIATION::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Depreciation Class\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Class\* field contains a summary of
	!	information on how to depreciate the desired object in
	!	a particular manner.
	!	.lm -5
	!
	! Index:
	!	.x Deprecation Class>Record
	!	.x Record>Depreciation Class
	!
	!--

			AD_DEPRECIATION::DEPCLASS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;25", TEMP$, &
				AD_DEPRECIATION::DEPCLASS, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AD_MAIN_DEPCLASS.ID, "V0  ") = 1%)
				THEN
					AD_DEPRECIATION::DEPCLASS = &
						AD_DEPCLASS::DEPCLASS
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_DEPRECIATIONSCAN = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_DEPRECIATIONSCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_DEPRECIATION::ASSET_NUM, &
				AD_35ASSET::DESCRIPTION, &
				"AD", MLOOP, "ASSET", &
				"Asset number", AD_MAIN_ASSET.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_35ASSET::DESCRIPTION, 4%, 36%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_DEPRECIATIONSCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_DEPRECIATION::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AD", MLOOP, "OBJ", &
				"Object", AD_MAIN_OBJECT.ID)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ AD_DEPRECIATION::ASSET_NUM + &
						AD_DEPRECIATION::DEP_OBJECT, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_DEPRECIATIONSCAN = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 0%)
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			AD_MAIN_DEPRECIATIONSCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_DEPRECIATION::DEPCLASS, &
				AD_DEPCLASS::DESCRIPTION, &
				"AD", MLOOP, "DEP", &
				"Dep Class", AD_MAIN_DEPCLASS.ID)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DESCRIPTION$ = STRING$(40%, A"?"B)
			DESCRIPTION$ = AD_35ASSET::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, "Q0" + &
				AD_DEPRECIATION::ASSET_NUM) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DESCRIPTION$, 4%, 36%, , SMG$M_BOLD)
		END IF

	!
	! Set AD_DEPRECIATION_OLD value
	!
20500	CASE OPT_SETOLD
		AD_DEPRECIATION_OLD = AD_DEPRECIATION

	!
	! Restore AD_DEPRECIATION_OLD value
	!
	CASE OPT_RESETOLD
		AD_DEPRECIATION = AD_DEPRECIATION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_DEPRECIATION2 = AD_DEPRECIATION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_DEPRECIATION = AD_DEPRECIATION2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Asset #    Object DepClass"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_DEPRECIATION::ASSET_NUM + " " + &
				AD_DEPRECIATION::DEP_OBJECT + "      " + &
				AD_DEPRECIATION::DEPCLASS

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_DEPRECIATION::ASSET_NUM + &
				AD_DEPRECIATION::DEP_OBJECT, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_DEPRECIATION::DEP_OBJECT + &
				AD_DEPRECIATION::ASSET_NUM, &
				REGARDLESS

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
