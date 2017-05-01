1	%TITLE "Asset Depreciation"
	%SBTTL "AD_MAIN_DEPRECIATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_DEPRECIATION(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_DEPRECIATION/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_DEPRECIATION
	!	$ DELETE AD_MAIN_DEPRECIATION.OBJ;*
	!
	! Author:
	!
	!	03/14/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	12/01/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_exit
	!
	!	10/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use A'x'B instead of ASCII('x')
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
	MAP (AD_DEPRECIATION)		AD_DEPRECIATION_CDD	AD_DEPRECIATION
	MAP (AD_DEPRECIATION_OLD)	AD_DEPRECIATION_CDD	AD_DEPRECIATION_OLD, AD_DEPRECIATION2

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD	AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD	AD_DEPCLASS

	MAP (CH_AD_DEPRECIATION) AD_DEPRECIATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Asset Depreciation"
		SMG_WINDOW::CURREC= -2%
		SMG_WINDOW::NHELP = "AD_MAIN_DEPRECIATION"
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 5%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 14%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Object"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 14%
		SMG_WINDOW::HVIEW = 77%
		SMG_WINDOW::VVIEW = 5%

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
				AD_MAIN_DEPRECIATION = 1%
				CONTINUE 28000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_DEPRECIATION.CH%

		WHEN ERROR IN
			RESET #AD_DEPRECIATION.CH%
			GET #AD_DEPRECIATION.CH%, REGARDLESS
		USE
			CONTINUE 28000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,05, "(01) Object", &
			03,05, "(02) Dep Class", &
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
	!	^*(01) Object\*
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

			AD_DEPRECIATION::DEP_OBJECT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;25", TEMP$, &
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

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Depreciation Class\*
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

			AD_DEPRECIATION::DEPCLASS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25", TEMP$, &
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
		AD_MAIN_DEPRECIATION = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_DEPRECIATION = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_DEPRECIATION::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AD", MLOOP, "OBJ", &
				"Dep Object", AD_MAIN_OBJECT.ID)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ AD_DEPRECIATION::ASSET_NUM + &
						AD_DEPRECIATION::DEP_OBJECT, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_DEPRECIATION = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_DEPRECIATION = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_DEPRECIATION::DEPCLASS, &
				AD_DEPCLASS::DESCRIPTION, &
				"AD", MLOOP, "DEPCLAS", &
				"Dep Class", AD_MAIN_DEPCLASS.ID)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "Q0" + &
				AD_DEPRECIATION::DEP_OBJECT) <> 1%
			THEN
				AD_OBJECT::DESCRIPTION = &
					STRING$(LEN(AD_OBJECT::DESCRIPTION), &
					A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_OBJECT::DESCRIPTION, 2%, 36%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(AD_MAIN_DEPCLASS.ID, "Q0" + &
				AD_DEPRECIATION::DEPCLASS) <> 1%
			THEN
				AD_DEPCLASS::DESCRIPTION = &
					STRING$(LEN(AD_DEPCLASS::DESCRIPTION), &
					A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_DEPCLASS::DESCRIPTION, 3%, 36%, , SMG$M_BOLD)
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
		AD_DEPRECIATION::ASSET_NUM = MVALUE

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Object DepClass"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_DEPRECIATION::DEP_OBJECT + "      " + &
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

		END SELECT

	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE + &
						AD_DEPRECIATION::DEP_OBJECT, &
						REGARDLESS

				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF AD_DEPRECIATION::ASSET_NUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			AD_DEPRECIATION::ASSET_NUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:RECORD
	!	^*Record\*
	!	.b
	!	.lm +5
	!	The ^*Record\* option provides a faster way to depreciate
	!	assets by entering them directly through the depreciation file.
	!	.lm -5
	!
	! Index:
	!	.x Record
	!
	!--
