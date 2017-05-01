1	%TITLE "Depreciation Method Description"
	%SBTTL "AD_MAIN_METHOD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_METHOD(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	There are several methods of computing depreciation.  Any
	!	method which results in a reasonable allowance may be
	!	selected for each item of property, but such methods
	!	must thereafter apply consistently to that particular
	!	item.
	!	.b
	!	Some examples of Methods are:
	!	.table 3,25
	!	.te
	!	_* Straight Line
	!	.te
	!	_* Double Declining Balance
	!	.te
	!	_* ACRS
	!	.end table
	!
	! Index:
	!	.x Depreciation Method>Definition
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_METHOD/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_METHOD
	!	$ DELETE AD_MAIN_METHOD.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/27/88 - Frank F. Starman
	!		Add Calculation type 12,13,14
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	05/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_exit
	!
	!	09/25/96 - Kevin Handy
	!		Reformat source code
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
	!
	!	11/08/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD)		AD_METHOD_CDD	AD_METHOD
	MAP (AD_METHOD_OLD)	AD_METHOD_CDD	AD_METHOD_OLD, &
		AD_METHOD2

	MAP (CH_AD_MAIN_METHOD) AD_METHOD.CH%

	MAP (TT_AD_MAIN_METHOD) AD_METHOD.DEV1$ = 64%

	COM (TABLE_IC_TRANSTYPE) &
		CALCTITLE$ = 40%, &
		CALC$(12%) = 40%

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
		SMG_WINDOW::DESCR = "Depreciation Method Description"
		SMG_WINDOW::NHELP = "AD_MAIN_METHOD"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Category
		!
		CALCTITLE$ = "Code  Description"
		CALC$(0%) = "12"
		CALC$(1%) = "01    Optional Depreciation Table"
		CALC$(2%) = "02    Straight Line"
		CALC$(3%) = "03    125% Declining Balance"
		CALC$(4%) = "04    150% Declining Balance"
		CALC$(5%) = "05    175% Declining Balance"
		CALC$(6%) = "06    200% Declining Balance"
		CALC$(7%) = "07    125% Declining to Straight Line"
		CALC$(8%) = "08    150% Declining to Straight Line"
		CALC$(9%) = "09    175% Declining to Straight Line"
		CALC$(10%) = "10    200% Declining to Straight Line"
		CALC$(11%) = "11    Sum of the Years-Digits"
		CALC$(12%) = "12    Units-of-Production"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_METHOD.CH% <= 0%)
		THEN
			!
			! Get info required for main file
			!
			CALL READ_DEVICE("AD_METHOD", AD_METHOD.DEV1$, STAT%)
			AD_METHOD.DEV1$ = "SYS$LOGIN:" &
				IF AD_METHOD.DEV1$ = ""

			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, "Unable to open AD_METHOD file " + NUM1$(ERR), 0%)
				AD_MAIN_METHOD = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_METHOD.CH%

		WHEN ERROR IN
			RESET #AD_METHOD.CH%
			GET #AD_METHOD.CH%, REGARDLESS
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


		DATA	04,10, "(01) Method Code", &
			05,10, "(02) Description", &
			06,10, "(03) Method Type", &
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
			TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
			LAST_NOTE$, &
			TRM$(AD_METHOD::DEP_METHOD))

		LAST_NOTE$ = ENTR_NOTE(SMG_WINDOW::WNUMBER, &
			"8;1", TEMP$, &
			LAST_NOTE$, 64%, "11;78", MVALUE)

		ST% = LIBR_INSERTTEXT( &
			TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
			LAST_NOTE$, &
			TRM$(AD_METHOD::DEP_METHOD))


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
	!	^*(01) Method Code\*
	!	.b
	!	.lm +5
	!	The Depreciation Method Code field enters a
	!	code for the particular depreciation method.  Examples
	!	may be:
	!	.table 3,25
	!	.te
	!	_* DDSL = Double Declining to Straight Line
	!	.te
	!	_* SL   = Straight Line
	!	.end table
	!	The field may contain up to four (04) alphanumeric characters.
	!
	! Index:
	!	.x Depreciation>Method Code
	!	.x Method Code>Depreciation
	!
	!--

			AD_METHOD::DEP_METHOD = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;32", TEMP$, AD_METHOD::DEP_METHOD, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	This field contains the description of the depreciation
	!	method entered in field (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Method Code>Description
	!	.x Description>Method Code
	!
	!--

			AD_METHOD::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;32", TEMP$, AD_METHOD::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Method Type\*
	!	.b
	!	.lm +5
	!	The ^*Method Type\* field enters a calculation code for
	!	the depreciation table. The table follows:
	!	.table 3,25
	!	.te
	!	^*01\* Optional Depreciation Table
	!	.tE
	!	^*02\* Straight Line
	!	.tE
	!	^*03\* 125% Declining Balance
	!	.tE
	!	^*04\* 150% Declining Balance
	!	.tE
	!	^*05\* 175% Declining Balance
	!	.tE
	!	^*06\* 200% Declining Balance
	!	.tE
	!	^*07\* 125% Declining to Straight Line
	!	.tE
	!	^*08\* 150% Declining to Straight Line
	!	.tE
	!	^*09\* 175% Declining to Straight Line
	!	.tE
	!	^*10\* 200% Declining to Straight Line
	!	.tE
	!	^*11\* Sum of the year's digits
	!	.tE
	!	^*12\* Units-of-Production
	!	.end table
	!	The field will accommodate two (2) alphanumeric characters.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Pressing ^*<List Choices>\* will provide a list of valid
	!	Calculation codes.
	!
	! Index:
	!	.x Method>Depreciation Method Description
	!
	!--

			AD_METHOD::CALCULATION = &
				ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;32", TEMP$, AD_METHOD::CALCULATION, &
				MFLAG, "'E", MVALUE, CALC$(),CALCTITLE$, "005")

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_METHOD = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_METHOD::DEP_METHOD = ""
			THEN
				AD_MAIN_METHOD = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_METHOD::DEP_METHOD + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_METHOD = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
				END IF
			END IF

		CASE 3%
			IF AD_METHOD::CALCULATION = ""
			THEN
				AD_MAIN_METHOD = 1%
			END IF

		END SELECT

	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Need to remove under old key, and insert under
		! (possibly) new key
		!
		CASE "Change", "Blank", "Initialize"

		IF AD_METHOD::DEP_METHOD <> AD_METHOD_OLD::DEP_METHOD
		THEN
			ST% = LIBR_EXTRACTVAR( &
				TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
				LAST_NOTE$, &
				TRM$(AD_METHOD::DEP_METHOD))

			ST% = LIBR_DELETE( &
				TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
				TRM$(AD_METHOD_OLD::DEP_METHOD))

			ST% = LIBR_INSERTTEXT( &
				TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
				LAST_NOTE$, &
				TRM$(AD_METHOD::DEP_METHOD))

		END IF

		!
		! Need to remove text
		!
		CASE "Erase"

			ST% = LIBR_DELETE( &
				TRM$(AD_METHOD.DEV1$) + "AD_METHOD", &
				TRM$(AD_METHOD::DEP_METHOD))

		END SELECT


	!
	! Set AD_METHOD_OLD value
	!
20500	CASE OPT_SETOLD
		AD_METHOD_OLD = AD_METHOD

	!
	! Restore AD_METHOD_OLD value
	!
	CASE OPT_RESETOLD
		AD_METHOD = AD_METHOD_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_METHOD2 = AD_METHOD

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_METHOD = AD_METHOD2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code Description                    " + &
				"          MthdType"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,049"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_METHOD::DEP_METHOD + " " + &
				AD_METHOD::DESCRIPTION + " " + &
				AD_METHOD::CALCULATION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_METHOD::DEP_METHOD + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_METHOD::CALCULATION + &
				AD_METHOD::DEP_METHOD, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
