1	%TITLE "Convention Definition"
	%SBTTL "AD_MAIN_CONVENTION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CONVENTION(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Convention Definition\* option enters
	!	a definition for all conventions to be used in the Asset Depreciation
	!	system.  Examples are half-year, year, quarter, mid-quarter,
	!	month and mid-month conventions.
	!	.b
	!	For example, for half-year convention using the "Regardless"
	!	specification, one-half of a full year's depreciation will be
	!	calculated for the first year the subject property is placed in
	!	service, regardless of what date the property is actually placed in
	!	service.  For each of the remaining years of the life of the asset,
	!	a full year of depreciation is accumulated.  If the property is held
	!	for the entire recovery period, one-half of the normal annual
	!	depreciation will be calculated for the year following the end of the
	!	recovery period.  If the property is disposed of before the end of
	!	the recovery period, one-half of the normal annual depreciation will
	!	be calculated in the year of disposition.
	!	.lm -5
	!
	! Index:
	!	.x Convention>Definition
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CONVENTION/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CONVENTION
	!	$ DELETE AD_MAIN_CONVENTION.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - Frank F. Starman
	!
	! Modification history:
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.HB"
	MAP (AD_CONVENTION)		AD_CONVENTION_CDD	AD_CONVENTION
	MAP (AD_CONVENTION_OLD)	AD_CONVENTION_CDD	AD_CONVENTION_OLD, &
		AD_CONVENTION2, AD_CONVENTION3

	MAP (CH_AD_MAIN_CONVENTION) AD_CONVENTION.CH%

	MAP (TT_AD_MAIN_CONVENTION) AD_CONVENTION.DEV1$ = 64%

	COM (TABLE_AD_CONVENTION) &
		CALCTITLE$ = 20%, &
		CALC$(4%) = 20%

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
		SMG_WINDOW::DESCR = "Convention Definition Table"
		SMG_WINDOW::NHELP = "AD_MAIN_CONVENTION"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Specification
		!
		CALCTITLE$ = "Code  Description"
		CALC$(0%) = "4"
		CALC$(1%) = "B    Backward"
		CALC$(2%) = "F    Foreward"
		CALC$(3%) = "I    Inward"
		CALC$(4%) = "O    Outward"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_CONVENTION.CH% <= 0%)
		THEN
			CALL READ_DEVICE("AD_CONVENTION", &
				AD_CONVENTION.DEV1$, STAT%)
			AD_CONVENTION.DEV1$ = "SYS$LOGIN:" &
				IF AD_CONVENTION.DEV1$ = ""

			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, "Unable to open AD_CONVENTION file " + NUM1$(ERR), 0%)
				AD_MAIN_CONVENTION = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_CONVENTION.CH%

		WHEN ERROR IN
			RESET #AD_CONVENTION.CH%
			GET #AD_CONVENTION.CH%, REGARDLESS
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


		DATA	04,10, "(01) Code", &
			05,10, "(02) Description", &
			06,10, "(03) Coefficient", &
			07,10, "(04) Specification", &
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
			TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
			LAST_NOTE$, &
			TRM$(AD_CONVENTION::CONVENTION))

		LAST_NOTE$ = ENTR_NOTE(SMG_WINDOW::WNUMBER, &
			"8;1", TEMP$, &
			LAST_NOTE$, 64%, "11;78", MVALUE)

		ST% = LIBR_INSERTTEXT( &
			TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
			LAST_NOTE$, &
			TRM$(AD_CONVENTION::CONVENTION))


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
	!	^*(01) Code\*
	!	.b
	!	.lm +5
	!	The ^*Code\* field enters a code which
	!	will describe how the asset will be depreciated in the
	!	first year.
	!	.b
	!	Examples of conventions are:
	!	.table 3,25
	!	.te
	!	_* HY = Half Year
	!	.te
	!	_* MQ = Mid Quarter
	!	.te
	!	_* MM = Mid Month
	!	.end table
	!	Duplicate entries are not allowed.
	!	.lm -5
	!
	! Index:
	!	.x Convention>Code
	!
	!--


			AD_CONVENTION::CONVENTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;30", TEMP$, &
				AD_CONVENTION::CONVENTION, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The Description field enters a description
	!	of the convention code entered in field (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Convention
	!	.x Convention>Description
	!
	!--


			AD_CONVENTION::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;30", TEMP$, &
				AD_CONVENTION::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Coefficient\*
	!	.b
	!	.lm +5
	!	The ^*Coefficient\* field enters the number of
	!	months representing a particular convention.
	!	.b
	!	For example, for a half year convention, 6 (six) would be
	!	entered and for a month convention, 1 (one) would be entered.
	!	.lm -5
	!
	! Index:
	!	.x Coefficient
	!
	!--


			AD_CONVENTION::COEFF = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, &
				AD_CONVENTION::COEFF * 0.01, MFLAG, "##.##", &
				MVALUE) * 100%

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Specification\*
	!	.b
	!	.lm +5
	!	The ^*Specification\* field will represent the first month
	!	of depreciation.
	!	.b
	!	This field requires an entry.
	!	.b
	!	The only valid specification codes are:
	!	.table 3,25
	!	.te
	!	^*B\* = Carry Backward
	!	.te
	!	^*F\* = Carry Forward
	!	.te
	!	^*I\* = Inward
	!	.te
	!	^*O\* = Outward
	!	.end table
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will display a list of valid codes.
	!	.b
	!	Example:
	!	.b
	!	.lm 10
	!	If the coefficient = 6 (Half Year Convention)
	!	and the specification = B (Carry Backward) and
	!	the service date of the asset is in the first
	!	half of the year, then it will be equal to twelve (12)
	!	months depreciation in the first year.
	!	.b
	!	If the coefficient = 6 (Half Year Convention)
	!	and the specification = B (Carry Backward) and
	!	the service date of the asset is in the second
	!	half of the year, depreciation for the first year
	!	will be equal to six (6) months depreciation.
	!	.b
	!	If the coefficient = 6 (Half Year Convention)
	!	and the specification = F (Carry Forward) and
	!	the service date of the asset is in the first
	!	half of the year, then no depreciation in the
	!	first year will be calculated.
	!	.b
	!	If the coefficient = 6 (Half Year Convention)
	!	and the specification = F (Carry Forward) and
	!	the service date of the asset is in the last
	!	half of the year, then no depreciation in the
	!	first year will be calculated.
	!
	!
	! Index:
	!	.x Specification
	!
	!--

			AD_CONVENTION::SPECIFIC= ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, AD_CONVENTION::SPECIFIC, &
				MFLAG, "'", MVALUE, CALC$(), &
				CALCTITLE$, "005")

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_CONVENTION = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_CONVENTION::CONVENTION = ""
			THEN
				AD_MAIN_CONVENTION = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_CONVENTION::CONVENTION + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_CONVENTION = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
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

		IF AD_CONVENTION::CONVENTION <> AD_CONVENTION_OLD::CONVENTION
		THEN
			ST% = LIBR_EXTRACTVAR( &
				TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
				LAST_NOTE$, &
				TRM$(AD_CONVENTION::CONVENTION))

			ST% = LIBR_DELETE( &
				TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
				TRM$(AD_CONVENTION_OLD::CONVENTION))

			ST% = LIBR_INSERTTEXT( &
				TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
				LAST_NOTE$, &
				TRM$(AD_CONVENTION::CONVENTION))

		END IF
		!
		! Need to remove text
		!
		CASE "Erase"

			ST% = LIBR_DELETE( &
				TRM$(AD_CONVENTION.DEV1$) + "AD_CONVENTION", &
				TRM$(AD_CONVENTION::CONVENTION))

		END SELECT


	!
	! Set AD_CONVENTION_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CONVENTION_OLD = AD_CONVENTION

	!
	! Restore AD_CONVENTION_OLD value
	!
	CASE OPT_RESETOLD
		AD_CONVENTION = AD_CONVENTION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CONVENTION2 = AD_CONVENTION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CONVENTION = AD_CONVENTION2

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
				"          Coeff Specif"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,049,055"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_CONVENTION::CONVENTION + "   " + &
				AD_CONVENTION::DESCRIPTION + " " + &
				FORMAT$(0.01 * AD_CONVENTION::COEFF, "##,##") + &
				" " + &
				AD_CONVENTION::SPECIFIC
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_CONVENTION::CONVENTION + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
