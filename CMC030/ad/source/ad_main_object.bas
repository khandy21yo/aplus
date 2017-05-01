1	%TITLE "Depreciation Object Description"
	%SBTTL "AD_MAIN_OBJECT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_OBJECT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Depreciation Object Description\* screen defines
	!	an object code and enters a description for each code.  The
	!	Object refers to the different purposes for which depreciation
	!	is to be calculated and tracked.  For example:
	!	.table 3,25
	!	.te
	!	B = Book
	!	.te
	!	T = Tax
	!	.end table
	!	Objects other than "book" and "tax" can also be defined.
	!
	! Index:
	!	.x Description>Object
	!	.x Object>Description
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_OBJECT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_OBJECT
	!	$ DELETE AD_MAIN_OBJECT.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Updated for V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	09/25/96 - Kevin Handy
	!		Reformat source code.
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD	AD_OBJECT
	MAP (AD_OBJECT_OLD)	AD_OBJECT_CDD	AD_OBJECT_OLD, AD_OBJECT2

	MAP (CH_AD_OBJECT) AD_OBJECT.CH%

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
		SMG_WINDOW::DESCR = "Depreciation Object Description"
		SMG_WINDOW::NHELP = "AD_MAIN_OBJECT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Object"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_OBJECT.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open AD_OBJECT file " + NUM1$(ERR), 0%)
				AD_MAIN_OBJECT = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_OBJECT.CH%

		WHEN ERROR IN
			RESET #AD_OBJECT.CH%
			GET #AD_OBJECT.CH%, REGARDLESS
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


		DATA	8,  20, "(01) Dep Object", &
			10,  20, "(02) Description", &
			0,   0, ""

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
	!	^*(01) Depreciation Object\*
	!	.b
	!	.lm +5
	!	This field enters the code for a particular
	!	depreciation object.  Example entries are:
	!	.table 3,25
	!	.te
	!	^*B\* = Book
	!	.te
	!	^*T\* = Tax
	!	.end table
	!	The field will accommodate an entry of one (01) alphanumeric
	!	character.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation>Object Code
	!	.x Object>Depreciation
	!	.x Depreciation>Code
	!
	!--

			AD_OBJECT::DEP_OBJECT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;38", TEMP$, &
				AD_OBJECT::DEP_OBJECT, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The Description field enters the
	!	description of the Depreciation object code entered in
	!	field (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Object Description
	!
	!--

			AD_OBJECT::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;38", TEMP$, &
				AD_OBJECT::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_OBJECT = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_OBJECT::DEP_OBJECT = ""
			THEN
				AD_MAIN_OBJECT = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_OBJECT::DEP_OBJECT + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_OBJECT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set AD_OBJECT_OLD value
	!
20500	CASE OPT_SETOLD
		AD_OBJECT_OLD = AD_OBJECT

	!
	! Restore AD_OBJECT_OLD value
	!
	CASE OPT_RESETOLD
		AD_OBJECT = AD_OBJECT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_OBJECT2 = AD_OBJECT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_OBJECT = AD_OBJECT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_OBJECT::DEP_OBJECT + "    " + &
				AD_OBJECT::DESCRIPTION
			END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_OBJECT::DEP_OBJECT + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
