1	%TITLE "Depreciation Class Description"
	%SBTTL "AD_MAIN_DEPCLASS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_DEPCLASS(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Depreciation Class Description\* enters the necessary
	!	information needed for calculation of the current period depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Class>Depreciation
	!	.x Depreciation>Class
	!	.x Class>Depreciation
	!	.x Depreciation>Class
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_DEPCLASS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_DEPCLASS
	!	$ DELETE AD_MAIN_DEPCLASS.OBJ;*
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
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit% to scope::scope_exit.
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
	!	10/16/98 - Kevin Handy
	!		Lose excess %PAGE
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
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.HB"
	MAP (AD_DEPCLASS)	AD_DEPCLASS_CDD	AD_DEPCLASS
	MAP (AD_DEPCLASS_OLD)	AD_DEPCLASS_CDD	AD_DEPCLASS_OLD, AD_DEPCLASS2

	%INCLUDE "SOURCE:[AD.OPEN]AD_METHOD.HB"
	MAP (AD_METHOD)	AD_METHOD_CDD	AD_METHOD

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLE.HB"
	MAP (AD_TABLE)	AD_TABLE_CDD	AD_TABLE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILING.HB"
	MAP (AD_CEILING)	AD_CEILING_CDD	AD_CEILING

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.HB"
	MAP (AD_CONVENTION)	AD_CONVENTION_CDD	AD_CONVENTION

	%INCLUDE "SOURCE:[AD.OPEN]AD_PROPTYPE.HB"
	MAP (AD_PROPTYPE)	AD_PROPTYPE_CDD	AD_PROPTYPE

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_DEPCLASS) AD_DEPCLASS.CH%, &
		AD_DEPCLASS.READONLY%

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
		SMG_WINDOW::DESCR = "Depreciation Class Description"
		SMG_WINDOW::NHELP = "AD_MAIN_DEPCLASS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 12%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Class"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AD_DEPCLASS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_DEPCLASS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_DEPCLASS = ERR
			CONTINUE 770
		END WHEN

		AD_DEPCLASS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_DEPCLASS.OPN"
		USE
			AD_MAIN_DEPCLASS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_DEPCLASS.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_DEPCLASS.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_DEPCLASS.CH%
		WHEN ERROR IN
			RESET #AD_DEPCLASS.CH%
			GET #AD_DEPCLASS.CH%, REGARDLESS
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


		DATA	04,  05, "(01) Depreciation Class", &
			05,  05, "(02) Description", &
			06,  05, "(03) Property Type", &
			07,  05, "(04) Dep Method", &
			08,  05, "(05) Dep Optional Table", &
			09,  05, "(06) Recovery Period", &
			10,  05, "(07) First Year Conv", &
			11,  05, "(08) Disp Year Conv", &
			12,  05, "(09) Ceiling Table", &
			13,  05, "(10) Consider Salvage", &
			14,  05, "(11) Consider Bonus", &
			15,  05, "(12) Consider ITC", &
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

 ReEntry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Depreciation Class\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Class\* field enters the class in
	!	which the object is assigned for depreciation purposes.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Class>Depreciation Class Table
	!	.x Depreciation Class Table>Depreciation Class
	!
	!--

			AD_DEPCLASS::DEPCLASS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;30", TEMP$, &
				AD_DEPCLASS::DEPCLASS, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters the description of the
	!	Depreciation class code entered in field one (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Depreciation Class Table
	!	.x Depreciation Class Table>Description
	!
	!--

			AD_DEPCLASS::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;30", TEMP$, &
				AD_DEPCLASS::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Property Type\*
	!	.b
	!	.lm +5
	!	The ^*Property Type\* field indicates the type of property class the object
	!	is in by entering the correct code.
	!	.b
	!	By pressing the ^*<List Choices>\* key, while in the ^*Property Type\* field,
	!	all valid options are listed.
	!	.lm -5
	!
	! Index:
	!	.x Property Type>Depreciation Class Description
	!	.x Depreciation Class Description>Property Type
	!
	!--

			AD_DEPCLASS::PROPTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, AD_DEPCLASS::PROPTYPE, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_PROPTYPE.ID, "V0") = 1%
				THEN
					AD_DEPCLASS::PROPTYPE = &
						AD_PROPTYPE::PROPTYPE
				END IF
				GOTO Reentry
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Depreciation Method\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Method\* field contains the method which will be used
	!	in depreciation calculations. By pressing the ^*<List Choices>\* key a
	!	listing of all choices will be shown. By using the arrow keys to move to
	!	the desired choice and then pressing the ^*<Select>\* key, any method may
	!	be chosen.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Method>Depreciation Class Description
	!	.x Depreciation Class Description>Depreciation Method
	!
	!--

			AD_DEPCLASS::DEPMETHOD= &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, AD_DEPCLASS::DEPMETHOD, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_METHOD.ID, "V0") = 1%
				THEN
				AD_DEPCLASS::DEPMETHOD = &
						AD_METHOD::DEP_METHOD
				END IF
				GOTO Reentry
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Depreciation Optional Table\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Optional Table\* field enters of the optional
	!	table containing the percentage to be depreciated every year. A full listing of
	!	the Optional Tables may be accessed by pressing the ^*<List Choices>\* key.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Optional Table>Depreciation Class Description
	!	.x Depreciation Class description>Depreciation Optional Table
	!
	!--

			AD_DEPCLASS::OPTTABLE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;30", TEMP$, AD_DEPCLASS::OPTTABLE, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_TABLE.ID, "V0") = 1%
				THEN
					AD_DEPCLASS::OPTTABLE = &
						AD_TABLE::OPTTABLE
				END IF
				GOTO Reentry
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Recovery Period\*
	!	.b
	!	.lm +5
	!	The ^*Recovery Period\* enters how long it will take to
	!	depreciate the item fully or to the salvage value.
	!	.lm -5
	!
	! Index:
	!	.x Recovery Period>Depreciation Class Description
	!	.x Depreciation Class Description>REcovery Period
	!
	!--

			YEARS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;30", TEMP$, &
				VAL(EDIT$(AD_DEPCLASS::YEARS, -1%)), &
				MFLAG, "<0>#.#", MVALUE)

			AD_DEPCLASS::YEARS = FORMAT$(YEARS, "<0>#.#")

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) First Year Convention\*
	!	.b
	!	.lm +5
	!	The ^*First Year Convention\* field enters the depreciation
	!	convention which will be used in the first year of the depreciation. Several
	!	options are available and may be viewed by pressing
	!	the ^*<List Choices>\* key.
	!	.lm -5
	!
	! Index:
	!	.x First  Year Convention>Depreciation Class Description
	!	.x Depreciation Class Description>First Year Convention
	!
	!--

			AD_DEPCLASS::FYCONV = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, AD_DEPCLASS::FYCONV, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_CONVENTION.ID, "V0") = 1%
				THEN
				AD_DEPCLASS::FYCONV= &
						AD_CONVENTION::CONVENTION
				END IF
				GOTO Reentry
			END IF

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Disposal Year Convention\*
	!	.b
	!	.lm +5
	!	The ^*Disposal Year Convention\* field enters the depreciation
	!	convention which will be used in the final year of the depreciation. Several
	!	options are available and may be viewed by pressing
	!	the ^*<List Choices>\* key.
	!	.lm -5
	!
	! Index:
	!	.x Disposal Year Convention>Depreciation Class Description
	!	.x Depreciation Class Description>Disposal Year Convention
	!
	!--

			AD_DEPCLASS::DYCONV = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;30", TEMP$, AD_DEPCLASS::DYCONV, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_CONVENTION.ID, "V0") = 1%
				THEN
				AD_DEPCLASS::DYCONV= &
						AD_CONVENTION::CONVENTION
				END IF
				GOTO Reentry
			END IF

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Ceiling Table\*
	!	.b
	!	.lm +5
	!	The ^*Ceiling Table\* field contains the reference to the ceiling
	!	table used in the calculation of the depreciation. By pressing the ^*<List
	!	Choices>\* key, a listing of all Ceiling Table references is displayed. If no
	!	Ceiling Table is applicable, the field should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Ceiling Table>Depreciation Class Description
	!	.x Depreciation Class Description>Ceiling Table
	!
	!--

			AD_DEPCLASS::CEILTABLE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, AD_DEPCLASS::CEILTABLE, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_CEILING.ID, "V0") = 1%
				THEN
				AD_DEPCLASS::CEILTABLE= &
						AD_CEILING::CEILTABLE
				END IF
				GOTO Reentry
			END IF

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Consider Salvage\*
	!	.b
	!	.lm +5
	!	The ^*Consider Salvage\* field enters the user decision to
	!	whether the Salvage value should be considered when
	!	calculating the depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Consider Salvage>Depreciation Class Description
	!	.x Depreciation Class Description>Consider Salvage
	!
	!--

			AD_DEPCLASS::SALVFACTOR = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;30", TEMP$, AD_DEPCLASS::SALVFACTOR, &
				MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Consider Bonus\*
	!	.b
	!	.lm +5
	!	The ^*Consider Bonus\* field enters the user decision to
	!	whether the Bonus entered in the Master File should be considered when
	!	calculating the depreciation.
	!	.lm-5
	!
	! Index:
	!	.x Consider Bonus>Depreciation Class Description
	!	.x Depreciation Class Description>Consider Bonus
	!
	!--

			AD_DEPCLASS::BONUSFACTOR = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;30", TEMP$, AD_DEPCLASS::BONUSFACTOR, &
				MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Consider ITC\*
	!	.b
	!	.lm +5
	!	The ^*Consider ITC\* field enters the user decision to
	!	whether the ITC entered in the Master File should be considered when calculating
	!	the depreciation.
	!	.lm -5
	!
	! Index:
	!	.x Consider ITC>Depreciation Class Description
	!	.x Depreciation Class Description>Consider ITC
	!
	!--

			AD_DEPCLASS::ITCFACTOR = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;30", TEMP$, AD_DEPCLASS::ITCFACTOR, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		AD_MAIN_DEPCLASS = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_DEPCLASS::DEPCLASS = ""
			THEN
				AD_MAIN_DEPCLASS = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_DEPCLASS::DEPCLASS + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_DEPCLASS = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

20500	!******************************************************************
	! Set GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		AD_DEPCLASS_OLD = AD_DEPCLASS

	!*********************************************************************
	! Restore AD_DEPCLASS_OLD value
	!*********************************************************************
	CASE OPT_RESETOLD
		AD_DEPCLASS = AD_DEPCLASS_OLD

	!**********************************************************************
	! Set default value
	!**********************************************************************
	CASE OPT_SETDEFAULT
		AD_DEPCLASS2 = AD_DEPCLASS

	!**********************************************************************
	! Restore default value
	!**********************************************************************
	CASE OPT_RESETDEFAULT
		AD_DEPCLASS = AD_DEPCLASS2

	!**********************************************************************
	! View header
	!**********************************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Class Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_DEPCLASS::DEPCLASS + "  " + &
				AD_DEPCLASS::DESCRIPTION
		END SELECT

	!**********************************************************************
	! Find
	!**********************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_DEPCLASS::DEPCLASS + "", &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of AD_MAIN_DEPCLASS function
	!******************************************************************
	END FUNCTION

