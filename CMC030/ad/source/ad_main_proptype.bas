1	%TITLE "Property Type Description"
	%SBTTL "AD_MAIN_PROPTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_PROPTYPE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
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
	!	The ^*Property Type Description\* categorizes assets by property
	!	type which is necessary for the IRS depreciation form.
	!	.lm -5
	!
	! Index:
	!	.x Property Type>Description
	!	.x Description>Property Type
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_PROPTYPE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_PROPTYPE
	!	$ DELETE AD_MAIN_PROPTYPE.OBJ;*
	!
	! Author:
	!
	!	09/07/88 - Frank Starman
	!
	! Modification history:
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update for V3.6 coding standards.
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_PROPTYPE.HB"
	MAP (AD_PROPTYPE)	AD_PROPTYPE_CDD	AD_PROPTYPE
	MAP (AD_PROPTYPE_OLD)	AD_PROPTYPE_CDD	AD_PROPTYPE_OLD, AD_PROPTYPE2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_PROPTYPE) AD_PROPTYPE.CH%, &
		AD_PROPTYPE.READONLY%

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
		SMG_WINDOW::DESCR = "Property Type Description"
		SMG_WINDOW::NHELP = "AD_MAIN_PROPTYPE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

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
		IF AD_PROPTYPE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_PROPTYPE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_PROPTYPE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_PROPTYPE = ERR
			CONTINUE 770
		END WHEN

		AD_PROPTYPE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_PROPTYPE.OPN"
		USE
			AD_MAIN_PROPTYPE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_PROPTYPE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_PROPTYPE.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_PROPTYPE.CH%
		WHEN ERROR IN
			RESET #AD_PROPTYPE.CH%
			GET #AD_PROPTYPE.CH%, REGARDLESS
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


		DATA	08,  05, "(01) Property Type", &
			09,  05, "(02) Description", &
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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Property Type\*
	!	.b
	!	.lm +5
	!	The ^*Property Type\* field indicates the type of property class the object
	!	is in by entering the correct code.
	!	.lm -5
	!
	! Index:
	!	.x Property>Type
	!	.x Type>Property
	!
	!--

			AD_PROPTYPE::PROPTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;30", TEMP$, &
				AD_PROPTYPE::PROPTYPE, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description of the
	!	property code entered in field (01).
	!	.b
	!	The field will accommodate twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>Property Type Description
	!	.x Property Type Description>Description
	!
	!--

			AD_PROPTYPE::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;30", TEMP$, &
				AD_PROPTYPE::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		AD_MAIN_PROPTYPE = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_PROPTYPE::PROPTYPE = ""
			THEN
				AD_MAIN_PROPTYPE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ AD_PROPTYPE::PROPTYPE + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AD_MAIN_PROPTYPE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

20500	!******************************************************************
	! Set GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		AD_PROPTYPE_OLD = AD_PROPTYPE

	!*********************************************************************
	! Restore AD_PROPTYPE_OLD value
	!*********************************************************************
	CASE OPT_RESETOLD
		AD_PROPTYPE = AD_PROPTYPE_OLD

	!**********************************************************************
	! Set default value
	!**********************************************************************
	CASE OPT_SETDEFAULT
		AD_PROPTYPE2 = AD_PROPTYPE

	!**********************************************************************
	! Restore default value
	!**********************************************************************
	CASE OPT_RESETDEFAULT
		AD_PROPTYPE = AD_PROPTYPE2

	!**********************************************************************
	! View header
	!**********************************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Type Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_PROPTYPE::PROPTYPE + "   " + &
				AD_PROPTYPE::DESCRIPTION
			END SELECT

	!**********************************************************************
	! Find
	!**********************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_PROPTYPE::PROPTYPE + "", &
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
	! End of AD_MAIN_PROPTYPE function
	!******************************************************************
	END FUNCTION

