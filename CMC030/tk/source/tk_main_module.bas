1	%TITLE "Module Description"
	%SBTTL "TK_MAIN_MODULE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_MODULE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! any other copies therof may not be provided or otherwise made
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
	!	.p
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_MODULE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_MODULE
	!	$ DELETE TK_MAIN_MODULE.OBJ;*
	!
	! Author:
	!
	!	01/16/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE)		TK_MODULE_CDD	TK_MODULE
	MAP (TK_MODULE_OLD)	TK_MODULE_CDD	TK_MODULE_OLD, TK_MODULE2

	MAP (CH_TK_MODULE) TK_MODULE.CH%

	%PAGE

	ON ERROR GOTO 29000

	!
	! List of languages
	!
	LANGTITLE$ = "DefRef Description"
	LANG$(0%) = "6"
	LANG$(1%) = "ADA"
	LANG$(2%) = "BASIC"
	LANG$(3%) = "C"
	LANG$(4%) = "COBOL"
	LANG$(5%) = "FORTRAN"
	LANG$(6%) = "PASCAL"

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
		SMG_WINDOW::DESCR = "Module Description"
		SMG_WINDOW::NHELP = "TK_MAIN_MODULE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 16%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 11%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Module_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Category"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 5%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Type"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 7%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "module_Number"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 8%
		SMG_WINDOW::KNAME(4%) = "Language"
			SMG_WINDOW::KFIELD(4%, 0%) = 1%
			SMG_WINDOW::KFIELD(4%, 1%) = 8%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (TK_MODULE.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open TK_MODULE file " + NUM1$(ERR), 0%)
				TK_MAIN_MODULE = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = TK_MODULE.CH%

		WHEN ERROR IN
			RESET #TK_MODULE.CH%
			GET #TK_MODULE.CH%, REGARDLESS
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


		DATA	05,05, "(01) Module Name", &
			06,05, "(02) Description", &
			07,05, "(03) Extension", &
			08,05, "(04) Language", &
			09,05, "(05) Category", &
			10,05, "(06) Directory", &
			11,05, "(07) Module Type", &
			12,05, "(08) Module #", &
			14,05, "(09) Date", &
			15,05, "(10) Time", &
			16,05, "(11) Shareable", &
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

			TK_MODULE::MODNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				TK_MODULE::MODNAME, MFLAG, "'E", MVALUE)

		CASE 2%

			TK_MODULE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;23", TEMP$, &
				LEFT(TK_MODULE::DESCRIPTION, 55%), MFLAG, "'E", &
				MVALUE)

		CASE 3%

			TK_MODULE::EXTENSION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;23", TEMP$, &
				TK_MODULE::EXTENSION, MFLAG, "'E", MVALUE)

		CASE 4%

			TK_MODULE::LANGUAGE = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;23", TEMP$, &
				TK_MODULE::LANGUAGE, MFLAG, "'E", &
				MVALUE, LANG$(), LANGTITLE$, "")

		CASE 5%

			TK_MODULE::CATEGORY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;23", TEMP$, &
				TK_MODULE::CATEGORY, MFLAG, "'E", MVALUE)

		CASE 6%

			TK_MODULE::DIRECTORY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;23", TEMP$, &
				TK_MODULE::DIRECTORY, MFLAG, "'E", MVALUE)

		CASE 7%

			TK_MODULE::MODTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;23", TEMP$, &
				TK_MODULE::MODTYPE, MFLAG, "'E", MVALUE)

		CASE 8%

			TK_MODULE::MODNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;23", TEMP$, &
				TK_MODULE::MODNUM, MFLAG, "'E", MVALUE)
		CASE 9%

			TK_MODULE::CDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;25", TEMP$, &
				TK_MODULE::CDATE, MFLAG, "'E", MVALUE)

		CASE 10%

			TK_MODULE::CTIME= ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;25", TEMP$, &
				TK_MODULE::CTIME, MFLAG, "'E", MVALUE)

		CASE 11%

			TK_MODULE::SHAREABLE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;23", TEMP$, &
				TK_MODULE::SHAREABLE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TK_MAIN_MODULE = 0%

		SELECT MLOOP
					! MODNUM
		CASE 1%
			IF TK_MODULE::MODNAME = ""
			THEN
				TK_MAIN_MODULE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ TK_MODULE::MODNAME + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					TK_MAIN_MODULE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set TK_MODULE_OLD value
	!
20500	CASE OPT_SETOLD
		TK_MODULE_OLD = TK_MODULE

	!
	! Restore TK_MODULE_OLD value
	!
	CASE OPT_RESETOLD
		TK_MODULE = TK_MODULE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_MODULE2 = TK_MODULE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_MODULE = TK_MODULE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Module Name                            " + &
			"  Description                               " + &
			" Cat     Type   IdNum  CreateDate Language"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "043,086,094,101,108,119"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = TK_MODULE::MODNAME + "  " + &
				LEFT(TK_MODULE::DESCRIPTION, 40%) + "   " + &
				TK_MODULE::CATEGORY + "  " + &
				TK_MODULE::MODTYPE + "   " + &
				TK_MODULE::MODNUM + " " + &
				PRNT_DATE(TK_MODULE::CDATE, 8%) + " " + &
				TK_MODULE::LANGUAGE

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_MODULE::MODNAME + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE TK_MODULE::CATEGORY + &
				TK_MODULE::MODNUM, &
				REGARDLESS
		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE TK_MODULE::MODTYPE + &
				TK_MODULE::MODNUM, &
				REGARDLESS
		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE TK_MODULE::MODNUM + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
