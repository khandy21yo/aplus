1	%TITLE "Application Documentation Maintenance"
	%SBTTL "TK_MAST_APPLICATION"
	%IDENT "V3.6a Calico"

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
	!	This is a maintenance program for tracking
	!	application programs and keeping an inventory.
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	02/28/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/09/87 - Kevin Handy
	!		Modified to use library files.
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAST_APPLICATION/LINE
	!	$ LINK/EXE=TK_EXE: TK_MAST_APPLICATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_MAST_APPLICATION.OBJ;*
	!
	! Modification History:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Change 'SMG_PDID' to 'SCOPE::SMG_PBID'
	!		Change 'SVDID'  to 'SVDID%'
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	EXTERNAL LONG  FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	V% = MAIN_WINDOW(16251%, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE,  "", "")

19990	END



20000	FUNCTION INTEGER MAINT_GROUP(CDD_WINDOW_CDD WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)


	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_APPLICATION.HB"
	MAP (TK_APPLICATION)	TK_APPLICATION_CDD	TK_APPLICATION
	MAP (TK_APPLICATION2)	TK_APPLICATION_CDD	LOG_OLD, TK_APPLICATION2

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	MAP (FOO_BAR_FRED)	TK_APPLICATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION EDT$EDIT
	EXTERNAL LONG    FUNCTION LIBR_MAINT

	!
	! Declare data types
	!
	DECLARE	LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000


	SELECT MOPTION

	CASE OPT_INIT
		WINDOW::DESCR = " Program APPLICATION Maintenance "
		WINDOW::NHELP = "TK_APPLICATION"
		WINDOW::HSIZE = 78%
		WINDOW::VSIZE = 18%
		WINDOW::HVIEW = 78%
		WINDOW::VVIEW = 18%
		WINDOW::HPOS  = 2%
		WINDOW::VPOS  = 2%
		WINDOW::NITEMS= 5%
		WINDOW::FLAGS = 1%		! Allows a window

		WINDOW::NKEYS = 4%
		WINDOW::KNAME(0%) = "Id_number"
			WINDOW::KFIELD(0%, 0%) = 1%
			WINDOW::KFIELD(0%, 1%) = 1%
		WINDOW::KNAME(1%) = "Filename"
			WINDOW::KFIELD(1%, 0%) = 1%
			WINDOW::KFIELD(1%, 1%) = 2%
		WINDOW::KNAME(2%) = "Description"
			WINDOW::KFIELD(2%, 0%) = 1%
			WINDOW::KFIELD(2%, 1%) = 3%
		WINDOW::KNAME(3%) = "Catagory"
			WINDOW::KFIELD(3%, 0%) = 1%
			WINDOW::KFIELD(3%, 1%) = 4%

		CALL read_Defaults(WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[TK.OPEN]TK_APPLICATION.CRE"

		RESET #TK_APPLICATION.CH%
		GET #TK_APPLICATION.CH%, REGARDLESS

		WINDOW::CHAN  = TK_APPLICATION.CH%


	!
	! Window option
	!
	CASE 90%
		W.LIST$ = "Documentation Create-documentation Editsource eXit"

		TEMP$ = SCOPE::PRG_ITEM
		SCOPE::PRG_ITEM = "W_"

 WLoop:		WIN$ = ENTR_3OPTION(SCOPE, "WINDOW", W.LIST$, WIN.OPT%, 0%)

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_PF2, &
			SMG$K_TRM_F8,    SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			SCOPE::PRG_ITEM = TEMP$ + ""
			EXIT FUNCTION

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			! Good key

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO WLoop
		END SELECT

		SCOPE::PRG_ITEM = TEMP$ + ""

		SELECT WIN$
		!
		! Exit
		!
		CASE "X"
			SCOPE::PRG_ITEM = TEMP$ + ""
			EXIT FUNCTION

		!
		! Edit the source code
		!
		CASE "E"
 ELoop:			FILENAME$ = TRM$(TK_APPLICATION::MLOCA) + &
				TRM$(TK_APPLICATION::MNAME)

			CALL ENTR_3MESSAGE(SCOPE, 'Editing "'+FILENAME$ + &
				'"', 4%)
			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_CTRLC, &
				SMG$K_TRM_F8, &
				SMG$K_TRM_F10, &
				SMG$K_TRM_PF2, &
				SMG$K_TRM_CTRLZ

				EXIT FUNCTION

			CASE 0%, 10%, 12%, 13%, &
				SMG$K_TRM_DO
				! Good key

			CASE ELSE
				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
				GOTO ELoop
			END SELECT

			SMG_STATUS% = SMG$ERASE_DISPLAY( &
				SCOPE::SMG_MESSAGE)

			SMG_STATUS% = SMG$SET_CURSOR_MODE( &
				SCOPE::SMG_PBID, 0%) ! On

			SMG_STATUS% = SMG$SAVE_PHYSICAL_SCREEN( &
				SCOPE::SMG_PBID, SVDID%)

			SMG_STATUS% = EDT$EDIT(FILENAME$,,,,,,,)

			SMG_STATUS% = &
				SMG$RESTORE_PHYSICAL_SCREEN( &
				SCOPE::SMG_PBID, SVDID%)

			SMG_STATUS% = SMG$SET_CURSOR_MODE( &
				SCOPE::SMG_PBID, 1%) ! Off

		!
		! Document
		!
		CASE "D"
			ST% = LIBR_MAINT("REF:APPLICATION", &
				TRM$(TK_APPLICATION::MNUMB), &
				"Module Documentation", 0%)

		!
		! Create_Documentation
		!
		CASE "C"
			CALL TK_SUBR_MODULEDOCU("REF:APPLICATION", &
				TK_APPLICATION::MNUMB, &
				TRM$(TK_APPLICATION::MLOCA) + &
					TRM$(TK_APPLICATION::MNAME), &
				TRM$(TK_APPLICATION::MNAME) + &
					" - " + &
					TRM$(TK_APPLICATION::MDESC))

		END SELECT

		GOTO Wloop

	!
	! Display the background
	!
20100	CASE 1%

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SCREEN_DATA%)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

		DATA	0, &
			4, 3, "(01) ID Number", &
			6, 3, "(02) Filename", &
			8, 3, "(03) Description", &
			10, 3, "(04) Catagory", &
			12, 3, "(05) Location", &
			0, 0, ""

		RESTORE

 D1Loop:	!
		! Search for right window
		!
		READ W%

		IF (W% <> MWINDOW%)
		THEN
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			GOTO D1Loop
		END IF

 DLoop:		READ XPOS, YPOS, XSTR$
		IF (XPOS <> 0%)
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				XSTR$, XPOS, YPOS)
			GOTO DLoop
		END IF

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SCREEN_DATA%)

	!
	! Enter/Display/Default
	!
20200	CASE 2%
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%
			TK_APPLICATION::MNUMB = ENTR_3STRING(SCOPE, &
				WINDOW::WNUMBER, "4;23", TEMP$, &
				TK_APPLICATION::MNUMB, MFLAG, "'E", MVALUE)

		CASE 2%
			TK_APPLICATION::MNAME = ENTR_3STRING(SCOPE, &
				WINDOW::WNUMBER, "6;23", TEMP$, &
				TK_APPLICATION::MNAME, MFLAG, "'E", MVALUE)

		CASE 3%
			TK_APPLICATION::MDESC = ENTR_3STRING(SCOPE, &
				WINDOW::WNUMBER, "8;23", TEMP$, &
				TK_APPLICATION::MDESC, MFLAG, "'E", MVALUE)

		CASE 4%
			TK_APPLICATION::MCATA = ENTR_3STRING(SCOPE, &
				WINDOW::WNUMBER, "10;23", TEMP$, &
				TK_APPLICATION::MCATA, MFLAG, "'E", MVALUE)

		CASE 5%
			TK_APPLICATION::MLOCA = ENTR_3STRING(SCOPE, &
				WINDOW::WNUMBER, "13;13", TEMP$, &
				TK_APPLICATION::MLOCA, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE 3%
		MAINT_GROUP = 0%

		SELECT MLOOP

		CASE 1%
			IF TK_APPLICATION::MNUMB = ""
			THEN
				MAINT_GROUP = 1%
			ELSE
				IF (MVALUE = "")
				THEN
					WHEN ERROR IN
						GET #TK_APPLICATION.CH%, KEY #0% EQ &
							TK_APPLICATION::MNUMB, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					MAINT_GROUP = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF
		END SELECT

	!
	! Set LOG_OLD value
	!
20500	CASE 5%
		LOG_OLD = TK_APPLICATION

	!
	! Restore LOG_OLD value
	!
	CASE 6%
		TK_APPLICATION = LOG_OLD

	!
	! Set default value
	!
	CASE 7%
		TK_APPLICATION2 = TK_APPLICATION

	!
	! Restore default value
	!
	CASE 8%
		TK_APPLICATION = TK_APPLICATION2

	!
	! View header
	!
	CASE 9%
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "   ID Number      Filename                         Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "016,048"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				FORMAT$(TK_APPLICATION::MNUMB, &
					"'LLLLLLLLLLLLLL") + " " + &
				FORMAT$(TK_APPLICATION::MNAME, &
					"'" + STRING$(30%, A"L"B)) + " " + &
				FORMAT$(TK_APPLICATION::MDESC, &
					"'" + STRING$(29%, A"L"B))

		END SELECT

	!
	! Find
	!
	CASE 11%
		SELECT MLOOP

		CASE 0%
			FIND #TK_APPLICATION.CH%, &
				KEY #0% GE TK_APPLICATION::MNUMB, &
				REGARDLESS
		CASE 1%
			FIND #TK_APPLICATION.CH%, &
				KEY #1% GE TK_APPLICATION::MNAME, &
				REGARDLESS
		CASE 2%
			FIND #TK_APPLICATION.CH%, &
				KEY #2% GE TK_APPLICATION::MDESC, &
				REGARDLESS
		CASE 3%
			FIND #TK_APPLICATION.CH%, &
				KEY #3% GE TK_APPLICATION::MCATA, &
				REGARDLESS
		END SELECT
	END SELECT

	EXIT FUNCTION

29000	!
	! Trap errors
	!
	FILENAME$ = ""
	ON ERROR GO BACK

32767	END FUNCTION
