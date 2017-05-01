1	%TITLE "Generate COM Files to Install a System"
	%SBTTL "SI_SPEC_INSTALL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
	!
	! Computer Management Center, Inc.
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
	!	.p
	!	This program prints all programs that it can find out
	!	about that are used in a system.  It only looks for
	!	programs that menu and report call, so a program that
	!	chains to another will not show up here.  It will create
	!	a com file to copy systems onto {device}:[kits.a,b,c]
	!	directory.  Then will create the com file for the laydown
	!	under VMS install and finally it will modify the
	!	KITINSTALL.COM.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	04/01/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS SI_SOURCE:SI_SPEC_INSTALL/LINE
	!	$ LINK/EXE=SI_EXE: SI_SPEC_INSTALL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SI_SPEC_INSTALL.OBJ;*
	!
	! Modification history:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING.
	!
	!	05/30/89 - Kevin Handy
	!		Added installing CMC_3SHARE.EXE file.
	!
	!	06/19/89 - Kevin Handy
	!		Minor modification in CMC_31SHARE install.
	!
	!	08/01/89 - Frank Starman
	!		Modified to install REF:TK_FILEDICT (master device file)
	!
	!	08/08/89 - Aaron Redd
	!		Modified to reflect changes in Support System files.
	!
	!	08/29/89 - Kevin Handy
	!		Major modifications to the way that the install
	!		works.  Putting all products on seperate tapes.
	!		Using VMI$CALLBACK instead of the original hacks.
	!		Simplified KITINSTAL.COM files.  Only requires
	!		one directory to do the install instead of 27.
	!
	!	09/11/89 - Kevin Handy
	!		Continuing modifications of 08/29/89.
	!		Shorteden menu of unnecessary options.  Delete
	!		kit directory when finished. Delete kitinstal.?
	!		files as they are used. Fix various major/minor
	!		bugs found during debugging process.
	!
	!	09/12/89 - Kevin Handy
	!		More modifications. Broke routine to look up files
	!		into seperate subroutine (to prepare for installing
	!		data files).  Sorted list of systems to install
	!		as well as files to install.
	!
	!	09/23/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	10/16/89 - Kevin Handy
	!		Modified to use CMC$ROOT[UTL] instead of CMC:
	!		because one is defined in the install, and the
	!		other isn't.
	!
	!	08/13/90 - Kevin Handy
	!		Modified so that two systems that share the same
	!		base name (like AR_SYSTEM and AR_000001_SYSTEM)
	!		will be installed as one saveset.  This fixes
	!		problems that the install has with duplicated
	!		names/files.
	!
	!	08/13/90 - Kevin Handy
	!		Shortened record structure by merging PRONAM and
	!		CPYNAM into same variable, since they were always
	!		set to the same name anyway.
	!
	!	08/14/90 - Frank F. Starman
	!		Added more line into the CMC Banner file.
	!
	!	08/16/90 - Kevin Handy
	!		Added ability to send test data along for the ride.
	!
	!	09/05/90 - Frank F. Starman
	!		Correct variable name for help library.
	!
	!	10/02/90 - Frank F. Starman
	!		Copy to the save set all possible help file based
	!		on the program names.
	!
	!	05/28/91 - Frank F. Starman
	!		Look for release notes and copy them on the tape.
	!
	!	06/11/91 - Frank F. Starman
	!		Add "N" for spkitbld in the VAX/VMS version 5.4.
	!
	!	06/18/91 - Frank F. Starman
	!		Add version line into banner line.
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ op open statements.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Caused a TRM$() whenever VERSION$ is used.
	!
	!	04/12/95 - Kevin Handy
	!		Change VERSION$ from 3.5 to 3.6
	!		Change SCOPE::SCOPE_EXIT to scope::scope_exit.
	!
	!	04/15/95 - Kevin Handy
	!		Fix last param to entr_3choices.
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	10/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:SS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!**********************************************************************
	! Set up data storage areas, and define external modules needed
	!**********************************************************************
	!
	! Record structure to hold device and program information
	!
	RECORD PRG_REC
		STRING	PRODEV = 39
		STRING	PRONAM = 39
	END RECORD

	!
	! CDD inclusions and related MAPs
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE			SMG_SCROLL_CDD		SYSTEMS_INSTALL

	%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.HB"
	MAP	(SS_LICENSE)	SS_LICENSE_CDD		SS_LICENSE

	%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.HB"
	MAP	(SS_CUS_SYSMENU) SS_CUS_SYSMENU_CDD	SS_CUS_SYSMENU

	!
	! More memory MAPs
	!
	MAP (CH_SS_CUS_SYSMENU) &
		SS_CUS_SYSMENU.CH%

	MAP (CH_SS_LICENSE) &
		SS_LICENSE.CH%

	!
	! Declare variables and/or constants
	!
	DECLARE	LONG	CONSTANT	MAX_PROG = 1000%
	DECLARE	PRG_REC			PROG_LIST(MAX_PROG), TEMP_LIST
	DECLARE	LONG			SYS_STAT
	DECLARE	LONG			SMG_INSTALL
	DECLARE	STRING			SYS_MENU(200%)
	DECLARE	STRING			SYS_INSTALL(200%)
	DECLARE	STRING			SAMPLE_FILES(2000%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION DSPL_SCROLL(SMG_SCROLL_CDD, STRING DIM (), &
		LONG, STRING)
	EXTERNAL LONG	FUNCTION	EDT$EDIT
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG			READ_3BROADCAST

	%PAGE

	!**********************************************************************
	! Initialize variables
	!**********************************************************************
	!
	! Copyright and License Banner for CMC software
	!

	!
	! Release notes
	!
	REL_NOTE_PREFIX$ = "CMC$ROOT:[UTL]"
	REL_NOTE_SUFFIX$ = ".RELEASE_NOTES"

	!
	! Install notes
	!
	INS_NOTE_PREFIX$ = "CMC$ROOT:[UTL]"
	INS_NOTE_SUFFIX$ = ".INSTALL_NOTES"

	!
	! Set up the version number and the tape device
	!
	VERSION$ = "036"
	TAPE_DEV$ = "MKA500:"

	!
	! Other assignments
	!
	SAVE_SET_LOOP% = 2%
	!NETCHECKSIZE = 18000
	!SAFETYSIZE = 20000
	!KITINSTAL$ = "CMC:KIT_INSTAL.TEMPLATE"
	SPKITBLD$ = "sys$update:spkitbld.com"
	SYS_DEV$ = "$DISK1:[KITS.A]"
	SYS_MENU_NAME$ = "CMC$ROOT:[UTL]*.MNU"
	SYS_MENU_PREFIX$ = "CMC$ROOT:[UTL]"
	SYS_MENU_SUFFIX$ = ".MNU"

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	%PAGE

	!**********************************************************************
	! Do anything else before we start
	!**********************************************************************
	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

	!**********************************************************************
	! Assign channel numbers, and open all the files
	!**********************************************************************
	!
	! Assign channels
	!
	CALL ASSG_CHANNEL(MENU.CH%, STAT%)
	CALL ASSG_CHANNEL(MOVE_COM.CH%, STAT%)
	CALL ASSG_CHANNEL(KIT_COM.CH%, STAT%)
	CALL ASSG_CHANNEL(KITINSTAL.CH%, STAT%)
	CALL ASSG_CHANNEL(KITINSTAL_A.CH%, STAT%)
	CALL ASSG_CHANNEL(BLD_COM.CH%, STAT%)
	CALL ASSG_CHANNEL(TEMPLATE.CH%, STAT%)
	CALL ASSG_CHANNEL(BANNER.CH%, STAT%)

	!
	! Open up system menu file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.CRE"
	USE
		FILENAME$ = "SS_CUS_SYSMENU"
		CONTINUE HelpError
	END WHEN

	!
	! Open up product license file
	!
330	%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.CRE"

	%PAGE

	!**********************************************************************
	! Draw the screen, and set up its parameters
	!**********************************************************************
	!
	! Create the data display
	!
500	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, SMG_INSTALL%)
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_INSTALL%, " I N S T A L L ", &
		SMG$K_TOP, , SMG$M_BOLD)

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_INSTALL%, &
		SCOPE::SMG_PBID, 2%, 2%)
	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_INSTALL%, 1%, 1%)

	!
	! Define scroll region for systems to be installed
	!
	SYSTEMS_INSTALL::WINDOW		= SMG_INSTALL%
	SYSTEMS_INSTALL::TOP_ARRAY	= 1%
	SYSTEMS_INSTALL::BOT_ARRAY	= 0%
	SYSTEMS_INSTALL::SCROLL_TOP	= 1%
	SYSTEMS_INSTALL::SCROLL_BOT	= 18%
	SYSTEMS_INSTALL::BEG_ELEMENT	= 1%
	SYSTEMS_INSTALL::END_ELEMENT	= 0%
	SYSTEMS_INSTALL::TOP_LINE	= 1
	SYSTEMS_INSTALL::CUR_LINE	= 1
	SYSTEMS_INSTALL::CUR_W_ROW	= 1%
	SYSTEMS_INSTALL::CUR_W_COL	= 1%
	SYSTEMS_INSTALL::FIND_LINE	= 1%
	SYSTEMS_INSTALL::SMG_FLAG	= 0%
	SYSTEMS_INSTALL::PROMPT		= ""
	SYSTEMS_INSTALL::VIDEO_COMP	= 0%
	SYSTEMS_INSTALL::CHARSET	= 0%
	SYSTEMS_INSTALL::DRAW_COLS	= ""

	%PAGE

	!**********************************************************************
	! |
	!**********************************************************************
	!
	! Look up all menu files
	!
	CALL FIND_FILE(SYS_MENU_NAME$, SYS_MENU(), 16%, "", "")

	!
	! Initialize pointer for last selected option
	!
	OPT% = 0%

	!
	! Define list of options
	!
	OPT$ = "Select_saveset Testdata Build_tape Clear " + &
		"Release_note Install_note eXit"

 Menu:
	!
	! Enter desired option
	!
	SCOPE::PRG_ITEM = ""
	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT%, OPTFLAG%)

	SYS_STAT = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Ignore <CTRL><C>
	!
	CASE SMG$K_TRM_CTRLC
		GOTO Menu

	!
	! Exit Keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, 69%, 70%, 87%, &
		SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu

	END SELECT

 SelectOption:
	!
	! Decide what to do with the option
	!
	SELECT OPTION$
	!
	! Clear selections
	!
	CASE "C"
		SYS_INSTALL(WORK%) = "" &
			FOR WORK% = 1% TO SYSTEMS_INSTALL::BOT_ARRAY
		SYSTEMS_INSTALL::TOP_ARRAY	= 1%
		SYSTEMS_INSTALL::BOT_ARRAY	= 0%
		SYSTEMS_INSTALL::SCROLL_TOP	= 1%
		SYSTEMS_INSTALL::SCROLL_BOT	= 18%
		SYSTEMS_INSTALL::BEG_ELEMENT	= 1%
		SYSTEMS_INSTALL::END_ELEMENT	= 0%
		SYSTEMS_INSTALL::TOP_LINE	= 1
		SYSTEMS_INSTALL::CUR_LINE	= 1
		SYSTEMS_INSTALL::CUR_W_ROW	= 1%
		SYSTEMS_INSTALL::CUR_W_COL	= 1%
		SYSTEMS_INSTALL::FIND_LINE	= 1%

		TEMP = DSPL_SCROLL(SYSTEMS_INSTALL, SYS_INSTALL(), 0%, "PAINT")

		SAVE_SET_LOOP% = 2%

	!
	! Select save set
	!
	CASE "S"
 SelectSaveSet:
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"System for save set: ", 1%, 1%)

		SYSTEMNAME$ = SPACE$(20%)

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, 23%, &
			SYSTEMNAME$, -1%, 16% + 4096%)

		!
		! Exit from <Select> option
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! <List Choices> - List system menus
		!
		CASE SMG$K_TRM_F14
			SYS_MENU(LOOP%) = "" FOR LOOP% = 1% TO 200%

			CALL FIND_FILE(SYS_MENU_NAME$, SYS_MENU(), 16%, "", "")

			X% = ENTR_3CHOICE(SCOPE, "", "", SYS_MENU(), &
				"", 2% + 8% + 128%, &
				"  MENU SYSTEMS  ", "", 0%)

			IF (X% > 0%)
			THEN
				SYSTEMNAME$ = SYS_MENU(X%)
			ELSE
				CALL ENTR_3MESSAGE(SCOPE, "No system selected - save set aborted", 0%)
				GOTO SelectSaveSet
			END IF

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO SelectSaveSet

		END SELECT

		!
		! Get the input into usable form (exit option if no system)
		!
		SYSTEMNAME$ = EDIT$(SYSTEMNAME$, -1%)
		GOTO Menu IF (SYSTEMNAME$ = "")

		!
		! Test for duplicate system name
		!
		TEST% = 0%
		TEST% = -1% IF (SYSTEMNAME$ = EDIT$(SYS_INSTALL(WORK%), -1%)) &
			FOR WORK% = 1% TO SYSTEMS_INSTALL::BOT_ARRAY

		IF TEST%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Duplicate system", 0%)
			GOTO SelectSaveSet
		END IF

		!
		! Is this a valid system?
		!
		TEST% = -1%
		TEST% = 0% IF SYSTEMNAME$ = EDIT$(SYS_MENU(WORK%), -1%) &
			FOR WORK% = 1% TO VAL%(SYS_MENU(0%))

		IF TEST%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Invalid menu", 0%)
			GOTO SelectSaveSet
		END IF

		!
		! Add to save set list
		!
		SYSTEMS_INSTALL::BOT_ARRAY, SYSTEMS_INSTALL::END_ELEMENT = &
			SYSTEMS_INSTALL::BOT_ARRAY + 1%

		SYS_INSTALL(SYSTEMS_INSTALL::BOT_ARRAY) = SYSTEMNAME$

		!
		! Sort the list of systems to be installed
		!
		FOR LOOP1% = 1% TO SYSTEMS_INSTALL::BOT_ARRAY

			FOR LOOP2% = 1% TO SYSTEMS_INSTALL::BOT_ARRAY - LOOP1%

				IF SYS_INSTALL(LOOP2%) > SYS_INSTALL(LOOP2% + 1%)
				THEN
					TEMP$ = SYS_INSTALL(LOOP2%)
					SYS_INSTALL(LOOP2%) = SYS_INSTALL(LOOP2% + 1%)
					SYS_INSTALL(LOOP2% + 1%) = TEMP$
				END IF

			NEXT LOOP2%

		NEXT LOOP1%

		TEMP = DSPL_SCROLL(SYSTEMS_INSTALL, SYS_INSTALL(), 0%, "PAINT")

		SAVE_SET_LOOP% = SAVE_SET_LOOP% + 1%

		GOTO SelectSaveSet

	!
	! Select Testdata.
	!
	CASE "T"
		SICDATA% = 0%
 SelectTestdata:
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"SIC Code: ", 1%, 1%)

		TESTDATA1$ = SPACE$(30%)
		LSET TESTDATA1$ = TESTDATA$

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 12%, TESTDATA1$, -1%, 16% + 4096%)

		!
		! Exit from <Select> option
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO SelectTestdata

		END SELECT

		TESTDATA$ = TRM$(TESTDATA1$)
		IF TESTDATA$ = ""
		THEN
			GOTO Menu
		ELSE
			SICDATA% = SICDATA% + 1%
			SICDATA$(SICDATA%) = TESTDATA$
			TESTDATA$ = ""
			GOTO SelectTestdata
		END IF

	!
	! Put KITINSTALL in save set A, then create a COM file
	! to execute the SYS$UPDATE:SPKITBLD.COM file
	!
	CASE "B"
		GOSUB BuildTape
		GOTO ExitProgram

	!
	! Maintain the Release Note file
	!
	CASE "R"
 ReleaseNote:
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"Release Notes: ", 1%, 1%)

		SCOPE::PRG_ITEM = "RELNOTES"
	!++
	! Abstract:RELNOTES
	!
	!	^*Release Notes\*
	!
	! Index:
	!--
		RNOTES$ = "APLUS" + TRM$(VERSION$)
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 16%, RNOTES$, -1%, 16% + 4096%)
		!
		! Exit from <Release Note> option
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO ReleaseNote

		END SELECT

		!
		! Set up the version number
		!
		!
		! Set up the version number
		!
		VERSION$ = RIGHT(RNOTES$, LEN(RNOTES$) - 2%) &
			IF (RNOTES$ <> "")

		SYS_STAT = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = EDT$EDIT(REL_NOTE_PREFIX$ + RNOTES$ + &
			REL_NOTE_SUFFIX$,,,,,,,)
		SYS_STAT = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"ERROR in edit!!!! " + &
				NUM1$(SMG_STATUS%), 0%)
			GOTO ExitProgram
		END IF

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Maintain Install instructions
	!
	CASE "I"
 InstallNote:
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"Instalation Notes: ", 1%, 1%)

		SCOPE::PRG_ITEM = "INSNOTES"
	!++
	! Abstract:INSNOTES
	!
	!	^*Instalation Notes\*
	!
	! Index:
	!--
		INOTES$ = "APLUS" + TRM$(VERSION$)
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 20%, INOTES$, -1%, 16% + 4096%)

		!
		! Exit from <Install instructions> option
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO InstallNote

		END SELECT

		!
		! Set up the version number
		!
		VERSION$ = RIGHT(INOTES$, LEN(INOTES$) - 2%) &
			IF (INOTES$ <> "")

		SYS_STAT = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = EDT$EDIT(INS_NOTE_PREFIX$ + INOTES$ + &
			INS_NOTE_SUFFIX$,,,,,,,)
		SYS_STAT = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"ERROR in edit!!!! " + &
				NUM1$(SMG_STATUS%), 0%)
			GOTO ExitProgram
		END IF

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Exit the program
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO Menu

	%PAGE

1000	!*******************************************************************
	! Create the command file to do the actual creation of the
	! save sets on the tape
	!*******************************************************************

 BuildTape:
	!
	! Check to see if there are any save sets
	!
	IF SAVE_SET_LOOP% < 3%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No save set exist", 0%)
		RETURN
	END IF

	GOSUB License
	RETURN IF ABORT%

	SNUMBER% = FUNC_BANNERCODE(EDATE$ + LNUMBER$)

	!
	! Search for tape number
	!
1010	TAPE% = 0%
	WHEN ERROR IN
		FIND #SS_CUS_SYSMENU.CH%, KEY #0% EQ CUSTOM_NUM$, REGARDLESS
	USE
		CONTINUE 1050 IF ERR = 155%
		CONTINUE HelpError
	END WHEN

1020	WHEN ERROR IN
		GET #SS_CUS_SYSMENU.CH%, REGARDLESS
	USE
		CONTINUE 1050 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	GOTO 1050 IF SS_CUS_SYSMENU::CUSNUM <> CUSTOM_NUM$
	TAPE% = VAL%(SS_CUS_SYSMENU::TAPE)

	GOTO 1020

1050	TAPE% = TAPE% + 1%
	!
	! Create main command file
	!
	OPEN "BLD.COM" FOR OUTPUT AS FILE BLD_COM.CH%, &
		RECORDSIZE 132%

	OPEN "CMC:BASE_COM.TEMPLATE" FOR INPUT AS FILE TEMPLATE.CH%, &
		ACCESS READ, ALLOW MODIFY

	!
	! Create the BASE save set (Assume most commands will be in
	! the supplied COM file)
	!
1100	WHEN ERROR IN
		LINPUT #TEMPLATE.CH%, INLINE$
	USE
		CONTINUE 1130 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	IF INLINE$ = "~LICENSE"
	THEN
		PRINT #BLD_COM.CH%, "$ COPY SYS$INPUT " + &
			SYS_DEV$ + "BANNER.CMC"
		PRINT #BLD_COM.CH%, "! "
		PRINT #BLD_COM.CH%, "! License number"
		PRINT #BLD_COM.CH%, "! "
		PRINT #BLD_COM.CH%, "License number          :" + LNUMBER$
		PRINT #BLD_COM.CH%, "Expiration date         :" + EDATE$
		PRINT #BLD_COM.CH%, "Controlling number      :" + NUM1$(SNUMBER%)
		PRINT #BLD_COM.CH%, "Company name            :" + AR_35CUSTOM_EXAM::CUSNAM
		PRINT #BLD_COM.CH%, "Version                 :" + TRM$(VERSION$)
		PRINT #BLD_COM.CH%, "! "
		PRINT #BLD_COM.CH%, "! Tape number"
		PRINT #BLD_COM.CH%, "! "
		PRINT #BLD_COM.CH%, "Tape number             :" + &
			FORMAT$(TAPE%, "<0>#")
		PRINT #BLD_COM.CH%, "Creation date and time  :" + &
			DATE_TODAY + " " + &
						TIME_NOW
		PRINT #BLD_COM.CH%, "Version                 :" + TRM$(VERSION$)

	ELSE
		PRINT #BLD_COM.CH%, INLINE$
	END IF

	GOTO 1100

1130	CLOSE #TEMPLATE.CH%

	!
	! Ask for the current version number
	!
 AskVersion:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		"Version number <" + TRM$(VERSION$) + "> ", 1%, 1%)

	!
	! Test scope exit
	!
	JUNK$ = SPACE$(6%)
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(VERSION$) + 20%, &
		JUNK$, -1%, 16% + 4096%)

	!
	! Exit <Build Tape> option
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskVersion

	END SELECT

	VERSION$ = JUNK$ IF (JUNK$ <> "")

 AskTape:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		"Tape device <" + TAPE_DEV$ + "> ", 1%, 1%)

	JUNK$ = SPACE$(20%)

	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(TAPE_DEV$) + 17%, &
		JUNK$, -1%, 16% + 4096%)

	!
	! Exit from <Build Tape> option
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskTape

	END SELECT

	TAPE_DEV$ = JUNK$ IF (JUNK$ <> "")

	!******************************************************
	! Now if there are new licenses in this kit then
	! gosub assign next license number.
	!******************************************************
	!
	! Subtract 1 from save set loop
	!
	SAVE_SET_LOOP% = SAVE_SET_LOOP% - 1%

	CALL ENTR_3MESSAGE(SCOPE, "Create kit com files", 1%)

1150	!
	! Move data to work area
	!
 ! Modified to fake for/next loop so that we can look at next
 ! item to be installed to see if it should be installed in the same
 ! kit.
 !
 !	FOR WORK% = 1% TO SYSTEMS_INSTALL::BOT_ARRAY
	WORK% = 0%

 WorkLoop:
		WORK% = WORK% + 1%


		TOTAL_FILE% = 0%
		SAVE_SET$ = CHR$(WORK% + A"A"B)
		SYSTEMNAME$ = EDIT$(SYS_INSTALL(WORK%), -1%)
		I% = INSTR(1%, SYSTEMNAME$, "_")
		SYSTEM$ = LEFT(SYSTEMNAME$, I% - 1%)
		IF I%
		THEN
			KIT_NAME$ = "CMC" + SYSTEM$ + TRM$(VERSION$)
		ELSE
			KIT_NAME$ = "CMC" + SYSTEMNAME$ + TRM$(VERSION$)
		END IF

		!
		! Start this kit
		!
		PRINT #BLD_COM.CH%, "$!"
		PRINT #BLD_COM.CH%, '$ WRITE SYS$OUTPUT "Starting ' + &
			KIT_NAME$ + '"'
		PRINT #BLD_COM.CH%, "$ DELETE " + SYS_DEV$ + "*.*;*"
		PRINT #BLD_COM.CH%, "$ COPY KITINSTAL." + SAVE_SET$ + &
			" " + SYS_DEV$ + "KITINSTAL.COM"
		PRINT #BLD_COM.CH%, "$ DELETE KITINSTAL." + SAVE_SET$ + ";*"

		!
		! Open kitinstall output file for this kit
		!
		WHEN ERROR IN
			OPEN "KITINSTAL." + SAVE_SET$ &
				FOR OUTPUT AS FILE KIT_COM.CH%, &
				RECORDSIZE 132%

		USE
			FILENAME$ = "MENU"
			CONTINUE HelpError
		END WHEN

		!
		! Look up files to install
		!
 LookupNextMenu:
		GOSUB LookupMenu

		!
		! See if next saveset should be installed in with this
		! saveset.
		!
		IF WORK% < SYSTEMS_INSTALL::BOT_ARRAY
		THEN
			SYSTEMNAME1$ = EDIT$(SYS_INSTALL(WORK% + 1%), -1%)
			I% = INSTR(1%, SYSTEMNAME1$, "_")
			IF I%
			THEN
				KIT1_NAME$ = "CMC" + &
					LEFT(SYSTEMNAME1$, I% - 1%) + &
					TRM$(VERSION$)
			ELSE
				KIT1_NAME$ = "CMC" + SYSTEMNAME1$ + &
					TRM$(VERSION$)
			END IF

			IF KIT_NAME$ = KIT1_NAME$
			THEN
				SYSTEMNAME$ = SYSTEMNAME1$
				WORK% = WORK% + 1%
				GOTO LookupNextMenu
			END IF
		END IF

		!
		! Sort the list of systems to be installed
		!
		FOR LOOP1% = 1% TO TOTAL_FILE%

			FOR LOOP2% = 1% TO TOTAL_FILE% - LOOP1%

				IF PROG_LIST(LOOP2%)::PRONAM > &
					PROG_LIST(LOOP2% + 1%)::PRONAM
				THEN
					TEMP_LIST = PROG_LIST(LOOP2%)
					PROG_LIST(LOOP2%) = &
						PROG_LIST(LOOP2% + 1%)
					PROG_LIST(LOOP2% + 1%) = TEMP_LIST
				END IF

			NEXT LOOP2%

		NEXT LOOP1%

		!
		! Create copy com file
		!
		FOR LOOP% = 1% TO TOTAL_FILE%

			IF FIND_FILEEXISTS(TRM$(PROG_LIST(LOOP%)::PRODEV) + &
				TRM$(PROG_LIST(LOOP%)::PRONAM), 0%)
			THEN
				TEXT$ = "$ COPY " + &
					TRM$(PROG_LIST(LOOP%)::PRODEV) + &
					TRM$(PROG_LIST(LOOP%)::PRONAM) + &
					" " + SYS_DEV$ + &
					TRM$(PROG_LIST(LOOP%)::PRONAM)

				PRINT #BLD_COM.CH%, TEXT$
			ELSE
				IF INSTR(1%, PROG_LIST(LOOP%)::PRONAM, &
					"RELEASE_NOTES") = 0%
				THEN
					PROG_LIST(LOOP%)::PRODEV = ""
					PROG_LIST(LOOP%)::PRONAM = ""
				ELSE
					TEXT$ = "$ LIBRARY/TEXT" + &
						"/EXTRACT=H$" + SYSTEM$ + &
						"$" + KIT_NAME$ + &
						"/OUTPUT=" + TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						KIT_NAME$ + ".RELEASE_NOTES " + &
						"REF:HELP_" + SYSTEM$

					PRINT #BLD_COM.CH%, TEXT$

					TEXT$ = "$ RUNOFF" + &
						"/OUTPUT=" + TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						KIT_NAME$ + ".RELEASE_NOTES " + &
						TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						KIT_NAME$ + ".RELEASE_NOTES"

					PRINT #BLD_COM.CH%, TEXT$

					TEXT$ = "$ COPY " + &
						TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						TRM$(PROG_LIST(LOOP%)::PRONAM) + &
						" " + SYS_DEV$ + &
						TRM$(PROG_LIST(LOOP%)::PRONAM)

					PRINT #BLD_COM.CH%, TEXT$

				END IF
			END IF

		NEXT LOOP%

		PRINT #BLD_COM.CH%, "$ RENAME " + SYS_DEV$ + "*.*;* *.*;1"
		PRINT #BLD_COM.CH%, "$!"

		CLOSE MENU.CH%

2200		!
		! Create CMC_{SYSTEM}_KIT.COM file
		!
		OPEN "CMC:KITINSTAL.TEMPLATE" &
			FOR INPUT AS FILE TEMPLATE.CH%, &
			RECORDSIZE 132%, &
			ACCESS READ, ALLOW MODIFY

		FILE_COUNTER% = 0%

		PRINT #KIT_COM.CH%, "$!	CMC_" + TEMP_SYS_MENU$ + &
			"_KIT.COM  file"
		PRINT #KIT_COM.CH%, "$!"

2210		WHEN ERROR IN
			LINPUT #TEMPLATE.CH%, INLINE$
		USE
			CONTINUE 2220 IF ERR = 11%
			CONTINUE HelpError
		END WHEN

		IF INLINE$ = "~MOVE"
		THEN
			!
			! Create all directories needed
			!
			DIR_LIST$ = "CMC$ROOT:[UTL]~CMC$ROOT:[REF]~"
			FOR LOOP% = 1% TO TOTAL_FILE%
				!
				! While were at it, change CMC: to
				! CMC$ROOT:[UTL]
				!
				IF PROG_LIST(LOOP%)::PRODEV = "CMC:"
				THEN
					PROG_LIST(LOOP%)::PRODEV = "CMC$ROOT:[UTL]"
				END IF

				IF INSTR(1%, DIR_LIST$, &
					TRM$(PROG_LIST(LOOP%)::PRODEV) + "~") = 0%
				THEN
					PRINT #KIT_COM.CH%, &
						"$ VMI$CALLBACK CREATE_DIRECTORY USER " + &
						TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						' "/OWNER_UIC=[1,4]/PROT=(S:RWE,O:RWE,G:RWE,W:RE)"'

					DIR_LIST$ = DIR_LIST$ + &
						TRM$(PROG_LIST(LOOP%)::PRODEV) + &
						"~"
				END IF

			NEXT LOOP%

			!
			! Create copy com file
			!
			FOR LOOP% = 1% TO TOTAL_FILE%

				IF PROG_LIST(LOOP%)::PRONAM = ""
				THEN
					PRINT #KIT_COM.CH%, "$! Missing program"
				ELSE
					IF INSTR(1%, PROG_LIST(LOOP%)::PRONAM, ".EXE")
					THEN
						PRINT #KIT_COM.CH%, &
							"$ VMI$CALLBACK PROVIDE_IMAGE CMC$TEMP " + &
							TRM$(PROG_LIST(LOOP%)::PRONAM) + &
							" " + TRM$(PROG_LIST(LOOP%)::PRODEV)
					ELSE
						PRINT #KIT_COM.CH%, &
							"$ VMI$CALLBACK PROVIDE_FILE CMC$TEMP " + &
							TRM$(PROG_LIST(LOOP%)::PRONAM) + &
							" " + TRM$(PROG_LIST(LOOP%)::PRODEV)
					END IF
				END IF

				FILE_COUNTER% = FILE_COUNTER% + 1%

			NEXT LOOP%

			PRINT #KIT_COM.CH%, "$!"
			PRINT #KIT_COM.CH%, "$! Total of " + &
				NUM1$(FILE_COUNTER%) + " Files"
			PRINT #KIT_COM.CH%, "$!"
		ELSE
			PRINT #KIT_COM.CH%, INLINE$
		END IF

		GOTO 2210

2220		!
		! Set file exists flags to zero
		!
		CLOSE #KIT_COM.CH%

		!
		! Command to load onto tape
		!
		PRINT #BLD_COM.CH%, "$ @" + spkitbld$
		PRINT #BLD_COM.CH%, "N"
		PRINT #BLD_COM.CH%, " " + KIT_NAME$
		PRINT #BLD_COM.CH%, " " + TAPE_DEV$
		PRINT #BLD_COM.CH%, " Yes"
		PRINT #BLD_COM.CH%, " Yes"
		PRINT #BLD_COM.CH%, " " + SYS_DEV$ + "*.*"
		PRINT #BLD_COM.CH%, " No"

2300	GOTO WorkLoop IF WORK% < SYSTEMS_INSTALL::BOT_ARRAY

 !	NEXT WORK%

	Gosub BuildTestdata

	PRINT #BLD_COM.CH%, "$ DELETE " + SYS_DEV$ + "*.*;*"

	PRINT #BLD_COM.CH%, "$update:"
	TMPFIL$ = READ_SYSJOB + ".TMP"
	PRINT #BLD_COM.CH%, "$ open/write temp " + TMPFIL$

	SS_CUS_SYSMENU::CUSNUM	= CUSTOM_NUM$
	SS_CUS_SYSMENU::TAPE	= FORMAT$(TAPE%, "<0>#")
	SS_CUS_SYSMENU::INSDAT	= DATE_TODAY

	!
	! Put the records in temporary seq file
	!
	FOR LOOP% = 1% TO SAVE_SET_LOOP% - 1%
		SS_CUS_SYSMENU::SYSTEM	= MID(SYS_INSTALL(LOOP%), 1%, 2%)
		SS_CUS_SYSMENU::MENNUM	= &
			EDIT$(RIGHT(SYS_INSTALL(LOOP%), 4%), -1%)
		PRINT #BLD_COM.CH%, '$ write temp "' + &
			SS_CUS_SYSMENU::CUSNUM + &
			SS_CUS_SYSMENU::TAPE + &
			SS_CUS_SYSMENU::INSDAT + &
			SS_CUS_SYSMENU::SYSTEM + &
			SS_CUS_SYSMENU::MENNUM + '"'
	NEXT LOOP%

	PRINT #BLD_COM.CH%, "$ close temp"
	PRINT #BLD_COM.CH%, "$ convert/merge " + TMPFIL$ + " " + &
		SS_CUS_SYSMENU.DEV$ + "ss_cus_sysmenu.mas"
	PRINT #BLD_COM.CH%, "$ delete " + TMPFIL$ + ";*"

	PRINT #BLD_COM.CH%, "$exit:"

	PRINT #BLD_COM.CH%, "$ mount " + tape_dev$ + "/foreign"
	PRINT #BLD_COM.CH%, "$ dismount " + tape_dev$ + "/unload"
	PRINT #BLD_COM.CH%, "$ exit"

	CLOSE BLD_COM.CH%

	!
	! Submit job to create tape
	!
	CALL ENTR_3MESSAGE(SCOPE, "Submitting build com file", 1%)

	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)
	SMG_STATUS% = LIB$SPAWN("SUBMIT BLD")

	IF (SMG_STATUS% <> 1%) AND (SMG_STATUS% <> 0%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Error " + NUM1$(SMG_STATUS%) + " has occured", 0%)
	END IF

	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST), LOC(SCOPE))

	!
	! End of build process
	!

	RETURN

	%PAGE

2800	!*******************************************************************
	! Build test data onto tape
	!*******************************************************************
 BuildTestData:

	RETURN IF SICDATA% = 0%

	TOTAL_FILE% = 0%
	WORK% = SYSTEMS_INSTALL::BOT_ARRAY + 1%

	SAVE_SET$ = CHR$(WORK% + A"A"B)
	SYSTEMNAME$ = SICDATA$(SICDATA%)

	KIT_NAME$ = "DT" + SICDATA$(SICDATA%) + TRM$(VERSION$)

	!
	! Start this kit
	!
	PRINT #BLD_COM.CH%, "$!"
	PRINT #BLD_COM.CH%, '$ WRITE SYS$OUTPUT "Starting ' + KIT_NAME$ + '"'
	PRINT #BLD_COM.CH%, "$ DELETE " + SYS_DEV$ + "*.*;*"
	PRINT #BLD_COM.CH%, "$ COPY KITINSTAL." + SAVE_SET$ + &
		" " + SYS_DEV$ + "KITINSTAL.COM"
	PRINT #BLD_COM.CH%, "$ DELETE KITINSTAL." + SAVE_SET$ + ";*"

	!
	! Open kitinstall output file for this kit
	!
	OPEN "KITINSTAL." + SAVE_SET$ &
		FOR OUTPUT AS FILE KIT_COM.CH%, &
		RECORDSIZE 132%


	!
	! Look up files to install (Already should be sorted)
	!
	CALL FIND_FILE("[CMC030.SIC." + SICDATA$(SICDATA%) + "]*.*", &
		SAMPLE_FILES(), 16% + 32%, "", "")

	TESTDATA% = VAL%(SAMPLE_FILES(0%))
	COPYDATA% = 0%
	!
	! Create copy com file and ignore some files
	!
	FOR LOOP% = 1% TO TESTDATA%
		SKIP% = 0%
		SKIP% = INSTR(1%, SAMPLE_FILES(LOOP%), ".TXT")
		SKIP% = SKIP% + INSTR(1%, SAMPLE_FILES(LOOP%), ".TMP")
		SKIP% = SKIP% + INSTR(1%, SAMPLE_FILES(LOOP%), ".SPL")
		SKIP% = SKIP% + INSTR(1%, SAMPLE_FILES(LOOP%), "WINDOWS_")
		IF SKIP% = 0%
		THEN
			TEXT$ = "$ COPY " + &
				"[CMC030.SIC." + SICDATA$(SICDATA%) + "]" + &
				SAMPLE_FILES(LOOP%) + &
				" " + SYS_DEV$ + &
				SAMPLE_FILES(LOOP%)
			PRINT #BLD_COM.CH%, TEXT$
			COPYDATA% = COPYDATA% + 1%
			SAMPLE_FILES(COPYDATA%)= SAMPLE_FILES(LOOP%)
		END IF
	NEXT LOOP%

	TESTDATA% = COPYDATA%

	PRINT #BLD_COM.CH%, "$ RENAME " + SYS_DEV$ + "*.*;* *.*;1"
	PRINT #BLD_COM.CH%, "$!"

	CLOSE MENU.CH%

2805	!
	! Create CMC_{SYSTEM}_KIT.COM file
	!
	OPEN "CMC:TESTDATA_KITINSTAL.TEMPLATE" &
		FOR INPUT AS FILE TEMPLATE.CH%, &
		RECORDSIZE 132%, &
		ACCESS READ, ALLOW MODIFY

	FILE_COUNTER% = 0%

	PRINT #KIT_COM.CH%, "$!	CMC_TESTDATA_KIT.COM  file"
	PRINT #KIT_COM.CH%, "$!"

2810	WHEN ERROR IN
		LINPUT #TEMPLATE.CH%, INLINE$
	USE
		CONTINUE 2820 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	IF INLINE$ = "~MOVE"
	THEN
		!
		! Create copy com file
		!
		FOR LOOP% = 1% TO TESTDATA%

			PRINT #KIT_COM.CH%, &
				"$ VMI$CALLBACK PROVIDE_FILE CMC$TEMP " + &
				SAMPLE_FILES(LOOP%) + &
				" CMC$TESTDATA:"

			FILE_COUNTER% = FILE_COUNTER% + 1%

		NEXT LOOP%

		PRINT #KIT_COM.CH%, "$!"
		PRINT #KIT_COM.CH%, "$! Total of " + &
			NUM1$(FILE_COUNTER%) + " Files"
		PRINT #KIT_COM.CH%, "$!"
	ELSE
		PRINT #KIT_COM.CH%, INLINE$
	END IF

	GOTO 2810

2820	!
	! Set file exists flags to zero
	!
	CLOSE #KIT_COM.CH%

	!
	! Command to load onto tape
	!
	PRINT #BLD_COM.CH%, "$ @" + spkitbld$
	PRINT #BLD_COM.CH%, "N"
	PRINT #BLD_COM.CH%, " " + KIT_NAME$
	PRINT #BLD_COM.CH%, " " + TAPE_DEV$
	PRINT #BLD_COM.CH%, " Yes"
	PRINT #BLD_COM.CH%, " Yes"
	PRINT #BLD_COM.CH%, " " + SYS_DEV$ + "*.*"
	PRINT #BLD_COM.CH%, " No"

	RETURN

	%PAGE

3000	!*******************************************************************
	! Look up all files to be installed from a menu
	!*******************************************************************

 LookupMenu:

	!
	! Look for release notes
	!
	TOTAL_FILE% = TOTAL_FILE% + 1%
	PROG_LIST(TOTAL_FILE%)::PRODEV = "CMC:"
	PROG_LIST(TOTAL_FILE%)::PRONAM = KIT_NAME$ + ".RELEASE_NOTES"

	!
	! Open menu source file
	!
	CLOSE MENU.CH%

	OPEN SYS_MENU_PREFIX$ + SYSTEMNAME$ + SYS_MENU_SUFFIX$ &
		FOR INPUT AS FILE MENU.CH%, &
		RECORDSIZE 132%, &
		ACCESS READ, ALLOW MODIFY

	!
	! Get menu system name
	!
	X% = INSTR(1%, SYSTEMNAME$, "_")
	X% = LEN(SYSTEMNAME$) + 1% IF (X% = 0%)
	TEMP_SYS_MENU$ = LEFT(SYSTEMNAME$, X% - 1%)

	!
	! If menu system is UT the set to UTL
	!
	IF TEMP_SYS_MENU$ = "UT"
	THEN
		TEMP_SYS_MENU$ = "UTL"
	END IF

	GOTO GetNextRec

 GetNextRec:
3100	!
	! Get line
	!
	WHEN ERROR IN
		LINPUT #MENU.CH%, INLINE$
	USE
		CONTINUE 3200 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	IF LEFT(INLINE$, 1%) = "!"
	THEN
		IF LEFT(EDIT$(INLINE$, -1%), 6%) <> "!.FILE"
		THEN
			GOTO 3190
		ELSE
			MENU_TYPE$ = "P"
			MENU_FILE$ = EDIT$(RIGHT(INLINE$, 7%), 8%)
			!
			! Strip off extension if there is one
			!
			TEMP% = INSTR(1%, MENU_FILE$, ".")
			IF TEMP%
			THEN
				EXTENSION$ = RIGHT(MENU_FILE$, TEMP%)
				MENU_FILE$ = LEFT(MENU_FILE$, TEMP% - 1%)
			END IF
	ELSE
		!*******************************************************************
		! Parse one line
		!*******************************************************************
		!
		! Pull off dots
		!
		DOT_COUNTER% = 0%
		WHILE MID(INLINE$, DOT_COUNTER% + 1%, 1%) = "."
			DOT_COUNTER% = DOT_COUNTER% + 1%
		NEXT
		INLINE$ = RIGHT(INLINE$, DOT_COUNTER% + 1%)

		!
		! Pull apart line
		!
		FIRST_SPACE% = INSTR(1%, INLINE$, " ")

		FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, ">")
		FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, "<") &
			IF FIRST_ARROW% = 0%

		IF FIRST_ARROW% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to parse input line from menu '" + &
				INLINE$ + "'", 0%)
			GOTO 3190
		END IF

		SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, ">")
		SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, "<") &
			IF SECOND_ARROW% = 0%

		IF SECOND_ARROW% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to parse input line from menu '" + &
				INLINE$ + "'", 0%)
			GOTO 3190
		END IF

		!
		! Seperate parts
		!
		MENU_FILE$ = RIGHT(INLINE$, SECOND_ARROW% + 1%)
		IF MID(INLINE$, FIRST_ARROW%, 1%) = ">"
		THEN
			MENU_TYPE$ = "P"
		ELSE
			!
			! Skip help items since they are not programs
			!
			GOTO 3190
		END IF
		EXTENSION$ = ".EXE"
	END IF

	!
	! Look up device, etc.
	!
	CALL HELP_MENUHELPKEY(MENU_FILE$, &
		MENU_TYPE$, &
		"", &
		TLB_IDENT$, &
		TLB_PROGRAM$, &
		TLB_ITEM$, &
		TLB_DEVICE$, &
		1%)

	!
	! Add Extension to name
	!
	TLB_PROGRAM$ = EDIT$(TLB_PROGRAM$ + EXTENSION$, -1%)

	!
	! Search file list for currently existing file
	!
	GOTO 3190 IF PROG_LIST(I%)::PRONAM = TLB_PROGRAM$ &
		FOR I% = 1% TO TOTAL_FILE%

	!
	! Item not found, create it
	!
	I%, TOTAL_FILE% = TOTAL_FILE% + 1%

	WHILE (I% > 1%) AND (PROG_LIST(I% - 1%)::PRONAM > TLB_PROGRAM$)
		PROG_LIST(I%) = PROG_LIST(I% - 1%)
		I% = I% - 1%
	NEXT

	PROG_LIST(I%)::PRODEV = TLB_DEVICE$
	PROG_LIST(I%)::PRONAM = TLB_PROGRAM$

3190	GOTO GetNextRec

	%PAGE

3200	!
	! Add Help files
	!
	FILE.IDX% = TOTAL_FILE%

	FOR HLP% = 1% TO FILE.IDX%
		PREF% = INSTR(1%, PROG_LIST(HLP%)::PRONAM + "_", "_")

		!FILE_NAME$ = "HELP_" + TEMP_SYS_MENU$ + ".TLB"
		FILE_NAME$ = "HELP_" + LEFT(PROG_LIST(HLP%)::PRONAM,PREF% - 1%) + ".TLB"

		GOTO NextFile IF PROG_LIST(I%)::PRONAM = FILE_NAME$ &
			FOR I% = 1% TO TOTAL_FILE%

		IF FIND_FILEEXISTS("REF:" + FILE_NAME$, 0%)
		THEN
			I%, TOTAL_FILE% = TOTAL_FILE% + 1%
			PROG_LIST(I%)::PRODEV = "CMC$ROOT:[REF]"
			PROG_LIST(I%)::PRONAM = FILE_NAME$
		END IF
 NextFile:
	NEXT HLP%

3210	!
	! Add Menu file
	!
	FILE_NAME$ = SYSTEMNAME$ + ".MNU"

	GOTO 3220 IF PROG_LIST(I%)::PRONAM = FILE_NAME$ &
		FOR I% = 1% TO TOTAL_FILE%

	IF FIND_FILEEXISTS("CMC$ROOT:[UTL]" + FILE_NAME$, 0%)
	THEN
		I%, TOTAL_FILE% = TOTAL_FILE% + 1%
		PROG_LIST(I%)::PRODEV = "CMC$ROOT:[UTL]"
		PROG_LIST(I%)::PRONAM = FILE_NAME$
	END IF

3220	!
	! Add Form file
	!
	FILE_NAME$ = TEMP_SYS_MENU$ + "_FORM.TLB"

	GOTO 3230 IF PROG_LIST(I%)::PRONAM = FILE_NAME$ &
		FOR I% = 1% TO TOTAL_FILE%

	IF FIND_FILEEXISTS("CMC$ROOT:[UTL]" + FILE_NAME$, 0%)
	THEN
		I%, TOTAL_FILE% = TOTAL_FILE% + 1%
		PROG_LIST(I%)::PRODEV = "CMC$ROOT:[UTL]"
		PROG_LIST(I%)::PRONAM = FILE_NAME$
	END IF

3230	!

	RETURN

	%PAGE

 ExitProgram:
	!*******************************************************************
	! Exit to next program or menu
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 License:
	!******************************************************************
	! Enter license agreement data
	!******************************************************************
	ABORT% = 0%

	!
	! Look up customer number
	!
	CUSTOM_NUM$ = ""

 CustomNum:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		"Customer Number:", 1%, 1%)

	CUSTOM_NUM$ = LEFT(CUSTOM_NUM$ + SPACE$(10%), 10%)

	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
		1%, 18%, CUSTOM_NUM$, -1%, 4096%)

	!
	! List Choices
	!
	CASE SMG$K_TRM_F14
		CUSTOM_NUM$ = AR_35CUSTOM::CUSNUM &
			IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%)

	!
	! Maintain customer file
	!
	CASE SMG$K_TRM_F17
		IF (CUSTOM_NUM$ <> "")
		THEN
			ST% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "M0" + CUSTOM_NUM$)
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Enter customer number to maintain", 0%)
		END IF

	!
	! Exit from this subroutine
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		ABORT% = -1%
		GOTO EndLicense

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

		GOTO CustomNum1

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

	END SELECT

	GOTO CustomNum

 CustomNum1:
	GOTO CustomNum3 IF AR_EXAM_CUSTOM(CUSTOM_NUM$, AR_35CUSTOM_EXAM) = &
		CMC$_NORMAL


 CustomNum2:
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Undefined Customer number - ADD to Customer file (Y/N) ?", &
		"N", 8%, "!", "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		CALL ENTR_3MESSAGE(SCOPE, "Undefined Customer Number", 1%)

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO CustomNum2

	END SELECT

	GOTO CustomNum IF (INP$ <> "Y")

	ST% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "M0")
	CUSTOM_NUM$ = AR_35CUSTOM::CUSNUM
	GOTO CustomNum1

	!
	! Valid customer number
	!
 CustomNum3:

	!
	! If customer found in license file then get it
	!
	LNUMBER$, EDATE$ = ""
	IF MAIN_WINDOW(SS_MAIN_LICENSE.ID, "Q0" + CUSTOM_NUM$) = 1%
	THEN
		LNUMBER$ = SS_LICENSE::LICENSE_NUM
		EDATE$ = SS_LICENSE::EXPIR_DATE
		EDATE$ = "00000000" IF TRM$(EDATE$)=""
	ELSE
		!
		! Get the license number
		!
		CALL ENTR_3MESSAGE(SCOPE, "Missing License Number ???", 1%)
		ST% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "S0" + CUSTOM_NUM$ + &
			"|" + "License")
		!CUSTOM_NUM$ = AR_35CUSTOM::CUSNUM
		GOTO CustomNum3
	END IF

 EndLicense:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!******************************************************************
	! Error Trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions (and related memory MAPs)
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM
	MAP	(AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD	AR_35CUSTOM_OLD
	MAP	(AR_35CUSTOM_ONE)	AR_35CUSTOM_CDD	AR_35CUSTOM_ONE

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_MAIN_CONTACT
	EXTERNAL LONG	FUNCTION	AR_MAIN_35CUSTOM
	EXTERNAL LONG	FUNCTION	SS_MAIN_CUS_SYSMENU
	EXTERNAL LONG	FUNCTION	SS_MAIN_LICENSE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		!
		! Modify the menu
		!
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " cOntact System License"

		!
		! Optional menu items
		!
		CASE OPT_MOREMENU
			AR_35CUSTOM_ONE = AR_35CUSTOM
			SELECT SCOPE::PRG_ITEM

			!
			! List of Contacts
			!
			CASE "cOntact"
				MAINT_GROUP = MAIN_JOURNAL( &
					AR_MAIN_CONTACT.ID, "")

			!
			! List of systems customer has bought
			!
			CASE "System"
				MAINT_GROUP = MAIN_WINDOW( &
					SS_MAIN_CUS_SYSMENU.ID, "")

			!
			! License maintenance
			!
			CASE "License"
				MAINT_GROUP = MAIN_WINDOW( &
					SS_MAIN_LICENSE.ID, "")

			END SELECT

		!
		! Handle finishing various options specially
		!
		CASE OPT_AFTEROPT

			SELECT SCOPE::PRG_ITEM
			!
			! Change records
			!
			CASE "Change", "Blank", "Initialize"
				!
				! Change line items to match new header
				! if the key was changed.
				!
				IF AR_35CUSTOM_OLD::CUSNUM <> AR_35CUSTOM::CUSNUM
				THEN
					AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
					MAINT_GROUP   = MAIN_WINDOW( &
						SS_MAIN_CUS_SYSMENU.ID, "C")
					MAINT_GROUP   = MAIN_JOURNAL( &
						AR_MAIN_CONTACT.ID, "C")
					MAINT_GROUP   = MAIN_WINDOW( &
						SS_MAIN_LICENSE.ID, "C")
				END IF

			!
			! Erase record
			!
			CASE "Erase"
				!
				! Erase any line items under the header
				!
				AR_35CUSTOM_ONE = AR_35CUSTOM_OLD
				MAINT_GROUP   = MAIN_WINDOW( &
					SS_MAIN_CUS_SYSMENU.ID, "E")
				MAINT_GROUP   = MAIN_JOURNAL( &
					AR_MAIN_CONTACT.ID, "E")
				MAINT_GROUP   = MAIN_WINDOW( &
					SS_MAIN_LICENSE.ID, "E")

			END SELECT

		END SELECT

	CASE AR_MAIN_CONTACT.ID
		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SS_MAIN_CUS_SYSMENU.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

		MAINT_GROUP = SS_MAIN_CUS_SYSMENU(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SS_MAIN_LICENSE.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = AR_35CUSTOM_ONE::CUSNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = AR_35CUSTOM::CUSNUM
			CASE ELSE
				MVALUE = AR_35CUSTOM_ONE::CUSNUM
			END SELECT
		END SELECT

		MAINT_GROUP = SS_MAIN_LICENSE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
