1	%TITLE "Print Documentation"
	%SBTTL "TK_SPEC_DOCPRINT"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!
	! Index:
	!
	! Option:
	!
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
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_DOCPRINT/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_DOCPRINT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_DOCPRINT.OBJ;*
	!
	! Author:
	!
	!	11/30/88 - Frank Starman
	!
	! Modification history:
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	06/05/90 - Frank F. Starman
	!		Change com file command and do not ask for index.
	!
	!	09/28/90 - Frank F. Starman
	!		Enable to print cover sheet for systems.
	!		Ask for number of copies.
	!
	!	07/16/91 - Frank F. Starman
	!		Initialize and delete sys$laser que because WP has some
	!		problem.
	!
	!	12/01/92 - Kevin Handy
	!		Modified so that it doesn't delete the laser printer
	!		que before quing the document to print.  (Don't ask
	!		me why anyone thought this would work)
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
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
	!	10/17/2000 - Kevin Handy
	!		Turn off lame error trapping
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

 !	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG		READ_3BROADCAST

	DECLARE LONG SYS_STATUS

	CALL READ_INITIALIZE

	SCOPE::PRG_ITEM = ""

	%PAGE

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%

	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Print Document on the Laser Printer", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

	!
	! Ask for name of command file
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Name of a Document to Print ", 5%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Number of Copies", 6%, 5%)

	COM_FILE$ = LEFT("XX_SYSTEM[_COVER]" + SPACE$(20%), 39%)
	COPIES% = 1%

40	SCOPE::PRG_ITEM = "FLD01DOC"

	!++
	! Abstract:FLD01DOC
	!--

	COM_FILE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"5;45", "Document Name", COM_FILE$, &
		16%, "'E", COM_FILE$), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! SMG$K_TRM_DOWN
	!
	CASE SMG$K_TRM_DOWN
		GOTO 45

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 40

	END SELECT

45	SCOPE::PRG_ITEM = "FLD02"

	!++
	! Abstract:FLD02
	!--

	COPIES% = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, &
		"6;45", "Number of Copies", COPIES% * 1.0, &
		16%, "##", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO 40

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 45

	END SELECT

50	OPEN COM_FILE$ + ".COM" FOR OUTPUT AS FILE COM_FILE.CH%, &
		RECORDSIZE 132%

	!
	! This is here just temporary (I hope) because WP has a problem
	!
	PRINT #COM_FILE.CH%, &
		"$ init/que/start/on=$okilaser/def=nofeed sys$laser"

	IF INSTR(1%, COM_FILE$, "_COVER") OR INSTR(1%, COM_FILE$, "_BROCHURE")
	THEN
		IF INSTR(1%, COM_FILE$, "_BROCHURE")
		THEN
			PRINT #COM_FILE.CH%, "$ latex ref:" + COM_FILE$ + ".TEX"
		ELSE
			PRINT #COM_FILE.CH%, "$ tex ref:" + COM_FILE$ + ".TEX"
		END IF

		PRINT #COM_FILE.CH%, "$ dvijep -x0.0in -y0.5in " + COM_FILE$
		PRINT #COM_FILE.CH%, &
			"$ print/notify/que=sys$laser/passall/copies=" + &
				NUM1$(COPIES%) + "/delete " + COM_FILE$ + &
				".DVI-JEP"
		PRINT #COM_FILE.CH%, "$ delete " + COM_FILE$ + ".COM;*"
		PRINT #COM_FILE.CH%, "$ delete " + COM_FILE$ + ".LIS;*"
		PRINT #COM_FILE.CH%, "$ delete " + COM_FILE$ + ".DVI;*"
		PRINT #COM_FILE.CH%, "$ purge " + COM_FILE$ + ".*"
	ELSE
		PRINT #COM_FILE.CH%, &
			"$ print/notify/que=sys$laser/passall/copies=" + &
				NUM1$(COPIES%) + " " + COM_FILE$ + ".DVI-JEP"
	END IF

	!
	! This is here just temporary (I hope) because WP has a problem
	!

 ExitProgram:

	CLOSE COM_FILE.CH%

	!
	! Submit command file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Submitting command file", 1%)

	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	SMG_STATUS% = LIB$SPAWN("SUBMIT " + COM_FILE$ + ".COM" + &
		"/NOPRINT/NOTIFY")

	IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Error " + NUM1$(SMG_STATUS%) + " has occured", 0%)
	END IF

	SLEEP 1%

	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

 ExitProgram1:
	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	GOTO 32767


19000	!======================================================================
	! Error Trapping
	!======================================================================

	!
	! Untrapped error
	!
 !	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
 !		LEFT(FORMAT$(ERR, "#### ") + &
 !		FORMAT$(ERL, "#### ") + &
 !		ERT$(ERR) + SPACE$(78%), 78%), &
 !		17%, 1%)
 !
 !	RESUME 32767

32767	END
