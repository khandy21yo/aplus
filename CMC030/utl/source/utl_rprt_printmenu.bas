1	%TITLE "Print Menu Structure"
	%SBTTL "UTL_RPRT_PRINTMENU"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:UT001
	!
	! Abstract:HELP
	!	.p
	!	The ^*System Menu\* option prints
	!	a report which contains the menu for a selected
	!	system.
	!
	! Index:
	!	.x System Menus>Report
	!	.x Report>System Menus
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_PRINTMENU/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_PRINTMENU, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_PRINTMENU.OBJ;*
	!
	! AUTHOR:
	!
	!	08/03/87 - Frantisek Starman
	!
	! MODIFICATION HISTORY:
	!
	!	08/01/88 - Kevin Handy
	!		Modified to watch for exit.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*SYSTEM/LIST\*
	!	.p
	!	^*System/List\* prints a report which contains
	!	the menu for a selected system.
	!	.p
	!	^*Format: SYSTEM/LIST\*
	!	.p
	!	^*Example\*
	!	.literal
	!	Menu Command Level> /SYSTEM/LIST
	!	.end literal
	!
	! Index:
	!	.x SYSTEM/LIST
	!	.x System Menus>Report
	!	.x Report>System Menus
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! Dimension statements
	!
	DIM SYS_MENU_FILE$(10000%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assign channels
	!
	CALL ASSG_CHANNEL(SYS_MENU.CH%, STAT%)

	SYS_MENU.DEV$ = "CMC:"

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	MENU.NAME$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) System Name\*
	!	.p
	!	The ^*System Name\* option
	!	enters the System name for which
	!	a menu report is to be printed.
	!	.p
	!	The field will accommodate twenty (20) characters.
	!
	! Index:
	!	.x System>Name
	!	.x Name>System
	!
	!--


	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! Do a directory of all menu files
	!
	CALL FIND_FILE(SYS_MENU.DEV$ + "*.MNU", SYS_MENU_FILE$(), &
		16%, "", "")

	SYS_MENU_FILE% = VAL%(SYS_MENU_FILE$(0%))

	!
	! Loop through them all
	!
	FOR LOOP% = 1% TO SYS_MENU_FILE%

		IF COMP_STRING(SYS_MENU_FILE$(LOOP%), MENU.NAME$) <> 0% &
			OR MENU.NAME$ = ""
		THEN
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 1%) &
				IF MENU% <> 0%
			GOSUB 18000
			MENU% = 1%
		END IF

	NEXT LOOP%

 ExitTotal:
17400	!
	! Handle end of report
	!


 ExitProgram:

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

18000	!***************************************************************
	! Print out one menu
	!***************************************************************

	CLOSE SYS_MENU.CH%

	WHEN ERROR IN
		OPEN SYS_MENU.DEV$ + SYS_MENU_FILE$(LOOP%) + ".MNU" FOR INPUT AS &
			FILE SYS_MENU.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 18050 IF ERR = 5%

		IF ERR = 138% ! Locked file
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "SYS_MENU"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	! Main loop starts here
	!
	! Get next record
	!
	WHEN ERROR IN
		INPUT LINE #SYS_MENU.CH%, LIN$
	USE
		CONTINUE 18050 IF ERR = 11% ! End of file

		FILENAME$ = "SYS_MENU"
		CONTINUE HelpError
	END WHEN

	LEN% = INSTR(1%, LIN$, "H<")
	LEN% = INSTR(1%, LIN$, "P>") IF LEN% = 0%

	MEZ% = 0%
	MEZ% = 1% IF LEFT(LIN$, 1%) = "."
	MEZ% = 2% IF LEFT(LIN$, 2%) = ".."
	MEZ% = 3% IF LEFT(LIN$, 3%) = "..."
	MEZ% = 4% IF LEFT(LIN$, 4%) = "...."
	MEZ% = 5% IF LEFT(LIN$, 5%) = "....."

18010	!
	! Print out one line
	!
	TEXT$ = MID(LIN$, MEZ% + 1%, LEN% - 1% - MEZ%)
	SPA% = INSTR(1%, TEXT$, " ")

	TEXT$ = SPACE$(MEZ% * 15%) + &
		LEFT(TEXT$, SPA%) + &
		SPACE$(7% - SPA%) + &
		EDIT$(RIGHT(TEXT$, SPA%), 8%)

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		IF MEZ% < 2%

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

18020	!
	! Try for next record
	!
	GOTO GetNextRec

18050	RETURN

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
