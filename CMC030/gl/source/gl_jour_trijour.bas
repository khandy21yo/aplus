1	%TITLE "Maintain Sales Journal"
	%SBTTL "GL_JOUR_TRIJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!
	! Index:
	!
	! Option:
	!	GL_MAIN_TRIJOUR$HELP
	!	GL_MAIN_TRIJOUR_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_JOUR_TRIJOUR/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_JOUR_TRIJOUR,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_JOUR_TRIJOUR.OBJ;*
	!
	! Author:
	!
	!	06/06/95 - Kevin Handy
	!		Based upon AR_JOUR_SJ.
	!
	! Modification history:
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_TRIJOUR.HB"
	MAP	(GL_TRIJOUR)	GL_TRIJOUR_CDD	GL_TRIJOUR

	%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.HB"
	MAP	(GL_TRICONTROL)	GL_TRICONTROL_CDD	GL_TRICONTROL

	!
	! Common statements
	!
	COM (TT_GL_TRIJOUR) &
		BATCH_NO$ = 2%

	COM (CH_GL_TRICONTROL) &
		GL_TRICONTROL.CH%, &
		GL_TRICONTROL.READONLY%

	!
	! Dimension statements
	!
	DIM GL_TRIJOUR_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

200	GL_TRICONTROL.READONLY% = 1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.OPN"
	USE
		FILENAME$ = "GL_TRICONTROL"
		CONTINUE HelpError
	END WHEN

300	!******************************************************************
	! Get batch number
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("GL_TRIJOUR", GL_TRIJOUR.DEV$, STAT%)

	!
	! Find out what is already there
	!
	CALL FIND_FILE(GL_TRIJOUR.DEV$ + "GL_TRIJOUR_*.JRL", &
		GL_TRIJOUR_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	!
	! If any files are in the list, then do a query screen
	!
	GL_TRIJOUR_FILE% = VAL%(GL_TRIJOUR_FILE$(0%))

	IF GL_TRIJOUR_FILE%
	THEN
		!
		! Get ONLY the batch number of the file
		!
		FOR LOOP% = 1% TO GL_TRIJOUR_FILE%
			TEMP$ = RIGHT(GL_TRIJOUR_FILE$(LOOP%), 12%)
			I% = INSTR(1%, TEMP$, ".")
			I% = LEN(TEMP$) + 1% IF I% = 0%
			GL_TRIJOUR_FILE$(LOOP%) = LEFT(TEMP$, I% - 1%)
		NEXT LOOP%

		!
		! Query the user
		!
		X% = ENTR_3CHOICE(SCOPE, "", "", GL_TRIJOUR_FILE$(), "", &
			0%, "Tri Spur Journal Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(GL_TRIJOUR_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key ?
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	!
	! Ask user for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

310	!
	! Print background
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Tri Spur Journal Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch:   " + BATCH_NO$, 12%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for batch number
	!
	BATCH_NO$ = ""
	SCOPE::PRG_ITEM = "FLD01BATCH"
	!++
	! Abstract:FLD01BATCH
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		12%, 39%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the BATCH number in XX format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_TRIJOUR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_TRIJOUR
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_TRIJOUR.ID
		MAINT_GROUP = GL_MAIN_TRIJOUR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

	END FUNCTION
