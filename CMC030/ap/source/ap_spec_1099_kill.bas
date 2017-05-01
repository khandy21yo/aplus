1	%TITLE "Remove 1099 History File"
	%SBTTL "AP_SPEC_1099_KILL"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	This program deletes 1099 history files when they are
	!	no longer useful.
	!	.note
	!	After removing the file, the file is completely gone.
	!	If you need it again, you will either need to restore
	!	it from a backup, or recreate it.
	!	.end note
	!
	! Index:
	!	.X Remove>1099
	!	.x 1099>Remove
	!
	! Option:
	!
	!	AP_SPEC_1099_KILL$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_1099_KILL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_SPEC_1099_KILL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_1099_KILL.OBJ;*
	!
	! Author:
	!
	!	10/22/86 - Robert Peterson
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 format standards.
	!		Change last parameter on ENTR_3CHOICE from "" to 0%.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Don't bother erasing SMG_SCREEN_DATA%, which is
	!		never created.
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	DIM AP_1099_YYYY_FILE$(100)

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_1099_YYYY", AP_1099_YYYY.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE( AP_1099_YYYY.DEV$ + "AP_1099_*.HIS", &
		AP_1099_YYYY_FILE$(), 16%, "", "")

	AP_1099_YYYY_FILE% = VAL%(AP_1099_YYYY_FILE$(0%))

	IF AP_1099_YYYY_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"There are no 1099 history files to remove!", 0%)
		GOTO ExitProgram
	END IF

	IF AP_1099_YYYY_FILE%
	THEN
		AP_1099_YYYY_FILE$(LOOP%) = &
			MID(AP_1099_YYYY_FILE$(LOOP%), 9%, 4%) &
				FOR LOOP% = 1% TO AP_1099_YYYY_FILE%

		TEMP$ = "AP 1099 Register"

		X% = ENTR_3CHOICE(SCOPE, "", "", AP_1099_YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YEAR_1099$ = EDIT$(AP_1099_YYYY_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
			GOTO ExitProgram

	END SELECT

	GOTO 300

400	!
	! Confirm killing file
	!
	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	^*Confirm\* asks for user confirmation as to the deletion of 1099 History
	!	files when they are no longer needed.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Remove 1099 History Files
	!	.x Remove 1099 History Files>Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
		"", "Confirm removing History " + &
		" - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

500	!
	! kill history file
	!
 !	KILL AP_1099_YYYY.DEV$ + "AP_1099_" + YEAR_1099$ + ".HIS" &
 !		FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AP_1099_YYYY.DEV$ + "AP_1099_" + &
		YEAR_1099$ + ".HIS;*")

 ExitProgram:

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	GOTO ExitProgram

	%Page

32767	END
