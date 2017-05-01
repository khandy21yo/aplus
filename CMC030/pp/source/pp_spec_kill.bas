1	%TITLE "REMOVE - Remove Unneeded PP Folders"
	%SBTTL "PP_SPEC_KILL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	.b
	!	.lm +5
	!	The ^*Remove Unneeded PP Folders\*
	!	option
	!	deletes a daily folder file which exists but
	!	contains no data or invalid data.
	!	.b
	!	You should not delete a folder which has been
	!	accural or final posted, because it will have
	!	data in the General Ledger which will not be
	!	removed by this program.
	!	.lm -5
	!
	! Index:
	!	.x Remove>Daily Folder
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_SPEC_KILL/LINE
	!	$ LINK/EXECUTABLE=PP_EXE: PP_SPEC_KILL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_SPEC_KILL.OBJ;*
	!
	! Author:
	!
	!	09/01/98 - Kevin Handy
	!
	! Modification history:
	!
	!	10/16/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	10/26/98 - Kevin Handy
	!		Don't create SMG_SCREEN_DISPLAY, since it is
	!		never used.
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.HB"
	MAP	(PP_DAILY)	PP_DAILY_CDD	PP_DAILY

	!
	! Dimension statements
	!
	DIM FILE_DATE$(200%), REVERSE_LIST$(200%)

	%PAGE

	!*******************************************************************
	! Initialize program
	!*******************************************************************
	CALL READ_INITIALIZE

	!
	! Create display
	!
 !	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, &
 !		SMG_SCREEN_DATA%)
 !
 !	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
 !		SCOPE::SMG_PBID, 1%, 1% )

	%PAGE

	!******************************************************************
	! Check the existence of the three files, and see what's available
	!******************************************************************

	!
	! Look up device for PP Daily file
	!
	CALL READ_DEVICE("PP_DAILY", PP_DAILY.DEV$, STAT%)
	CALL FIND_FILE(PP_DAILY.DEV$ + "PP_DAILY_*.JRL", FILE_DATE$(), &
		16%, "", "")

	FILE_DATE% = VAL%(FILE_DATE$(0%))

	IF (FILE_DATE% = 0%)
	THEN
		PP_EXIST% = 0%
	ELSE
		PP_EXIST% = -1%
		DAT.STRT% = 10%
	END IF

	!
	! If no files exist, then get outta here
	!
	IF NOT (PP_EXIST%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No Daily folders found", 0%)
		GOTO ExitProgram
	END IF

	REVERSE_LIST$(FILE_DATE% - LOOP% + 1%) = &
		MID(FILE_DATE$(LOOP%), DAT.STRT% + 4%, 2%) + "/" + &
		MID(FILE_DATE$(LOOP%), DAT.STRT% + 6%, 2%) + "/" + &
		MID(FILE_DATE$(LOOP%), DAT.STRT%, 4%) &
			FOR LOOP% = FILE_DATE% TO 1% STEP -1%

	%PAGE

	!******************************************************************
	! Get the date the user wants to kill
	!******************************************************************

	!
	! Ask for the folder date
	!
100	X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), "", 0%, &
		"PP Daily Folder Dates", "", 0%)

	!
	! If user specified a date, then go on down
	!
	IF (X% > 0%)
	THEN
		BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
			LEFT(REVERSE_LIST$(X%), 2%) + &
			MID(REVERSE_LIST$(X%), 4%, 2%)

		GOTO 500
	END IF

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	GOTO 100

	%PAGE

	!******************************************************************
	! Confrim whether the user wants to kil these files
	!******************************************************************

	!
	! Set help information
	!
500	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm PP Remove Process\*
	!	.p
	!	The ^*Confirm PPRemove Process\* asks for user confirmation of the
	!	removal of the unneeded daily folder file.
	!
	! Index:
	!	.x Confirm>Remove Unneeded Folder
	!	.x Remove Unneeded Folder>Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm PP Daily Remove process  - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	GOTO ExitProgram IF INP$ <> "Y"

	!
	! Reset help information
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

	!******************************************************************
	! Remove the folders, if they exist
	!******************************************************************

1000	!
	! Remove the folder
	!
	CLOSE PP_DAILY.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Removing the daily file", 8%)

 !	KILL TRM$(PP_DAILY.DEV$) + "PP_DAILY_" + BATCH_NO$ + ".JRL" &
 !		FOR I% = 1% TO 100%

	SMG_STATUS% = LIB$DELETE_FILE(TRM$(PP_DAILY.DEV$) + &
		"PP_DAILY_" + BATCH_NO$ + ".JRL;*")

2000	!

	%PAGE

 ExitProgram:

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

32000	END
