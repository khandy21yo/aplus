1	%TITLE "REMOVE - Remove Unneeded PR Folders"
	%SBTTL "PR_SPEC_KILL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	.b
	!	.lm +5
	!	The ^*Remove Unneeded PR Folders\*
	!	option in the Payroll Utility menu
	!	will delete a payroll folder file which exists but
	!	contains no data or invalid data.
	!	.b
	!	You should not delete a folder which has been
	!	accural or final posted, because it will have
	!	data in the General Ledger which will not be
	!	removed by this program.
	!	.lm -5
	!
	! Index:
	!	.x Remove>Payroll Folder
	!	.x Payroll>Remove Folder
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_KILL/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_KILL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_KILL.OBJ;*
	!
	! Author:
	!
	!	05/15/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/07/89 - Aaron Redd
	!		Modified to keep running, even if one or two
	!		of the three files are missing.
	!
	!	11/30/92 - Kevin Handy
	!		Fixed bug in calculation of available payroll dates.
	!		Used FILE_DATE% more than should have.
	!
	!	04/26/93 - Kevin Handy
	!		Modified to display messages about deleting files
	!		before deleting them, not after (where error causes
	!		them not to print anyway).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice
	!
	!	06/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/08/98 - Kevin Handy
	!		Fix so won't come up with extra errors when
	!		one class of files is missing.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose excess %PAGE
	!
	!	10/26/98 - Kevin Handy
	!		Don't bother creating SMG_SCREEN_DATA%
	!
	!	09/19/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP	(PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)	PR_TRN_DED_CDD		PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY

	!
	! Dimension statements
	!
	DIM REVERSE_LIST$(200%), &
		FILE_DATE$(200%), &
		FILE_DATE1$(200%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

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
 !		SCOPE::SMG_PBID, 1%, 1%)

	%PAGE

	!******************************************************************
	! Check the existence of the three files, and see what's available
	!******************************************************************

	!
	! Look up device for PR Pay file
	!
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", FILE_DATE$(), &
		16%, "", "")

	FILE_DATE% = VAL%(FILE_DATE$(0%))

	IF (FILE_DATE% = 0%)
	THEN
		PAY.EXIST% = 0%
	ELSE
		PAY.EXIST% = -1%
		DAT.STRT% = 12%
	END IF

	!
	! Look up device for PR Deductions file
	!
	CALL READ_DEVICE("PR_TRN_DED", PR_TRN_DED.DEV$, STAT%)
	CALL FIND_FILE(PR_TRN_DED.DEV$ + "PR_TRN_DED_*.JRL", FILE_DATE1$(), &
		16%, "", "")

	FILE_DATE1% = VAL%(FILE_DATE1$(0%))

	IF (FILE_DATE1% = 0%)
	THEN
		DED.EXIST% = 0%
	ELSE
		DED.EXIST% = -1%

		!
		! If Pay file didn't exist, then set the list of available files
		!
		IF NOT PAY.EXIST%
		THEN
			FILE_DATE% = FILE_DATE1%
			FILE_DATE$(I%) = FILE_DATE1$(I%) &
				FOR I% = 0% to FILE_DATE1%
			DAT.STRT% = 12%
		END IF
	END IF

	!
	! Look up device for PR Check file
	!
	CALL READ_DEVICE("PR_TRN_CHECK", PR_TRN_CHECK.DEV$, STAT%)
	CALL FIND_FILE(PR_TRN_CHECK.DEV$ + "PR_TRN_CHECK_*.JRL", &
		FILE_DATE1$(), &
		16%, "", "")

	FILE_DATE1% = VAL%(FILE_DATE1$(0%))

	IF (FILE_DATE1% = 0%)
	THEN
		CHECK.EXIST% = 0%
	ELSE
		CHECK.EXIST% = -1%

		!
		! If PR Pay and Deduction files didn't exist,
		! then set the list of available files
		!
		IF NOT (PAY.EXIST% OR DED.EXIST%)
		THEN
			FILE_DATE% = FILE_DATE1%
			FILE_DATE$(I%) = FILE_DATE1$(I%) &
				FOR I% = 1% to FILE_DATE1%
			DAT.STRT% = 14%
		END IF
	END IF

	!
	! If no files exist, then get outta here
	!
	IF NOT ((PAY.EXIST% OR DED.EXIST%) OR CHECK.EXIST%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No Payroll folders found", 0%)
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
	! Ask for the payroll folder date
	!
100	X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), "", 0%, &
		"Payroll Folder Dates", "", 0%)

	!
	! If user specified a date, then go on down
	!
	IF (X% > 0%)
	THEN
		BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
			LEFT(REVERSE_LIST$(X%), 2%) + &
			MID(REVERSE_LIST$(X%), 4%, 2%)

		!
		! If no PR Pay folders exist, then why bother to check flags?
		!
		GOTO 300 IF PAY.EXIST%

		GOTO 320
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
	! Open the PR Pay file and check its update to master flag
	!******************************************************************

	!
	! Open PR Pay folder
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
		RESET #PR_TRN_PAY.CH%
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 320 IF (ERR = 11%) OR (ERR = 5%)
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! Check pay update flag
	!
	IF PR_TRN_PAY::UPDATE_FLAG
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"The update status flag has been set.", 0%)
		GOTO ExitProgram
	END IF

	%PAGE

	!******************************************************************
	! Open to Deduction and Check folders to check their existence
	!******************************************************************

320	!
	! Open PR Deduction folder
	!
	IF DED.EXIST%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
		USE
			CONTINUE 340 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN
	END IF

340	!
	! Open PR Check folder
	!
	IF CHECK.EXIST%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
		USE
			CONTINUE 500 IF ERR = 5%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

	END IF

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
	!	^*Confirm Payroll Remove Process\*
	!	.p
	!	The ^*Confirm Payroll Remove Process\* asks for user confirmation of the
	!	removal of the unneeded payroll folder file.
	!
	! Index:
	!	.x Confirm>Remove Unneeded Payroll Folder
	!	.x Remove Unneeded Payroll Folder>Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Payroll Remove process  - then press <Do> ", &
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
	! Remove the Payroll Pay folder
	!
	CLOSE PR_TRN_PAY.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Removing the pay file", 8%)

 !	KILL TRM$(PR_TRN_PAY.DEV$) + "PR_TRN_PAY_" + BATCH_NO$ + ".JRL" &
 !		FOR I% = 1% TO 100%

	SMG_STATUS% = LIB$DELETE_FILE(TRM$(PR_TRN_PAY.DEV$) + &
		"PR_TRN_PAY_" + BATCH_NO$ + ".JRL;*")

2000	!
	! Remove the PR Deduction file
	!
	CLOSE PR_TRN_DED.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Removing the deduction file", 1%)

 !	KILL TRM$(PR_TRN_DED.DEV$) + "PR_TRN_DED_" + BATCH_NO$ + ".JRL" &
 !		FOR I% = 1% TO 100%

	SMG_STATUS% = LIB$DELETE_FILE(TRM$(PR_TRN_DED.DEV$) + &
		"PR_TRN_DED_" + BATCH_NO$ + ".JRL;*")

3000	!
	! Remove the PR Check file
	!
	CLOSE PR_TRN_CHECK.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Removing the check file", 1%)

 !	KILL TRM$(PR_TRN_CHECK.DEV$) + "PR_TRN_CHECK_" + BATCH_NO$ + ".JRL" &
 !		FOR I% = 1% TO 100%

	SMG_STATUS% = LIB$DELETE_FILE(TRM$(PR_TRN_CHECK.DEV$) + &
		"PR_TRN_CHECK_" + BATCH_NO$ + ".JRL;*")

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

	%PAGE

19000	!******************************************************************
	! Error Trapping
	!******************************************************************

	!
	! Trap all untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	END
