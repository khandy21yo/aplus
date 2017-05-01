1	%TITLE "Kill Cash Disbursements Journal"
	%SBTTL "AP_SPEC_KILL_CDJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
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
	!	The ^*Kill Cash Disbursements Journal\* program deletes specified
	!	batch numbers within the Purchase Journal Maintenance.
	!	.lm -5
	!
	! Index:
	!	.x Kill Cash Disbursements Journal
	!
	! Option:
	!
	!	AP_SPEC_KILL_CDJ$BATCH
	!
	! Author:
	!
	!	07/11/2002 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_KILL_CDJ/LINE
	!	$ LINK/EXEC=AP_EXE:*.EXE AP_SPEC_KILL_CDJ,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_KILL_CDJ.OBJ;*
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Map of the various files
	!
	COM (CH_AP_CDJ) &
		AP_CDJ.CH%, &
		AP_CDJ.READONLY%

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD	AP_CDJ

	DIM JRL_FILE$(50%)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_CDJ", AP_CDJ.DEV$, STAT%)

	!
	! Get info required for main file
	!
300	!
	! Kill any journals with null batch numbers.
	!
	SMG_STATUS% = LIB$DELETE_FILE(AP_CDJ.DEV$ + "AP_CDJ_.JRL;*")

	!
	! Query user for year of file
	!
	CALL FIND_FILE(AP_CDJ.DEV$ + "AP_CDJ_*.JRL", JRL_FILE$(), 16%, "", "")

	JRL_FILE% = VAL%(JRL_FILE$(0%))

	IF JRL_FILE%
	THEN
		JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 8%, 2%) &
			FOR LOOP% = 1% TO JRL_FILE%

		X% = ENTR_3CHOICE(SCOPE, "", "", JRL_FILE$(), "", &
			0%, "CDJ Batch Files", "", 0%)

		IF X% > 0%
		THEN
			CDJ_BATCH$ = EDIT$(JRL_FILE$(X%), -1%)
			GOTO 700
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	SCOPE::PRG_ITEM = "BATCH"
	!++
	! Abstract:BATCH
	!	^*Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the batch number which
	!	is to be deleted.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Kill Purchase Journal
	!	.x Kill Purchase Journal>Batch Number
	!
	!--
	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS( SMG_SCREEN_DATA%, &
		"Please enter a Batch number?", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
320	!
	! Get the journal name
	!
	CDJ_BATCH$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 6%, 49%, &
		CDJ_BATCH$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	CDJ_BATCH$ = "01" IF EDIT$(CDJ_BATCH$, -1%) = ""

	IF LEN(EDIT$(CDJ_BATCH$, -1%)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch # in (XX) format", 0%)
		GOTO 320
	END IF

700	!
	! Confirm
	!
	SCOPE::PRG_ITEM = "DELBATCH"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Delete Batch " + CDJ_BATCH$, "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	GOTO ExitProgram IF INP$ = "N"

	SMG_STATUS% = LIB$DELETE_FILE(AP_CDJ.DEV$ + "AP_CDJ_" + &
		CDJ_BATCH$ + ".JRL;*")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
