1	%TITLE "Kill Cash Receipts Journal"
	%SBTTL "AR_SPEC_KILL_CRJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Kill Cash Receipts Journal\* program
	!	deletes specified
	!	batch numbers within the Cash Receipts Journal Maintenance.
	!	.lm -5
	!
	! Index:
	!	.x Kill Cash Receipts Journal
	!
	! Option:
	!
	!	AR_SPEC_KILL_CRJ$BATCH
	!
	! Author:
	!
	!	03/30/95 - Kevin Handy
	!		Started with a unusable program.
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_KILL_CRJ/LINE
	!	$ LINK/EXEC=AR_EXE:*.EXE AR_SPEC_KILL_CRJ,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_KILL_CRJ.OBJ;*
	!
	! Modification history:
	!
	!	03/30/95 - Kevin Handy
	!		(V3.6)
	!		Fixed bug where it didn't look at the correct
	!		characters for the batch number.
	!		Added device to kill statement.
	!		Allowed different devices for header and lines.
	!		used LIB$DELETE instead of KILL.
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!		Change last param to entr_3choice from "" to 0%.
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		No error trap necessary?
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Map of the various files
	!
	COM (CH_AR_CRJH) &
		AR_CRJH.CH%, &
		AR_CRJH.READONLY%

	COM (CH_AR_CRJL) &
		AR_CRJL.CH%, &
		AR_CRJL.READONLY%

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD	AR_CRJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP (AR_CRJL)		AR_CRJL_CDD	AR_CRJL

	DIM JRL_FILE$(50%)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_CRJH", AR_CRJH.DEV$, STAT%)
	CALL READ_DEVICE("AR_CRJL", AR_CRJL.DEV$, STAT%)

	!
	! Get info required for main file
	!
300	!
	! Kill any journals with null batch numbers.
	!
	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJH.DEV$ + "AR_CRJH_.JRL;*")
	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJL.DEV$ + "AR_CRJL_.JRL;*")

	!
	! Query user for year of file
	!
	CALL FIND_FILE(AR_CRJH.DEV$ + "AR_CRJH_*.JRL", JRL_FILE$(), &
		16%, "", "")

	JRL_FILE% = VAL%(JRL_FILE$(0%))

	IF JRL_FILE%
	THEN
		JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 9%, 2%) &
			FOR LOOP% = 1% TO JRL_FILE%

		X% = ENTR_3CHOICE(SCOPE, "", "", JRL_FILE$(), "", &
			0%, "CRJ Batch Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(JRL_FILE$(X%), -1%)
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
	!	The ^*Batch Number\* field enters the batch which
	!	is to be deleted.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Kill Cash Receipts Journal
	!	.x Kill Cash Receipts Journal>Batch Number
	!
	!--
	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Please enter a Batch number?", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
		SMG_SCREEN_DATA%, SCOPE::SMG_PBID, 1%, 1%)
320	!
	! Get the journal name
	!
	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 6%, 49%, &
		BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	BATCH_NO$ = "01" IF EDIT$(BATCH_NO$, -1%) = ""

	IF LEN(EDIT$(BATCH_NO$, -1%)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch # in (XX) format", 0%)
		GOTO 320
	END IF

700	!
	SCOPE::PRG_ITEM = "DELBATCH"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Delete Batch " + BATCH_NO$, "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	GOTO ExitProgram IF INP$ = "N"

	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJH.DEV$ + "AR_CRJH_" + &
		BATCH_NO$ + ".JRL;*")
	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJL.DEV$ + "AR_CRJL_" + &
		BATCH_NO$ + ".JRL;*")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	GOTO ExitProgram

32767	END
