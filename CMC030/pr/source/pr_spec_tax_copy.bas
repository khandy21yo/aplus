1	%TITLE "Read W-2 Tape to Text File"
	%SBTTL "PR_SPEC_TAX_COPY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1994 BY
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
	!	.p
	!	The "Copy W2 Tape to Text File" program will copy the contents
	!	of a W2 tape to an ASCII disk file.
	!
	! Index:
	!	.x W-2 Tape>Copy
	!	.x Copy>W-2 Tape
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TAX_COPY
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_TAX_COPY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TAX_COPY.OBJ;*
	!
	! Author:
	!
	!	10/17/94 - Kevin Handy
	!		Taken from PR_SPEC_TAX_VERIFY
	!
	! Modification history:
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/03/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose NoFile (Dead code)
	!
	!	10/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAXTAPE.INC"

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Set up screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	!
	! Look up device
	!
	CALL READ_DEVICE("TAPE_DEVICE", TAPE_DEVICE$, STAT%)

 AskTape:
	TOTAPE% = -1%

	SCOPE::PRG_ITEM = "FLDTAPEDEV"
	!++
	! Abstract:FLDTAPEDEV
	!	^*Tape Device\*
	!	.p
	!	The ^*Tape Device\* asks for user input of the type of tape used to store the
	!	information for transportation.
	!
	! Index:
	!	.x W-2 Tape>Tape Device
	!	.x Tape Device>W-2 Tape
	!
	!--

	TEMP$ = "Tape device <" + TAPE_DEVICE$ + "> "

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = SPACE$(20%)
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
		1%, LEN(TEMP$) + 2%, JUNK$, -1%, 16% + 4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

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

	IF JUNK$ <> ""
	THEN
		TAPE_DEVICE$ = JUNK$
	END IF

	IF TAPE_DEVICE$ = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please Enter Tape Device", 1%)
		GOTO AskTape
	END IF

	!***************************************************************
	! Open mag tape drive
	!***************************************************************

	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)

	IF LEFT(TAPE_DEVICE$, 1%) = "M" AND INSTR(1%, TAPE_DEVICE$, ":")
	THEN
		!
		! Magtape (I Hope)
		!
		OPEN TAPE_DEVICE$ FOR INPUT AS FILE PRNT.CH%, &
			ACCESS READ, &
			RECORDSIZE 25% * 276%

		V% = MAGTAPE(3%, 0%, PRNT.CH%)
		TOTAPE% = -1%
	ELSE
		!
		! Text File (I Hope)
		!
		open TAPE_DEVICE$ for input as file #prnt.ch%, &
			recordsize 276%, &
			ACCESS READ, &
			ALLOW READ
	END IF

	!
	! Paint instruction screen
	!

 Verify:
	SCOPE::PRG_ITEM = "VERIFY"
	VERIFY$ = "                    "

	VERIFY$ = ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
		"Outout file name ", &
		VERIFY$, 0%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Verify
	END SELECT

	IF VERIFY$ = ""
	THEN
		GOTO Verify
	END IF

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	CALL ASSG_CHANNEL(VERIFY.CH%, STAT%)

	OPEN VERIFY$ FOR OUTPUT AS FILE VERIFY.CH%, &
		RECORDSIZE 280%

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	IF TOTAPE%
	THEN
		!
		! Magtape
		!
		WHEN ERROR IN
			GET #PRNT.CH%
		USE
			CONTINUE ExitTotal IF ERR = 11%
			FILENAME$ = TAPE_DEVICE$
			CONTINUE HelpError
		END WHEN

		MOVE FROM #PRNT.CH%, BUFF_TEXT$ = RECOUNT

		FOR TEXT_LOOP% = 1% TO LEN(BUFF_TEXT$) STEP 276%

			TEXT$ = MID(BUFF_TEXT$, TEXT_LOOP%, 276%)
			PRINT #VERIFY.CH%, TEXT$

		NEXT TEXT_LOOP%
	ELSE
		!
		! Text file
		!
		WHEN ERROR IN
			Linput #prnt.ch%, TEXT$
		USE
			CONTINUE ExitTotal IF ERR = 11%
			FILENAME$ = TAPE_DEVICE$
			CONTINUE HelpError
		END WHEN

		PRINT #VERIFY.CH%, TEXT$

	END IF

	GOTO 2000

	%Page

 ExitTotal:
	!*********************************************************************
	! totals
	!*********************************************************************

	CLOSE #PRNT.CH%

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
 ! NoFile:
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !
 !	GOTO ExitProgram

32767	END
