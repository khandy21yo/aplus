1	%TITLE "Write Check Information to a Tape"
	%SBTTL "CK_SPEC_BANKTAPE_WRITE"
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
	!	.b
	!	.lm +5
	!	This program is used to write reconciliation
	!	information from the CK files to a generic ASCII file
	!	which can then be written to a tape.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_SPEC_BANKTAPE_WRITE/LINE
	!	$ LINK/EXE=CK_EXE: CK_SPEC_BANKTAPE_WRITE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_SPEC_BANKTAPE_WRITE.OBJ;*
	!
	! Author:
	!
	!	06/06/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/27/2000 - Kevin Handy
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

	!
	! Maps
	!
	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP	(CK_CKMNT)	CK_CKMNT_CDD	CK_CKMNT

	%PAGE

	!
	! Set up format for text file
	!
	!"'LLLLLLLLL'LLLLL   'RRRRRRRRRR      'LLLLL              '"
	!
	ACCT.FORM$ = "'LLLLLLLLL"
	CKNUM.FORM$ = "'LLLLL   "
	CKAMT.FORM$ = "'RRRRRRRRRR      "
	CKDAT.FORM$ = "'LLLLL              "
	REC.ID.FORM$ = "'"

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!*******************************************************************
	! Initialize maintainance
	!*******************************************************************

	CALL READ_INITIALIZE

	SCOPE::PRG_ITEM = "ASK_WHAT"

	!
	! Read device for CKMNT master file
	!
	CALL READ_DEVICE("CK_CKMNT", CK_CKMNT.DEV$, STAT%)

	%PAGE

	!*******************************************************************
	! Draw the screen
	!*******************************************************************
300	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Write Check Reconciliation Info to Tape ", SMG$K_TOP)

	!
	! Print background
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Account Number: ", 8%, 15%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"File Name: ", 10%, 15%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"GL Period (YYYYPP):", 12%, 15%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Set defaults for the File Name, Account number, and Period
	!
	FILE_NAME$ = "            "
	ACCT_NUM$ = "          "
	YYYYPP$ = "      "

	!
	! Query user for Account Number
	!
320	SCOPE::PRG_ITEM = "ACCT_NUM"

	SCOPE::SCOPE_EXIT = ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 8%, 35%, &
		ACCT_NUM$, -1%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 360

	CASE SMG$K_TRM_DOWN
		GOTO 340

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	!
	! Query user for File Name
	!
340	SCOPE::PRG_ITEM = "FILE_NAME"

	SCOPE::SCOPE_EXIT = ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 10%, 35%, &
		FILE_NAME$, -1%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 360

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 340

	END SELECT

360	SCOPE::PRG_ITEM = "GL_PERIOD"

	SCOPE::SCOPE_EXIT = ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 12%, 35%, &
		YYYYPP$, -1%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 340

	CASE SMG$K_TRM_DOWN
		GOTO 320

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 360

	END SELECT

	!
	! Go back to start if any default is still there
	!
	GOTO 320 IF &
		(FILE_NAME$ = "            ") OR &
		(ACCT_NUM$ = "          ") OR &
		(YYYYPP$ = "      ")

	!*******************************************************************
	! Create the ASCII file
	!*******************************************************************
1000	CALL ASSG_CHANNEL(FILE_NAME.CH%, STATUS%)

	OPEN FILE_NAME$ FOR OUTPUT AS FILE FILE_NAME.CH%, &
		ACCESS WRITE

	!*******************************************************************
	! Open CK_CKMNT master file
	!*******************************************************************
1050	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.OPN"

	!*******************************************************************
	! Loop through CK master file, writing where needed
	!*******************************************************************

1100	WHEN ERROR IN
		GET #CK_CKMNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	!
	! Do not write record if check is not from GL, or if
	! not from the right period
	!
	GOTO 1100 IF (CK_CKMNT::STYPE <> "G") OR &
		(CK_CKMNT::GLDATE <> YYYYPP$)

	!
	! Change check amount to fit FORMAT$
	!
	TEMP.AMT% = CK_CKMNT::CKAMT * 100%
	STR.AMT$ = NUM1$(TEMP.AMT%)

	!
	! Put check date in MMDDYY format
	!
	TEMP.DAT$ = RIGHT(CK_CKMNT::CKDAT,5%) + MID(CK_CKMNT::CKDAT,3%,2%)

	!
	! Put together the text line
	!
	TEXT.LINE$ = ACCT_NUM$ + SPACE$(10% - LEN(ACCT_NUM$)) + &
		CK_CKMNT::CKNUM + SPACE$(9% - LEN(CK_CKMNT::CKNUM)) + &
		SPACE$(10% - LEN(STR.AMT$)) + STR.AMT$ + &
		"      " + TEMP.DAT$ + SPACE$(6% - LEN(TEMP.DAT$)) + &
		SPACE$(15%) + "1"

1101	!TEXT.LINE$ = FORMAT$(ACCT.FORM$, ACCT_NUM$)
1102	!TEXT.LINE$ = TEXT.LINE$ + FORMAT$(CKNUM.FORM$, CK_CKMNT::CKNUM)
1103	!TEXT.LINE$ = TEXT.LINE$ + FORMAT$(CKAMT.FORM$, STR.AMT$)
1104	!TEXT.LINE$ = TEXT.LINE$ + FORMAT$(CKDAT.FORM$, TEMP.DAT$)
1105	!TEXT.LINE$ = TEXT.LINE$ + FORMAT$(REC.ID.FORM$, "1")

	!
	! Put the text line in the ASCII file
	!
1106	PRINT #FILE_NAME.CH%, TEXT.LINE$

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

1120	!
	! Close files
	!
	CLOSE #FILE_NAME.CH%
	CLOSE #CK_CKMNT.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

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

32767	END
