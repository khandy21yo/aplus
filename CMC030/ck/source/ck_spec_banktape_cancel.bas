1	%TITLE "Enter Cancelled Checks from Bank Disk"
	%SBTTL "CK_SPEC_BANKTAPE_CANCEL"
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
	!	The ^*DISK\* option reads diskettes provided
	!	by the bank containing data in reference to cancelled checks and deposits
	!	credited.
	!	.lm -5
	!
	! Index:
	!	.x Diskette>Read
	!	.x Read>Diskette from Bank
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_SPEC_BANKTAPE_CANCEL/LINE
	!	$ LINK/EXE=CK_EXE: CK_SPEC_BANKTAPE_CANCEL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_SPEC_BANKTAPE_CANCEL.OBJ;*
	!
	! Author:
	!
	!	06/01/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
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
	CALL	READ_DEVICE("CK_CKMNT", CK_CKMNT.DEV$, STAT%)

	%PAGE

	!*******************************************************************
	! Draw the screen
	!*******************************************************************
300	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, &
		SMG_SCREEN_DATA%)

	!
	! Print background
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Read Cleared Items from Diskette", 6%, 15%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Bank Account: ", 10%, 15%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"File Name: ", 12%, 15%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	!
	! Set defaults for the File Name and Bank Account number
	!
	FILE_NAME$ = "            "
	BANK_ACCT$ = "      "

	!
	! Query user for Account Number
	!
320	SCOPE::PRG_ITEM = "FLD01BA"

	!++
	! Abstract:FLD01BA
	!	^*Bank Account\*
	!	.b
	!	.lm +5
	!	The ^*Bank Account\* field enters the number or name
	!	of the cash account as defined in the Check Reconciliation utility file.
	!	.b
	!	The field will accommodate up to six (6) characters.
	!	.lm -5
	!
	! Index:
	!	.x Bank Account>Read Cancelled from Disk
	!	.x Read Cancelled form Disk>Bank Account
	!
	!--

	SCOPE::SCOPE_EXIT = ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 10%, 31%, &
		BANK_ACCT$, -1%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO 340

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	!
	! Query user for File Name
	!
340	SCOPE::PRG_ITEM = "FLD02FN"

	!++
	! Abstract:FLD02FN
	!	^*File Name\*
	!	.b
	!	.lm +5
	!	The ^*File Name\* field enters the name of the file into
	!	which the temporary file (imported from a PC) is to be
	!	read. An example of a file name could be "CK__JUL89.BNK."
	!	.b
	!	The field provides twelve (12) spaces for entry.
	!	.lm -5
	!
	! Index:
	!	.x File Name
	!	.x Name>File
	!
	!--

	SCOPE::SCOPE_EXIT = ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 12%, 31%, &
		FILE_NAME$, -1%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO 320

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 340

	END SELECT

	!
	! Go back to start if either default is still there
	!
	GOTO 320 IF (FILE_NAME$ = "            ") OR (BANK_ACCT$ = "      ")

	!*******************************************************************
	! Try to open the bank file
	!*******************************************************************
1000	CALL ASSG_CHANNEL(FILE_NAME.CH%, STATUS%)

	OPEN FILE_NAME$ FOR INPUT AS FILE FILE_NAME.CH%, &
		ACCESS READ

	!*******************************************************************
	! Open CK_CKMNT master file
	!*******************************************************************
1050	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.MOD"

	!*******************************************************************
	! Loop through text file
	!*******************************************************************

1100	WHEN ERROR IN
		LINPUT #FILE_NAME.CH%, ENTRYLINE$
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = FILE_NAME$
		CONTINUE HelpError
	END WHEN

	GOTO 1100 IF RIGHT(ENTRYLINE$, 57%) <> "1"

	CK_CKMNT::BANK_ACCT = BANK_ACCT$
	CK_CKMNT::ETYPE = "C"
	CK_CKMNT::STYPE = "B"
	CK_CKMNT::GLDATE = ""

	CK_CKMNT::CKNUM = MID(ENTRYLINE$, 11%, 6%)

	TEMP.YEAR$ = MID(ENTRYLINE$, 40%, 2%)
	TEMP.MNTH$ = MID(ENTRYLINE$, 36%, 2%)
	TEMP.DAYS$ = MID(ENTRYLINE$, 38%, 2%)
	CK_CKMNT::CKDAT = "19" + TEMP.YEAR$ + TEMP.MNTH$ + TEMP.DAYS$

	TEMP_AMNT$ = MID(ENTRYLINE$, 20%, 10%)
	CK_CKMNT::CKAMT = VAL%(TEMP_AMNT$) * 1.0 / 100.0

1110	PUT #CK_CKMNT.CH%

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
