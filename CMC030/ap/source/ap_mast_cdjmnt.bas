1	%TITLE "Accounts Payable Maintain Cash Disbursements"
	%SBTTL "AP_MAST_CDJMNT"
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
	!	The ^*Maintain Cash Disbursements\* option
	!	edits the Cash Disbursements file preparatory to printing checks.
	!	.b
	!	The primary tasks which may be accomplished in editing or
	!	maintaining the Cash Disbursement file are:
	!	.list "*"
	!	.list element
	!	.lm 15
	!	Delete a record for an item which is not to be paid
	!	in the pending check printing routine. (The Cash
	!	Disbursements file is a working file. Deleting a
	!	record in this file is like deleting a "copy" of a
	!	record in the Accounts Payable file and has no
	!	effect on the "original" Accounts Payable record.)
	!	.list element
	!	Change the amount to be paid in the event of a
	!	partial payment.
	!	.list element
	!	Record a discount lost on an item on which a
	!	discount was previously recorded.
	!	.list element
	!	Post the check number and check date to an item
	!	for which a manual check was prepared subsequent
	!	to the time a vendor's charge was entered in the
	!	Purchases Journal.
	!	.end list
	!
	! Index:
	!	.x Accounts Payable>Cash Disbursements>Maintain
	!	.x Cash Disbursements>Maintain
	!	.x Maintain>Cash Disbursements
	!
	! Option:
	!	AP_MAIN_CDJ_H$HELP
	!	AP_MAIN_CDJ_L$HELP
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_CDJMNT/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_MAST_CDJMNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_CDJMNT.OBJ;*
	!
	! Author:
	!
	!	10/12/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	12/10/92 - Kevin Handy
	!		Modified for 132 columns.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Added batch number to AP_CDJ.
	!
	!	05/09/97 - Kevin Handy
	!		Lose SMG_BLANK1% display
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ) AP_CDJ_CDD AP_CDJ

	!
	! Common areas
	!
	COM (CH_AP_CDJ) &
		AP_CDJ.CH%, &
		AP_CDJ.READONLY%, &
		CDJ_BATCH$ = 2%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_FAKEHEADER

	!
	! Definitions
	!
	DIM JRL_FILE$(50%)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_CDJ", AP_CDJ.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(AP_CDJ.DEV$ + "AP_CDJ_*.JRL", JRL_FILE$(), &
		16%, "", "")

	JRL_FILE% = VAL%(JRL_FILE$(0%))

	IF JRL_FILE%
	THEN
		JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 8%, 2%) &
			FOR LOOP% = 1% TO JRL_FILE%

		X% = ENTR_3CHOICE(SCOPE, "", "", JRL_FILE$(), "", &
			0%, "CD Batch Files", "", 0%)

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

	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Please enter a Batch number<01> ?  01", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

320	!
	! Get the journal name
	!
	CDJ_BATCH$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		6%, 55%, CDJ_BATCH$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	CDJ_BATCH$ = "01" IF EDIT$(CDJ_BATCH$, -1%) = ""

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	IF LEN(EDIT$(CDJ_BATCH$, -1%)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch # in (XX) format", 0%)
		GOTO 320
	END IF

700	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.CRE"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	AP_CDJ.READONLY% = 0%

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_FAKEHEADER(AP_MAIN_CDJ_H.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! CDD and Maps
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION AP_MAIN_CDJ_H
	EXTERNAL LONG FUNCTION AP_MAIN_CDJ_L
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CDJ_H.ID

		MAINT_GROUP = AP_MAIN_CDJ_H(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CDJ_L.ID

		MAINT_GROUP = AP_MAIN_CDJ_L(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
