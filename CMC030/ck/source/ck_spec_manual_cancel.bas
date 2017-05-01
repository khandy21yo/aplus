1	%TITLE "Manually Enter Cancelled Checks"
	%SBTTL "CK_SPEC_MANUAL_CANCEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	!	The ^*Manually Clear Cancelled Checks\* option enters
	!	the cancelled (items which have cleared the bank) checks, deposits, or any
	!	adjustments which have been made to the checking account.
	!	.lm -5
	!
	! Index:
	!	.x Enter>Cancelled Checks
	!	.x Enter>Deposits Credited
	!	.x Cancel>Items Cleared by Bank
	!
	! Option:
	!
	!	CK_MAIN_MANUAL_CANCEL$HELP
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_SPEC_MANUAL_CANCEL/LINE
	!	$ LINK/EXE=CK_EXE: CK_SPEC_MANUAL_CANCEL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_SPEC_MANUAL_CANCEL.OBJ;*
	!
	! Author:
	!
	!	04/20/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

	!
	! Common areas
	!
	COM (TT_CK_SPEC_CANCELL) &
		ETYPE$ = 1%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_JOURNAL

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Ask user for entry type
	!*******************************************************************

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
		"Enter Cleared Checks and Deposits", 6%, 15%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"C = Enter Cleared Checks", 8%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"D = Enter Deposits Cleared", 10%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"A = Enter Adjustments", 12%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Option (C/D/A): ", &
		14%, 15%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	SCOPE::PRG_ITEM = "FLD01TP"

	!++
	! Abstract:FLD01TP
	!	^*Option C/D/A\*
	!	.b
	!	.lm +5
	!	The ^*Option C/D/A\* selection enters a one (1) character code
	!	which indicates which kind of bank transaction is to be entered, i.e. checks,
	!	deposits or adjustments.  Before the deposits are entered, they must be
	!	identified with the user assigned deposit number.
	!	.b
	!	Enter the code for the chosen selection and press ^*<Do>\*.
	!	.table 3,25
	!	.te
	!	^*C\* = Check
	!	.te
	!	^*D\* = Deposit
	!	.te
	!	^*A\* = Adjustment
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Select>Option to Enter Cancelled Checks
	!	.x Select>Option to Enter Deposits Credited
	!	.x Select>Option to Enter Bank Adjustments
	!
	!--

	!
	! Query user for batch number
	!
	ETYPE$ = " "

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 14%, 31%, ETYPE$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	IF (ETYPE$ <> "C") AND (ETYPE$ <> "D") AND (ETYPE$ <> "A")
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter either C, D, or A.", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_JOURNAL(CK_MAIN_MANUAL_CANCEL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

	EXTERNAL LONG FUNCTION CK_MAIN_MANUAL_CANCEL
	EXTERNAL LONG FUNCTION CK_MAIN_CONTROLACC

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE CK_MAIN_CONTROLACC.ID

		MAINT_GROUP = CK_MAIN_CONTROLACC(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE CK_MAIN_MANUAL_CANCEL.ID

		MAINT_GROUP = CK_MAIN_MANUAL_CANCEL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
