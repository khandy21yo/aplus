1	%TITLE "Inventory Period Ledger"
	%SBTTL "IC_MAST_TRANSACTION"
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
	!	The ^*Inventory Period Ledger\* contains all transactions concerning the
	!	inventory including those which are received from other ledgers. All
	!	transactions for each product are assembled in this ledger.
	!	.lm -5
	!
	! Index:
	!	.x Period Transaction
	!	.x Archive>Period Transaction
	!
	! Option:
	!
	!	IC_MAIN_TRANSACTION$HELP
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAST_TRANSACTION/LINE
	!	$ LINK/EXE=IC_EXE: IC_MAST_TRANSACTION,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_MAST_TRANSACTION.OBJ;*
	!
	! Author:
	!
	!	05/10/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix parameters to ENTR_3CHOICE
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter to READ_PERIOD to integer.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	03/21/2001 - Kevin Handy
	!		Change number of files listed from 100 to 300
	!
	!	06/19/2001 - Kevin Handy
	!		Change number of files listed from 300 to 600
	!
	!	02/07/2002 - Kevin Handy
	!		Reverse the order of files in the list.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (PERIOD_IC_TRANSACTION) YYYYPP$ = 6%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAINT_GROUP

	!
	! Dimension statements
	!
	DIM IC_TRANSACTION_FILE$(600)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

300	!
	! Open up control file, and grap record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		CONTINUE 310
	END WHEN

310	!******************************************************************
	! Get period for batch
	!******************************************************************

	!
	! Get info required for main file
	!
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), 16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))

	IF IC_TRANSACTION_FILE%
	THEN
		IC_TRANSACTION_FILE$(LOOP%) = &
			MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%) &
			FOR LOOP% = 1% TO IC_TRANSACTION_FILE%

		!
		! Reverse the list
		!
		FOR LOOP% = 1% TO IC_TRANSACTION_FILE% / 2%
			TEMP$ = IC_TRANSACTION_FILE$(LOOP%)
			IC_TRANSACTION_FILE$(LOOP%) = &
				IC_TRANSACTION_FILE$(IC_TRANSACTION_FILE% - &
				LOOP% + 1%)
			IC_TRANSACTION_FILE$(IC_TRANSACTION_FILE% - &
				LOOP% + 1%) = TEMP$
		NEXT LOOP%

		TEMP$ = "Inventory Ledger Periods"

		X% = ENTR_3CHOICE(SCOPE, "", "", IC_TRANSACTION_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYYPP$ = EDIT$(IC_TRANSACTION_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

315	CUR.PERIOD$ = IC_CONTROL::PERIOD

	!
	! Verify current period
	!
	V% = READ_PERIOD("READ", IC_CONTROL::ERA, CUR.PERIOD$, &
		"", "", "", "", 0%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Inventory Ledger Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Period: ", 12%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	SCOPE::PRG_ITEM = "FLD01PERIOD"
	!++
	! Abstract:FLD01PERIOD
	!	^*Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the desired period for
	!	viewing and working.
	!	.lm -5
	!
	! Index:
	!
	!--

	TRANSACTION.PERIOD$ = ENTR_PERIOD(SMG_SCREEN_DATA%, "12;39", &
		"Period ", CUR.PERIOD$, 0%, IC_CONTROL::ERA, "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_DOWN
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	IF TRANSACTION.PERIOD$ = "??????"
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"The period is undefined or is not open", 0%)
		GOTO 320
	END IF

	YYYYPP$ = TRANSACTION.PERIOD$

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(IC_MAIN_TRANSACTION.ID, "")

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

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	EXTERNAL LONG FUNCTION IC_MAIN_TRANSACTION
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_TRANSTYPE
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE IC_MAIN_TRANSACTION.ID
		MAINT_GROUP = IC_MAIN_TRANSACTION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_TRANSTYPE.ID
		MAINT_GROUP = UTL_MAIN_TRANSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
