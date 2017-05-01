1	%TITLE "Transaction History"
	%SBTTL "IC_MAST_35HISTORY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Product Transaction History\* report provides access to the product
	!	totals based on the transactions. This report is a summary report of the
	!	ledger and balances report.  Each time a period is closed, the totals in the
	!	History report are updated by adding the totals from the period last closed.
	!	.lm -5
	!
	! Index:
	!	.x Transaction History
	!	.x Archive>Transaction History
	!
	! Option:
	!
	!	IC_MAIN_35HISTORY$HELP
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAST_35HISTORY/LINE
	!	$ LINK/EXE=IC_EXE: IC_MAST_35HISTORY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_MAST_35HISTORY.OBJ;*
	!
	! Author:
	!
	!	12/30/91 - Dan Perkins
	!
	! Modification history:
	!
	!	09/08/93 - Kevin Handy
	!		Modified to allow any error at 300, instead of
	!		just "File not found (5)", because a file locked
	!		error would cause a crash and burn.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice.
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter in READ_PERIOD to integer
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/09/2001 - Kevin Handy
	!		Increase dimenstion for transaction files from
	!		100 to 300.
	!
	!	02/07/2002 - Kevin Handy
	!		Reverse the order of dates in the list.
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
	! Common Area
	!
	COM (YEAR_IC_35HISTORY) YYYY$ = 4%, ERA$ = 2%

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	DIM IC_35HISTORY_FILE$(300)

	CALL READ_DEVICE("IC_35HISTORY", IC_35HISTORY.DEV$, STAT%)

300	!
	! Open up control file, and grab record
	!
	IC_CONTROL::PERIOD = "??????"
	ERA$ = "?"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		CONTINUE 310
	END WHEN

	ERA$ = IC_CONTROL::ERA

310	!
	! Query user for year of file
	!
	CALL READ_DEVICE("IC_35HISTORY", IC_35HISTORY.DEV$, STAT%)

	CALL FIND_FILE(IC_35HISTORY.DEV$ + "IC_35HISTORY_%%%%.HIS", &
		IC_35HISTORY_FILE$(), 16%, "", "")

	IC_35HISTORY_FILE% = VAL%(IC_35HISTORY_FILE$(0%))

	IF IC_35HISTORY_FILE%
	THEN
		IC_35HISTORY_FILE$(LOOP%) = &
			MID(IC_35HISTORY_FILE$(LOOP%), 14%, 4%) &
			FOR LOOP% = 1% TO IC_35HISTORY_FILE%

		FOR LOOP% = 1% TO IC_35HISTORY_FILE% / 2%
			TEMP$ = IC_35HISTORY_FILE$(LOOP%)
			IC_35HISTORY_FILE$(LOOP%) = &
				IC_35HISTORY_FILE$(IC_35HISTORY_FILE% - &
				LOOP% + 1%)
			IC_35HISTORY_FILE$(IC_35HISTORY_FILE% - &
				LOOP% + 1%) = TEMP$
		NEXT LOOP%

		TEMP$ = "Inventory History Year"

		X% = ENTR_3CHOICE(SCOPE, "", "", IC_35HISTORY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY$ = EDIT$(IC_35HISTORY_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	CUR.PERIOD$ = IC_CONTROL::PERIOD

	!
	! Verify current period
	!
	V% = READ_PERIOD("READ", IC_CONTROL::ERA,CUR.PERIOD$, &
		"", "", "", "", 0%)

	!
	! Ask for Year number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Year number:", 11%, 30%)

320	!
	! Set up the help message
	!

	SCOPE::PRG_ITEM = "FLD01YEAR"
	!++
	! Abstract:FLD01YEAR
	!	^*Year\*
	!	.B
	!	.LM +5
	!	The ^*Year\* field enters the desired year for
	!	viewing and working.
	!	.LM -5
	!
	! Index:
	!
	!--

	!
	! Assign default year number
	!
	YYYY$ = LEFT$(CUR.PERIOD$, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, YYYY$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	YYYY$ = EDIT$(YYYY$, -1%)

	IF LEN(TRM$(YYYY$)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the year number in XXXX format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(IC_MAIN_35HISTORY.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************


 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION IC_MAIN_35HISTORY
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_TRANSTYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_PERIOD

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE IC_MAIN_35HISTORY.ID
		MAINT_GROUP = IC_MAIN_35HISTORY(SMG_WINDOW, &
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

	CASE UTL_MAIN_PERIOD.ID
		MAINT_GROUP = UTL_MAIN_PERIOD(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
