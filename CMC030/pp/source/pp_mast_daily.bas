1	%TITLE "Pacific Pride Daily Transaction Maintenance"
	%SBTTL "PP_MAST_DAILY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	!	The ^*Daily Transaction Maintenance\* option
	!	accesses the file where the Pacific Pride Daily Transaction records are
	!	maintained.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	PP_MAIN_DAILY$HELP
	!
	! Compile:
	!
	!	$ BAS SOURCE:[PP.SOURCE]PP_MAST_DAILY/LINE
	!	$ LINK/EXEC:PP_EXE PP_MAST_DAILY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_MAST_DAILY.OBJ;*
	!
	! Author:
	!
	!	01/06/93 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/28/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"

	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("PP_DAILY", PP_DAILY.DEV$, STAT%)

300	!
	! Query user for date of file
	!
	CALL FIND_FILE(PP_DAILY.DEV$ + "PP_DAILY_%%%%%%%%.JRL", &
		PP_DAILY_FILE$(), 16%, "", "")

	PP_DAILY_FILE% = VAL%(PP_DAILY_FILE$(0%))

	IF PP_DAILY_FILE%
	THEN
		PP_DAILY_FILE$(LOOP%) = PRNT_DATE( &
			MID(PP_DAILY_FILE$(LOOP%), 10%, 8%), 8%) &
				FOR LOOP% = 1% TO PP_DAILY_FILE%

		TEMP$ = "Daily Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", PP_DAILY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = DATE_STOREDATE( &
				EDIT$(PP_DAILY_FILE$(X%), -1%))

			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for batch number
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", &
		11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01BATCH"

	!
	! Assign default batch number
	!
	BATCH_NO$ = DATE_TODAY

	BATCH_NO$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "11;43", TEMP$, &
		BATCH_NO$, 0%, "8", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in YYYYMMDD format", 0%)

		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PP_MAIN_DAILY.ID, "")

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

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION PP_MAIN_DAILY
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PP_MAIN_TRANTYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE
	EXTERNAL LONG FUNCTION UTL_MAIN_STATE

	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PP_MAIN_DAILY.ID

		MAINT_GROUP = PP_MAIN_DAILY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PP_MAIN_TRANTYPE.ID

		MAINT_GROUP = PP_MAIN_TRANTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID

		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_STATE.ID

		MAINT_GROUP = UTL_MAIN_STATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
