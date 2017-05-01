1	%TITLE "Pacific Pride Monthly Transaction Maintenance"
	%SBTTL "PP_MAST_MONTHLY"
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
	!	The ^*Monthly Transaction Maintenance\* option
	!	accesses the file where the Pacific Pride Monthly Transaction records are
	!	maintained.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	PP_MAIN_MONTHLY$HELP
	!
	! Compile:
	!
	!	$ BAS SOURCE:[PP.SOURCE]PP_MAST_MONTHLY/LINE
	!	$ LINK/EXEC:PP_EXE PP_MAST_MONTHLY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_MAST_MONTHLY.OBJ;*
	!
	! Author:
	!
	!	01/11/93 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/06/94 - Kevin Handy
	!		Added dimension for PP_MONTHLY_FILE$(), so that it
	!		isn't limited to 10 months.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last param to entr_3choice.
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
	!		Lose useless error trap.
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
		YYYY_PP$ = 6%

	DIM PP_MONTHLY_FILE$(120%)

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("PP_MONTHLY", PP_MONTHLY.DEV$, STAT%)

300	!
	! Get information needed to open MONTHLY file
	!
	CALL FIND_FILE( PP_MONTHLY.DEV$ + "PP_MONTHLY_%%%%%%.LED", &
		PP_MONTHLY_FILE$(), 16%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	PP_MONTHLY_FILE% = VAL%(PP_MONTHLY_FILE$(0%))

	IF PP_MONTHLY_FILE%
	THEN
		PP_MONTHLY_FILE$(LOOP%) = &
			MID(PP_MONTHLY_FILE$(LOOP%), 12%, 4%) + "_" + &
			MID(PP_MONTHLY_FILE$(LOOP%), 16%, 2%) &
				FOR LOOP% = 1% TO PP_MONTHLY_FILE%

		TEMP$ = "Monthly Transaction Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", PP_MONTHLY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY_PP$ = LEFT(PP_MONTHLY_FILE$(X%), 4%) + &
				RIGHT(PP_MONTHLY_FILE$(X%), 6%)

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

310	PERIOD$ = MID(DATE_TODAY, 5%, 2%)
	YEAR$   = LEFT(DATE_TODAY, 4%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Monthly Transaction File Maintenance", 10%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Year:   " + YEAR$, 12%, 31%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Period: " + PERIOD$, 13%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for year of file
	!
	SCOPE::PRG_ITEM = "FLD001"

	!++
	!
	! Abstract:FLD001
	!	^*Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.b
	!	The format for entry is YYYY.
	!	.lm 5
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 12%, 39%, YEAR$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 330

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	IF LEN(EDIT$(YEAR$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the transaction year in YYYY format", 0%)

		GOTO 320
	END IF

330	SCOPE::PRG_ITEM = "FLD002"

	!++
	! Abstract:FLD002
	!	^*Period\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 13%, 39%, PERIOD$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_DOWN
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 330

	END SELECT

	IF LEN(EDIT$(PERIOD$, -1%)) <> 2% OR &
		VAL%(PERIOD$) < 1% OR &
		VAL%(PERIOD$) > 12%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"The period must be between 1 and 12.", 0%)
		GOTO 330
	END IF

	YYYY_PP$  = YEAR$ + PERIOD$

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PP_MAIN_MONTHLY.ID, "")

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

	EXTERNAL LONG FUNCTION PP_MAIN_MONTHLY
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PP_MAIN_TRANTYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE
	EXTERNAL LONG FUNCTION UTL_MAIN_STATE

	COM (BATCH_NO) &
		YYYY_PP$ = 6%

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PP_MAIN_MONTHLY.ID

		MAINT_GROUP = PP_MAIN_MONTHLY(SMG_WINDOW, &
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
