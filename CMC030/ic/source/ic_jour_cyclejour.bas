1	%TITLE "Journal Entry"
	%SBTTL "IC_JOUR_CYCLEJOUR"
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
	!	The ^*Journal Entry\* option
	!	maintains journal records.
	!	.b
	!	After selecting this option, a screen will appear for the selection
	!	of an existing batch number. When a new batch number is to be created,
	!	press ^*Return\* to bypass the existing batch number screen. A new
	!	screen will appear accommodating entry of a new batch number.
	!	.lm -5
	!
	! Index:
	!	.x Cycle Counting Journal
	!
	! Option:
	!	IC_MAIN_CYCLEJOUR$HELP
	!	IC_MAIN_JOURCOUNT$HELP
	!	IC_MAIN_JOURADJUST$HELP
	!	IC_MAIN_CYCLEJOUR$COPY
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_JOUR_CYCLEJOUR/LINE
	!	$ LINK/EXE=IC_EXE: IC_JOUR_CYCLEJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_JOUR_CYCLEJOUR.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	02/25/92 - Kevin Handy
	!		Removed references to the function PD_MAST_PACK
	!		which Frank deleted, but left references to so
	!		programs couldn't compile.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change last parameter to ENTR_3CHOICE from "" to 0%.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		TRM$ the BATCH_NO$ before taking it's length
	!
	!	12/21/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_IC_CYCLEJOUR) &
		IC_CYCLEJOUR.CH%, &
		IC_CYCLEJOUR.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAINT_GROUP

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

300	!
	! Query user for year of file
	!
	CALL READ_DEVICE("IC_CYCLEJOUR", IC_CYCLEJOUR.DEV$, STAT%)
	CALL FIND_FILE(IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_*.JRL", &
		IC_CYCLEJOUR_FILE$(), 16%, "", "")

	IC_CYCLEJOUR_FILE% = VAL%(IC_CYCLEJOUR_FILE$(0%))

	IF IC_CYCLEJOUR_FILE%
	THEN
		IC_CYCLEJOUR_FILE$(LOOP%) = &
			MID(IC_CYCLEJOUR_FILE$(LOOP%), 14%, 2%) &
				FOR LOOP% = 1% TO IC_CYCLEJOUR_FILE%

		TEMP$ = "Cycle Counting Journal Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", IC_CYCLEJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(IC_CYCLEJOUR_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch number:", 11%, 30%)

320	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(IC_MAIN_CYCLEJOUR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION IC_MAIN_CYCLEJOUR
	EXTERNAL LONG FUNCTION IC_MAIN_JOURCOUNT
	EXTERNAL LONG FUNCTION IC_MAIN_JOURADJUST
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_TRANSTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE IC_MAIN_CYCLEJOUR.ID
		MAINT_GROUP = IC_MAIN_CYCLEJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE IC_MAIN_JOURCOUNT.ID
		MAINT_GROUP = IC_MAIN_JOURCOUNT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE IC_MAIN_JOURADJUST.ID
		MAINT_GROUP = IC_MAIN_JOURADJUST(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_TRANSTYPE.ID
		MAINT_GROUP = UTL_MAIN_TRANSTYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
