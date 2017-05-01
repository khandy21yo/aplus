1	%TITLE "Issue Journal Entry"
	%SBTTL "WP_JOUR_ISSJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center
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
	!	The ^*Issue Journal Entry\* option enters journals for the
	!	materials issued on jobs.
	!	.b
	!	Each Journal File is assigned a batch number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Material Issue Journal
	!	.x Journal>Material Issue
	!
	! Option:
	!	WP_MAIN_ISSJOUR$HELP
	!	WP_MAIN_ISSLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_JOUR_ISSJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_JOUR_ISSJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_JOUR_ISSJOUR.OBJ;*
	!
	! Author:
	!
	!	07/18/91 - Craig Tanner
	!
	! Modification history:
	!
	!	09/25/92 - Dan Perkins
	!		Added code to read PD_PRODUCT.
	!
	!	01/11/95 - Kevin Handy
	!		Some reformatting.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice.
	!
	!	07/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/19/98 - Kevin Handy
	!		Change batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/19/2000 - Kevin Handy
	!		Fix problem with long batch numbers. (was cutting
	!		it off at 2 characters)
	!
	!	11/27/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_ISSJOUR) &
		WP_ISSJOUR.CH%

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("WP_ISSJOUR", WP_ISSJOUR.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(WP_ISSJOUR.DEV$ + "WP_ISSJOUR_*.JRL", &
		WP_ISSJOUR_FILE$(), 16%, "", "")

	WP_ISSJOUR_FILE% = VAL%(WP_ISSJOUR_FILE$(0%))

	IF WP_ISSJOUR_FILE%
	THEN
 !		I% = INSTR(12%, WP_ISSJOUR_FILE$(LOOP%), ".") - 1%
 !		I% = 13% IF I% = -1%
 !
 !		WP_ISSJOUR_FILE$(LOOP%) = &
 !			SEG$(WP_ISSJOUR_FILE$(LOOP%), 12%, I%) &
 !			FOR LOOP% = 1% TO WP_ISSJOUR_FILE%
		WP_ISSJOUR_FILE$(LOOP%) = &
			RIGHT(WP_ISSJOUR_FILE$(LOOP%), 12%) &
			FOR LOOP% = 1% TO WP_ISSJOUR_FILE%

		TEMP$ = "WIP Order ISSJOUR Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", WP_ISSJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(WP_ISSJOUR_FILE$(X%), -1%)
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "BATCH"

	BATCH_NO$ = "01      "

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		11%, 43%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) < 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter at least 2 characters", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(WP_MAIN_ISSJOUR.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)	WP_ISSJOUR_CDD WP_ISSJOUR
	MAP (WP_ISSJOUR_OLD)	WP_ISSJOUR_CDD WP_ISSJOUR_OLD
	MAP (WP_ISSJOUR_ONE)	WP_ISSJOUR_CDD WP_ISSJOUR_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_ISSJOUR
	EXTERNAL LONG FUNCTION WP_MAIN_ISSLINE
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION WP_MAIN_REQREGISTER
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_ISSJOUR.ID

		MAINT_GROUP = WP_MAIN_ISSJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			WP_ISSJOUR_ONE = WP_ISSJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line"

		CASE OPT_MOREMENU
			WP_ISSJOUR_ONE = WP_ISSJOUR
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_ISSLINE.ID, &
					"")
			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"

				IF WP_ISSJOUR::JOB <> WP_ISSJOUR_OLD::JOB
				THEN
					WP_ISSJOUR_ONE = WP_ISSJOUR_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_ISSLINE.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_ISSJOUR_ONE = WP_ISSJOUR
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_ISSLINE.ID, &
					"E")

			!
			! Add records in subwindow
			!
			CASE "Add"
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_ISSLINE.ID, &
					"A")

			END SELECT
		END SELECT

	CASE WP_MAIN_ISSLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_ISSJOUR_ONE::REQNUM + &
				WP_ISSJOUR_ONE::JOB + &
				WP_ISSJOUR_ONE::LLINE

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = WP_ISSJOUR::REQNUM + &
					WP_ISSJOUR::JOB + WP_ISSJOUR::LLINE
			CASE ELSE
				MVALUE = WP_ISSJOUR_ONE::REQNUM + &
					WP_ISSJOUR_ONE::JOB + &
					WP_ISSJOUR_ONE::LLINE
			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_ISSLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REQREGISTER.ID

		MAINT_GROUP = WP_MAIN_REQREGISTER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
