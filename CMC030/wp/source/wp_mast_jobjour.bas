1	%TITLE "Job Journal Entry"
	%SBTTL "WP_MAST_JOBJOUR"
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
	!	.lm +5
	!	.b
	!	The ^*Job Journal Entry\* option maintains
	!	entries for which work orders have been created.
	!	.b
	!	Each Journal File is assigned a batch number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Job Order Journal Entry
	!	.x Work Order Journal Entry
	!	.x Journal Entry>Work Order
	!	.x Journal Entry>Job Order
	!
	! Option:
	!
	!	WP_MAIN_JOBJOUR$HELP
	!	WP_MAIN_JOBLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAST_JOBJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_MAST_JOBJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_MAST_JOBJOUR.OBJ;*
	!
	! Author:
	!
	!	05/25/91 - Val James Allen
	!
	! Modification history:
	!
	!	01/14/93 - Frank F. Starman
	!		Added WP_MAIN_REQLINE function
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice
	!
	!	04/24/95 - Kevin Handy
	!		Fix bug where it whould try to change the job number
	!		on reqline when changing the cost in the line.
	!		(WP_JOB_OLD was never set up)
	!
	!	07/27/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/23/2000 - Kevin Handy
	!		Lose error trapping, that did not trap any errors.
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

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("WP_JOB", WP_JOB.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(WP_JOB.DEV$ + "WP_JOB_*.JRL", WP_JOB_FILE$(), &
		16%, "", "")

	WP_JOB_FILE% = VAL%(WP_JOB_FILE$(0%))

	IF WP_JOB_FILE%
	THEN
		FOR LOOP% = 1% TO WP_JOB_FILE%
			WP_JOB_FILE$(LOOP%) = &
				RIGHT$(WP_JOB_FILE$(LOOP%), 8%)
		NEXT LOOP%

		TEMP$ = "WIP Order Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", WP_JOB_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NUM$ = EDIT$(WP_JOB_FILE$(X%), -1%)
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01BATCH"

	!
	! Assign default batch number
	!
	BATCH_NUM$ = "01      "

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		BATCH_NUM$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NUM$ = EDIT$(BATCH_NUM$, -1%)

	IF LEN(BATCH_NUM$) < 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter at least 2 characers batch number", 0%)
		GOTO 320
	END IF

400	BATCH_NO$ = BATCH_NUM$
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(WP_MAIN_JOBJOUR.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB
	MAP (WP_JOB_OLD)	WP_JOB_CDD		WP_JOB_OLD
	MAP (WP_JOB_ONE)	WP_JOB_CDD		WP_JOB_ONE

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE
	MAP (WP_ORDERLINE_OLD)	WP_ORDERLINE_CDD	WP_ORDERLINE_OLD
	MAP (WP_ORDERLINE_ONE)	WP_ORDERLINE_CDD	WP_ORDERLINE_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_JOBJOUR
	EXTERNAL LONG FUNCTION WP_MAIN_JOBLINE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION JC_MAIN_TYPE
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION JC_MAIN_CLASS
	EXTERNAL LONG FUNCTION WP_MAIN_REQLINE
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_JOBJOUR.ID

		MAINT_GROUP = WP_MAIN_JOBJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			WP_JOB_ONE = WP_JOB

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line "

		CASE OPT_MOREMENU
			WP_JOB_ONE = WP_JOB

			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				!
				! Force no changes due to job number change
				!
				WP_JOB_OLD = WP_JOB

				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_JOBLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add records in subwindow
			!
			CASE "Add"
				WP_JOB_ONE = WP_JOB
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_JOBLINE.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF WP_JOB::JOB <> WP_JOB_OLD::JOB
				THEN
					WP_JOB_ONE = WP_JOB_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_JOBLINE.ID, "C")
				END IF

				!
				! Check for location change and if so then reset
				! inventory quantities from old location to new
				! location.
				!
				IF WP_JOB::LOCATION <> WP_JOB_OLD::LOCATION
				THEN
					WP_JOB_ONE = WP_JOB_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_JOBLINE.ID, "R")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_JOB_ONE = WP_JOB
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_JOBLINE.ID, "E")

			END SELECT

		END SELECT

	CASE WP_MAIN_JOBLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_JOB_ONE::JOB

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_JOB::JOB

			CASE ELSE
				MVALUE = WP_JOB_ONE::JOB

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_JOBLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " reQ "

		CASE OPT_MOREMENU
			WP_ORDERLINE_ONE = WP_ORDERLINE

			SELECT EDIT$(MVALUE, -1%)

			!
			! Line
			!
			CASE "REQ"
	!++
	! Abstract:REQ
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REQLINE.ID, &
					"")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add records in subwindow
			!
			CASE "Add"
				WP_ORDERLINE_ONE = WP_ORDERLINE
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REQLINE.ID, &
					"A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				WP_ORDERLINE_ONE = WP_ORDERLINE_OLD
				IF WP_JOB::JOB <> WP_JOB_OLD::JOB
				THEN
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_REQLINE.ID, "C")
				END IF

				!
				! Check for location change and if so then reset
				! inventory quantities from old location to new
				! location.
				!
				IF WP_JOB::LOCATION <> WP_JOB_OLD::LOCATION
				THEN
					WP_ORDERLINE_ONE = WP_ORDERLINE_OLD
					WP_JOB_ONE = WP_JOB_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_REQLINE.ID, "R")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REQLINE.ID, &
					"E")

			END SELECT

		END SELECT

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_TYPE.ID

		MAINT_GROUP = JC_MAIN_TYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_CLASS.ID

		MAINT_GROUP = JC_MAIN_CLASS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REGLINE.ID

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REQLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_JOB_ONE::JOB + WP_ORDERLINE_ONE::LLINE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_JOB::JOB + WP_ORDERLINE::LLINE

			CASE ELSE
				MVALUE = WP_JOB_ONE::JOB + &
					WP_ORDERLINE_ONE::LLINE

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_REQLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
