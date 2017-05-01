1	%TITLE "Requisition Journal Entry"
	%SBTTL "WP_JOUR_REQJOUR"
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
	!	The ^*Requisition Journal Entry\* option
	!	enters journals for material requisitions for the
	!	production of work or job orders.
	!	.b
	!	Each Requisition Journal File is assigned a batch number consisting
	!	of two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Journal>Material
	!	.x Material>Requisition Journal
	!
	! Option:
	!
	!	WP_MAIN_REQJOUR$HELP
	!	WP_MAIN_REQLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_JOUR_REQJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_JOUR_REQJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_JOUR_REQJOUR.OBJ;*
	!
	! Author:
	!
	!	07/24/91 - Jeff Beard
	!
	! Modification history:
	!
	!	07/16/92 - Dan Perkins
	!		Call REQLINE from AFTEROPT.  Change ISSJOUR variables
	!		to REQJOUR variables.  Cleaned code.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice
	!
	!	06/15/95 - Kevin Handy
	!		Modified so it doesn't automatically call the line
	!		option after adding a header.
	!
	!	07/26/95 - Kevin Handy
	!		Aligned cases in select statement so that they
	!		line up.
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/19/98 - Kevin Handy
	!		Change batch number from 3 to 8 digits.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/28/2000 - Kevin Handy
	!		Lose ON ERROR stuff
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
	CALL READ_DEVICE("WP_REQJOUR", WP_REQJOUR.DEV$, STAT%)

300	!
	! Query user for Batch
	!
	CALL FIND_FILE(WP_REQJOUR.DEV$ + "WP_REQJOUR_*.JRL", &
		WP_REQJOUR_FILE$(), 16%, "", "")

	WP_REQJOUR_FILE% = VAL%(WP_REQJOUR_FILE$(0%))

	IF WP_REQJOUR_FILE%
	THEN
		WP_REQJOUR_FILE$(LOOP%) = &
			RIGHT(WP_REQJOUR_FILE$(LOOP%), 12%) &
			FOR LOOP% = 1% TO WP_REQJOUR_FILE%

		TEMP$ = "Requisition Journal    "

		X% = ENTR_3CHOICE(SCOPE, "", "", WP_REQJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(WP_REQJOUR_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

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

	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		BATCH_NO$, -1%, 16%)

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
	V% = MAIN_WINDOW(WP_MAIN_REQJOUR.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR
	MAP (WP_REQJOUR_OLD)	WP_REQJOUR_CDD		WP_REQJOUR_OLD
	MAP (WP_REQJOUR_ONE)	WP_REQJOUR_CDD		WP_REQJOUR_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_MAIN_REQJOUR
	EXTERNAL LONG FUNCTION WP_MAIN_REQLINE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_REQJOUR.ID

		MAINT_GROUP = WP_MAIN_REQJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			WP_REQJOUR_ONE = WP_REQJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line"

		CASE OPT_MOREMENU
			WP_REQJOUR_ONE = WP_REQJOUR

			SELECT EDIT$(MVALUE, -1%)

			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!	^*Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* option in the Command menu
	!	maintains the Requisition of line items in a particular
	!	record.
	!	.lm -5
	!
	! Index:
	!
	!--
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REQLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF WP_REQJOUR::JOB + WP_REQJOUR::LLINE <> &
					WP_REQJOUR_OLD::JOB + &
					WP_REQJOUR_OLD::LLINE
				THEN
					WP_REQJOUR_ONE = WP_REQJOUR_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_REQLINE.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				WP_REQJOUR_ONE = WP_REQJOUR
				MAINT_GROUP = MAIN_WINDOW(WP_MAIN_REQLINE.ID, &
					"E")

			END SELECT

		END SELECT

	CASE WP_MAIN_REQLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = WP_REQJOUR::JOB + WP_REQJOUR::LLINE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = WP_REQJOUR::JOB + WP_REQJOUR::LLINE

			CASE ELSE
				MVALUE = WP_REQJOUR_ONE::JOB + &
					WP_REQJOUR_ONE::LLINE

			END SELECT

		END SELECT

		MAINT_GROUP = WP_MAIN_REQLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REGLINE.ID

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
