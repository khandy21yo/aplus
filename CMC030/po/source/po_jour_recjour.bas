1	%TITLE "Purchase Order Receive Journal"
	%SBTTL "PO_JOUR_RECJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Purchase Order Receive Journal\* option
	!	maintains orders being received.
	!	Each Journal File is assigned a user batch number consisting of
	!	two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	PO_MAIN_RECJOUR$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_JOUR_RECJOUR/LINE
	!	$ LINK/EXE=PO_EXE: PO_JOUR_RECJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_JOUR_RECJOUR.OBJ;*
	!
	! Author:
	!
	!	02/15/92 - Dan Perkins
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Add dimension for file list
	!
	!	12/01/2000 - Kevin Handy
	!		Lose useless error trapping
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

	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	!
	! Set up COMMON areas
	!
	COM (CH_PO_RECJOUR) &
		BATCH_NO$ = 2%, &
		PO_RECJOUR.CH%

	DIM PO_RECJOUR_FILE$(50%)

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("PO_RECJOUR", PO_RECJOUR.DEV$, STAT%)

310	!
	! Set things do display the BATCH
	!
	CALL FIND_FILE(PO_RECJOUR.DEV$ + "PO_RECJOUR_%%.JRL", &
		PO_RECJOUR_FILE$(), 16%, "", "")

	PO_RECJOUR_FILE% = VAL%(PO_RECJOUR_FILE$(0%))

	IF PO_RECJOUR_FILE%
	THEN
		PO_RECJOUR_FILE$(LOOP%) = &
			MID(PO_RECJOUR_FILE$(LOOP%), 12%, 2%) &
				FOR LOOP% = 1% TO PO_RECJOUR_FILE%

		TEMP$ = "Purchase Order Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", PO_RECJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(PO_RECJOUR_FILE$(X%), -1%)
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
	BATCH_NO$ = "01"

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

	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(PO_MAIN_RECJOUR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR)	PO_RECJOUR_CDD		PO_RECJOUR
	MAP (PO_RECJOUR_OLD)	PO_RECJOUR_CDD		PO_RECJOUR_OLD
	MAP (PO_RECJOUR_ONE)	PO_RECJOUR_CDD		PO_RECJOUR_ONE

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.HB"
	MAP (PO_RECLINE)	PO_RECLINE_CDD		PO_RECLINE
	MAP (PO_RECLINE_OLD)	PO_RECLINE_CDD		PO_RECLINE_OLD
	MAP (PO_RECLINE_ONE)	PO_RECLINE_CDD		PO_RECLINE_ONE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION PO_MAIN_RECJOUR
	EXTERNAL LONG FUNCTION PO_MAIN_RECLINE
	EXTERNAL LONG FUNCTION PO_MAIN_REGLINE
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PO_MAIN_PRODCROSS
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_VENDOR.ID
		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_RECJOUR.ID

		MAINT_GROUP = PO_MAIN_RECJOUR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)
		SELECT MOPTION
		CASE OPT_ENTRY
			PO_RECJOUR_ONE = PO_RECJOUR
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line "
		CASE OPT_MOREMENU
			PO_RECJOUR_ONE = PO_RECJOUR
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(PO_MAIN_RECLINE.ID, "")
			END SELECT
		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Erase records in subwindow
			!
			CASE "Add"
				PO_RECJOUR_ONE = PO_RECJOUR
				MAINT_GROUP = &
					MAIN_WINDOW(PO_MAIN_RECLINE.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PO_RECJOUR::PO <> PO_RECJOUR_OLD::PO
				THEN
					PO_RECJOUR_ONE = PO_RECJOUR_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_RECLINE.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PO_RECJOUR_ONE = PO_RECJOUR
				MAINT_GROUP = &
					MAIN_WINDOW(PO_MAIN_RECLINE.ID, "E")
			END SELECT
		END SELECT

	CASE PO_MAIN_RECLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = PO_RECJOUR_ONE::PO
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = PO_RECJOUR::PO
			CASE ELSE
				MVALUE = PO_RECJOUR_ONE::PO
			END SELECT

		END SELECT

		MAINT_GROUP = PO_MAIN_RECLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_REGLINE.ID
		MAINT_GROUP = PO_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_PRODCROSS.ID
		MAINT_GROUP = PO_MAIN_PRODCROSS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID
		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
