1	%TITLE "Purchase Order Journal"
	%SBTTL "PO_JOUR_ORDERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Purchase Order Entry Journal\* option
	!	maintains orders being placed.
	!	Each Journal File is assigned a user batch number consisting of
	!	two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order>Maintenance
	!	.x Maintenance>Purchase Orders
	!	.x Journal>Purchase Orders
	!	.x Purchase Order>Journal
	!
	! Option:
	!
	!	PO_MAIN_ORDERJOUR$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_JOUR_ORDERJOUR/LINE
	!	$ LINK/EXE=PO_EXE: PO_JOUR_ORDERJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_JOUR_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!	06/13/90 - Aaron Redd
	!
	! Modification history:
	!
	!	07/16/90 - Kevin Handy
	!		Modified to open REGISTER file.
	!
	!	02/07/92 - Dan Perkins
	!		Added BATCH feature to program.
	!
	!	02/12/92 - Dan Perkins
	!		Added code to accomodate SUBLINE function.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	01/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/28/94 - Kevin Handy
	!		Make GL access read only.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last paramater to ENTR_3CHOICES.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	! MAP's
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Set up COMMON areas
	!
	COM (CH_PO_ORDERJOUR) &
		BATCH_NO$ = 2%, &
		PO_ORDERJOUR.CH%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("PO_ORDERJOUR", PO_ORDERJOUR.DEV$, STAT%)

300	!
	! Open chart of accounts read/only
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 310
	END WHEN

	GL_CHART.READONLY% = -1%

310	!
	! Set things do display the BATCH
	!
	CALL FIND_FILE( PO_ORDERJOUR.DEV$ + "PO_ORDERJOUR_%%.JRL", PO_ORDERJOUR_FILE$(), &
		16%, "", "")

	PO_ORDERJOUR_FILE% = VAL%(PO_ORDERJOUR_FILE$(0%))

	IF PO_ORDERJOUR_FILE%
	THEN
		PO_ORDERJOUR_FILE$(LOOP%) = &
			MID(PO_ORDERJOUR_FILE$(LOOP%), 14%, 2%) &
				FOR LOOP% = 1% TO PO_ORDERJOUR_FILE%

		TEMP$ = "Purchase Order Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", PO_ORDERJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(PO_ORDERJOUR_FILE$(X%), -1%)
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

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(PO_MAIN_ORDERJOUR.ID, "")

	%PAGE

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR
	MAP (PO_ORDERJOUR_OLD)	PO_ORDERJOUR_CDD	PO_ORDERJOUR_OLD
	MAP (PO_ORDERJOUR_ONE)	PO_ORDERJOUR_CDD	PO_ORDERJOUR_ONE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE
	MAP (PO_ORDERLINE_OLD)	PO_ORDERLINE_CDD	PO_ORDERLINE_OLD
	MAP (PO_ORDERLINE_ONE)	PO_ORDERLINE_CDD	PO_ORDERLINE_ONE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION MAIN_WINDOW

	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PO_MAIN_ACKNOWLEDGE
	EXTERNAL LONG FUNCTION PO_MAIN_PRODCROSS
	EXTERNAL LONG FUNCTION PO_MAIN_ORDERJOUR
	EXTERNAL LONG FUNCTION PO_MAIN_ORDERLINE
	EXTERNAL LONG FUNCTION PO_MAIN_ORDERSLINE
	EXTERNAL LONG FUNCTION PO_MAIN_REGLINE
	EXTERNAL LONG FUNCTION PO_MAIN_REGSLINE
	EXTERNAL LONG FUNCTION PO_MAIN_TYPE
	EXTERNAL LONG FUNCTION PO_MAIN_NOTES
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE
	EXTERNAL LONG FUNCTION UTL_MAIN_STATE
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UT_MAIN_FOB
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_ACKNOWLEDGE.ID

		MAINT_GROUP = PO_MAIN_ACKNOWLEDGE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_ORDERJOUR.ID

		MAINT_GROUP = PO_MAIN_ORDERJOUR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			PO_ORDERJOUR_ONE = PO_ORDERJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line "

		CASE OPT_MOREMENU
			PO_ORDERJOUR_ONE = PO_ORDERJOUR

			SELECT EDIT$(MVALUE, -1%)

			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add records in subwindow
			!
			CASE "Add"
				PO_ORDERJOUR_ONE = PO_ORDERJOUR
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERLINE.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PO_ORDERJOUR::PO <> PO_ORDERJOUR_OLD::PO
				THEN
					PO_ORDERJOUR_ONE = PO_ORDERJOUR_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_ORDERLINE.ID, "C")
				END IF

				!
				! Check for location change and if so then reset
				! inventory quantities from old location to new
				! location.
				!
				IF PO_ORDERJOUR::FROMLOCATION <> PO_ORDERJOUR_OLD::FROMLOCATION
				THEN
					PO_ORDERJOUR_ONE = PO_ORDERJOUR_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_ORDERLINE.ID, "R")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PO_ORDERJOUR_ONE = PO_ORDERJOUR
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERLINE.ID, "E")

			END SELECT

		END SELECT

	CASE PO_MAIN_ORDERLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PO_ORDERJOUR_ONE::PO

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PO_ORDERJOUR::PO

			CASE ELSE
				MVALUE = PO_ORDERJOUR_ONE::PO

			END SELECT

		CASE OPT_OPTLIST

			MVALUE = MVALUE + " Line_detail "

		CASE OPT_MOREMENU
			PO_ORDERLINE_ONE = PO_ORDERLINE

			SELECT EDIT$(MVALUE, -1%)

			!
			! Options
			!
			CASE "LINE_DETAIL"
	!++
	! Abstract:LINE_DETAIL
	!--
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERSLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			CASE "Add"
				PO_ORDERLINE_ONE = PO_ORDERLINE
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERSLINE.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PO_ORDERLINE::PO <> PO_ORDERLINE_OLD::PO
				THEN
					PO_ORDERLINE_ONE = PO_ORDERLINE_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_ORDERSLINE.ID, "C")
				END IF

				!
				! Check for location change and if so then reset
				! inventory quantities from old location to new
				! location.
				!
				IF PO_ORDERJOUR::FROMLOCATION <> PO_ORDERJOUR_OLD::FROMLOCATION
				THEN
					PO_ORDERLINE_ONE = PO_ORDERLINE_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_ORDERSLINE.ID, "R")
				END IF

			CASE "Erase"
				PO_ORDERLINE_ONE = PO_ORDERLINE
				MAINT_GROUP = MAIN_WINDOW(PO_MAIN_ORDERSLINE.ID, "E")

			END SELECT

		END SELECT

		MAINT_GROUP = PO_MAIN_ORDERLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_ORDERSLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PO_ORDERLINE_ONE::PO + PO_ORDERLINE_ONE::PO_LINE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PO_ORDERLINE::PO + PO_ORDERLINE::PO_LINE

			CASE ELSE
				MVALUE = PO_ORDERLINE_ONE::PO + PO_ORDERLINE_ONE::PO_LINE

			END SELECT

		END SELECT

		MAINT_GROUP = PO_MAIN_ORDERSLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_PRODCROSS.ID

		SELECT MOPTION

		CASE OPT_SUBWIND
			MVALUE = PO_ORDERLINE::OUR_PRODUCT

		END SELECT

		MAINT_GROUP = PO_MAIN_PRODCROSS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_REGLINE.ID

		MAINT_GROUP = PO_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_REGSLINE.ID

		MAINT_GROUP = PO_MAIN_REGSLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_TYPE.ID

		MAINT_GROUP = PO_MAIN_TYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_NOTES.ID

		MAINT_GROUP = PO_MAIN_NOTES(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID

		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_STATE.ID

		MAINT_GROUP = UTL_MAIN_STATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_FOB.ID

		MAINT_GROUP = UT_MAIN_FOB(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID

		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
