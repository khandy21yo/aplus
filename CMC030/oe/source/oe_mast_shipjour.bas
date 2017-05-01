1	%TITLE "Shipping Journal Entry"
	%SBTTL "OE_MAST_SHIPJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Shipping Journal Entry\* option
	!	maintains shipping orders. Each journal file is to be assigned
	!	a user batch number consisting of two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Shipping Journal Entry
	!
	! Option:
	!
	!	OE_MAIN_SHIPJOUR$HELP
	!	OE_MAIN_SHIPLINE$HELP
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAST_SHIPJOUR/LINE
	!	$ LINK/EXE=OE_EXE: OE_MAST_SHIPJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_MAST_SHIPJOUR.OBJ;*
	!
	! Author:
	!
	!
	! Modification history:
	!
	!	09/09/91 - Dan Perkins
	!		Modified to accept F14, List Choices, key in first
	!		field of OE_INVLINE program.
	!
	!	08/20/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last param to entr_3enter.
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_OE_SHIPJOUR) &
		BATCH_NO$ = 2%, &
		OE_SHIPJOUR.CH%

	COM (CH_OE_SHIPLINE) &
		OE_SHIPLINE.CH%

	COM (CH_OE_SHIPLINE2) &
		OE_SHIPLINE2.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAINT_GROUP

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(OE_SHIPLINE2.CH%, STAT%)

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_SHIPJOUR", OE_SHIPJOUR.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(OE_SHIPJOUR.DEV$ + "OE_SHIPJOUR_*.JRL", OE_SHIPJOUR_FILE$(), &
		16%, "", "")

	OE_SHIPJOUR_FILE% = VAL%(OE_SHIPJOUR_FILE$(0%))

	IF OE_SHIPJOUR_FILE%
	THEN
		OE_SHIPJOUR_FILE$(LOOP%) = &
			MID(OE_SHIPJOUR_FILE$(LOOP%), 13%, 2%) &
				FOR LOOP% = 1% TO OE_SHIPJOUR_FILE%

		TEMP$ = "Order Shipping Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", OE_SHIPJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(OE_SHIPJOUR_FILE$(X%), -1%)
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

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(OE_MAIN_SHIPJOUR.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.HB"
	MAP (OE_SHIPJOUR)	OE_SHIPJOUR_CDD		OE_SHIPJOUR
	MAP (OE_SHIPJOUR_OLD)	OE_SHIPJOUR_CDD		OE_SHIPJOUR_OLD
	MAP (OE_SHIPJOUR_ONE)	OE_SHIPJOUR_CDD		OE_SHIPJOUR_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPJOUR
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION OE_MAIN_REGHEADER
	EXTERNAL LONG FUNCTION OE_MAIN_REGLINE
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPLINE
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE OE_MAIN_SHIPJOUR.ID

		MAINT_GROUP = OE_MAIN_SHIPJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			OE_SHIPJOUR_ONE = OE_SHIPJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line "

		CASE OPT_MOREMENU
			OE_SHIPJOUR_ONE = OE_SHIPJOUR

			SELECT EDIT$(MVALUE, -1%)

			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(OE_MAIN_SHIPLINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF OE_SHIPJOUR::ORDNUM <> &
					OE_SHIPJOUR_OLD::ORDNUM
			THEN
				OE_SHIPJOUR_ONE = OE_SHIPJOUR_OLD
				MAINT_GROUP = MAIN_WINDOW( &
					OE_MAIN_SHIPLINE.ID, "C")
			END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				OE_SHIPJOUR_ONE = OE_SHIPJOUR
				MAINT_GROUP = MAIN_WINDOW(OE_MAIN_SHIPLINE.ID, "E")
			END SELECT
		END SELECT

	CASE OE_MAIN_SHIPLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = OE_SHIPJOUR_ONE::ORDNUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = OE_SHIPJOUR::ORDNUM
			CASE ELSE
				MVALUE = OE_SHIPJOUR_ONE::ORDNUM
			END SELECT
		END SELECT

		MAINT_GROUP = OE_MAIN_SHIPLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGHEADER.ID

		MAINT_GROUP = OE_MAIN_REGHEADER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGLINE.ID

		MAINT_GROUP = OE_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)


	END SELECT

32767	END FUNCTION
