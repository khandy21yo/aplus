1	%TITLE "Purchase Order Register Maintenance"
	%SBTTL "PO_MAST_REGISTER"
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
	!	.p
	!	The ^*Maintain Purchase Order Register\* option
	!	maintains the PO Register.
	!	.note
	!	This program is supplied for unusual maintenance purposes only.
	!	You should not normally need to make changes to the register
	!	through the use of this program.
	!	Changed should normally be made through the journals (PO Order,
	!	PO Receive, and AP PJ) so that all systems are impacted with
	!	the necessary changes.
	!	Only make changes here when there is no other way.
	!	.end note
	!
	! Index:
	!	.x Purchase Order>Register>Maintenance
	!	.x Maintenance>Purchase Orders>Register
	!	.x Register>Purchase Orders
	!	.x Purchase Order>Register
	!
	! Option:
	!
	!	PO_MAIN_REGISTER$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAST_REGISTER/LINE
	!	$ LINK/EXE=PO_EXE: PO_MAST_REGISTER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_MAST_REGISTER.OBJ;*
	!
	! Author:
	!
	!	06/29/90 - Kevin Handy
	!
	! Modification history:
	!
	!	02/18/92 - Dan Perkins
	!		Modified to accomodate changes in file layouts.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	12/06/2000 - Kevin Handy
	!		Lose unnecessary error trap
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
	! Common areas
	!
	COM (CH_PO_REG_LINE) &
		PO_REG_LINE.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initialize all the standard stuff through an external call
	!*******************************************************************
	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(PO_MAIN_REGLINE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	MAP (PO_REG_LINE_OLD)	PO_REG_LINE_CDD		PO_REG_LINE_OLD
	MAP (PO_REG_LINE_ONE)	PO_REG_LINE_CDD		PO_REG_LINE_ONE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE
	MAP (PO_REG_SUB_LINE_OLD) PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_OLD
	MAP (PO_REG_SUB_LINE_ONE) PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_ONE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION PO_MAIN_REGLINE
	EXTERNAL LONG FUNCTION PO_MAIN_REGSLINE
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PO_MAIN_TYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
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

	CASE PO_MAIN_REGLINE.ID

		MAINT_GROUP = PO_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)
		SELECT MOPTION
		CASE OPT_ENTRY
			PO_REG_LINE_ONE = PO_REG_LINE
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line "
		CASE OPT_MOREMENU
			PO_REG_LINE_ONE = PO_REG_LINE
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = &
					MAIN_WINDOW(PO_MAIN_REGSLINE.ID, "")
			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Erase records in subwindow
			!
			CASE "Add"
				PO_REG_LINE_ONE = PO_REG_LINE
				MAINT_GROUP = MAIN_WINDOW( &
					PO_MAIN_REGSLINE.ID, "A")

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF PO_REG_LINE::PO <> PO_REG_LINE_OLD::PO
				THEN
					PO_REG_LINE_ONE = PO_REG_LINE_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						PO_MAIN_REGSLINE.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				PO_REG_LINE_ONE = PO_REG_LINE
				MAINT_GROUP = MAIN_WINDOW( &
					PO_MAIN_REGSLINE.ID, "E")
			END SELECT
		END SELECT

	CASE PO_MAIN_REGSLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = PO_REG_LINE_ONE::PO + PO_REG_LINE_ONE::PO_LINE

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = PO_REG_LINE::PO + PO_REG_LINE::PO_LINE

			CASE ELSE
				MVALUE = PO_REG_LINE_ONE::PO + &
					PO_REG_LINE_ONE::PO_LINE

			END SELECT

		END SELECT

		MAINT_GROUP = PO_MAIN_REGSLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_TYPE.ID
		MAINT_GROUP = PO_MAIN_TYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID
		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
