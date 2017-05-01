1	%TITLE "Equipment Ledger Description Maintenance"
	%SBTTL "EL_MAST_EQUIPMENT"
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
	!	The ^*Equipment Ledger Description Maintenance\* option provides
	!	the means to enter and maintain all equipment descriptions.
	!	Each new piece of equipment must be entered here.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	EL_MAIN_EQUIPMENT$HELP
	!	SB_MAIN_BUDGET$HELP
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_MAST_EQUIPMENT/LINE
	!	$ LINK/EXE=EL_EXE: EL_MAST_EQUIPMENT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_MAST_EQUIPMENT.OBJ;*
	!
	! Author:
	!
	!	10/14/92 - Dan Perkins
	!
	! Modification history:
	!
	!	07/24/93 - Frank F. Starman
	!		replace EL type and category with PD type and category.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(EL_MAIN_EQUIPMENT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!******************************************************************
	! Error trapping
	!******************************************************************

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[EL.OPEN]EL_EQUIPMENT.HB"
	MAP (SB_SUBACCOUNT)	EL_EQUIPMENT_CDD	EL_EQUIPMENT
	MAP (EL_EQUIPMENT_OLD)	EL_EQUIPMENT_CDD	EL_EQUIPMENT_OLD
	MAP (EL_EQUIPMENT_ONE)	EL_EQUIPMENT_CDD	EL_EQUIPMENT_ONE

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	EXTERNAL LONG FUNCTION EL_MAIN_EQUIPMENT
	EXTERNAL LONG FUNCTION PD_MAIN_PRODTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION SB_MAIN_BUDGET
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE EL_MAIN_EQUIPMENT.ID

		MAINT_GROUP = EL_MAIN_EQUIPMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " bUdget"

		CASE OPT_MOREMENU
			EL_EQUIPMENT_ONE = EL_EQUIPMENT

			SELECT EDIT$(MVALUE, -1%)
			!
			! Price
			!
			CASE "BUDGET"
				MAINT_GROUP = MAIN_WINDOW(SB_MAIN_BUDGET.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF EL_EQUIPMENT::SUBJECT + &
					EL_EQUIPMENT::EQNUM <> &
					EL_EQUIPMENT_OLD::SUBJECT + &
					EL_EQUIPMENT_OLD::EQNUM
				THEN
					EL_EQUIPMENT_ONE = EL_EQUIPMENT_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						SB_MAIN_BUDGET.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				EL_EQUIPMENT_ONE = EL_EQUIPMENT

				MAINT_GROUP = MAIN_WINDOW(SB_MAIN_BUDGET.ID, "E")

			END SELECT

		END SELECT

	CASE PD_MAIN_PRODTYPE.ID

		MAINT_GROUP = PD_MAIN_PRODTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_CATEGORY.ID

		MAINT_GROUP = PD_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SB_MAIN_BUDGET.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = "EL" + EL_EQUIPMENT_ONE::EQNUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = "EL" + EL_EQUIPMENT::EQNUM

			CASE ELSE
				MVALUE = "EL" + EL_EQUIPMENT_ONE::EQNUM

			END SELECT

		END SELECT

		MAINT_GROUP = SB_MAIN_BUDGET(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
