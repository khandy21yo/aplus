1	%TITLE "Option Master File"
	%SBTTL "MO_MAST_OPTGROUP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991, BY
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
	!	Pertinent information concerning the Option Group can be entered
	!	and maintained by accessing the ^*Option Master File\* option.
	!
	! Index:
	!
	! Option:
	!
	!	MO_MAIN_OPTGROUP$HELP
	!	MO_MAIN_OPTION$HELP
	!
	! Author:
	!
	!	02/27/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAST_OPTGROUP
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_MAST_OPTGROUP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_MAST_OPTGROUP.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP	(MO_OPTGROUP)		MO_OPTGROUP_CDD	MO_OPTGROUP
	MAP	(MO_OPTGROUP_OLD)	MO_OPTGROUP_CDD	MO_OPTGROUP_OLD
	MAP	(MO_OPTGROUP_ONE)	MO_OPTGROUP_CDD	MO_OPTGROUP_ONE

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION MO_MAIN_OPTGROUP
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION MO_MAIN_OPTION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE MO_MAIN_OPTGROUP.ID

		MAINT_GROUP = MO_MAIN_OPTGROUP(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Option"

		CASE OPT_MOREMENU
			MO_OPTGROUP_ONE = MO_OPTGROUP
			SELECT EDIT$(MVALUE, -1%)

			CASE "OPTION"
				MAINT_GROUP = &
					MAIN_WINDOW(MO_MAIN_OPTION.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			CASE "Change", "Blank", "Initialize"
				IF MO_OPTGROUP::OPTGROUP <> &
					MO_OPTGROUP_OLD::OPTGROUP
				THEN
					MO_OPTGROUP_ONE = MO_OPTGROUP_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						MO_MAIN_OPTION.ID, "C")
				END IF

			CASE "Erase"
				MO_OPTGROUP_ONE = MO_OPTGROUP
				MAINT_GROUP = MAIN_WINDOW (MO_MAIN_OPTION.ID, "E")

			END SELECT
		END SELECT


	CASE MO_MAIN_OPTION.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = MO_OPTGROUP_ONE::OPTGROUP
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = MO_OPTGROUP::OPTGROUP
			CASE ELSE
				MVALUE = MO_OPTGROUP_ONE::OPTGROUP
			END SELECT
		END SELECT

		MAINT_GROUP = MO_MAIN_OPTION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
