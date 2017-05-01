1	%TITLE "Model Master Maintenance"
	%SBTTL "MO_MAST_MODEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Model Master\* screen accesses the
	!	routine where information concerning a particular Model and the
	!	associated Options is maintained.
	!
	! Index:
	!	.x Model>Maintenance
	!	.x Maintenance>Model
	!
	! Option:
	!
	!	MO_MAIN_MODEL$HELP
	!	MO_MAIN_MODEL_LINE$HELP
	!
	! Author:
	!
	!	02/27/91 - Val James Allen
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAST_MODEL
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_MAST_MODEL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_MAST_MODEL.OBJ;*
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

	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"

	!
	! Common Statements
	!
	COM (CH_MO_MODEL) &
		MO_MODEL.CH%, &
		MO_MODEL.READONLY%

	COM (CH_MO_MODELLINE) &
		MO_MODELLINE.CH%, &
		MO_MODELLINE.READONLY%


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

	V% = MAIN_WINDOW(MO_MAIN_MODEL.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.HB"
	MAP (MO_MODEL)		MO_MODEL_CDD		MO_MODEL
	MAP (MO_MODEL_OLD)	MO_MODEL_CDD		MO_MODEL_OLD, MO_MODEL2
	MAP (MO_MODEL_ONE)	MO_MODEL_CDD		MO_MODEL_ONE

	EXTERNAL LONG FUNCTION MO_MAIN_MODEL
	EXTERNAL LONG FUNCTION MO_MAIN_MODEL_LINE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKESIZE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKECLASS
	EXTERNAL LONG FUNCTION MO_MAIN_MODELCODE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION MO_MAIN_OPTGROUP
	EXTERNAL LONG FUNCTION MO_MAIN_OPTION
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE MO_MAIN_MODEL.ID

		MAINT_GROUP = MO_MAIN_MODEL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Option"

		CASE OPT_MOREMENU
			MO_MODEL_ONE = MO_MODEL

			SELECT EDIT$(MVALUE, -1%)

			CASE "OPTION"
				MAINT_GROUP = &
					MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			CASE "Change", "Blank", "Initialize"
				IF MO_MODEL::MODELCODE + &
					MO_MODEL::MSIZE + MO_MODEL::CLASS <> &
					MO_MODEL_OLD::MODELCODE + &
					MO_MODEL_OLD::MSIZE + &
					MO_MODEL_OLD::CLASS
				THEN
					MO_MODEL_ONE = MO_MODEL_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						MO_MAIN_MODEL_LINE.ID, "C")
				END IF

			CASE "Erase"
				MO_MODEL_ONE = MO_MODEL
				MAINT_GROUP = MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "E")

			END SELECT
		END SELECT

		CASE MO_MAIN_MODEL_LINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = MO_MODEL_ONE::MODELCODE + &
				MO_MODEL_ONE::MSIZE + MO_MODEL_ONE::CLASS

		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = MO_MODEL::MODELCODE + &
					MO_MODEL::MSIZE + MO_MODEL::CLASS
			CASE ELSE
				MVALUE = MO_MODEL_ONE::MODELCODE + &
					MO_MODEL_ONE::MSIZE + &
					MO_MODEL_ONE::CLASS
			END SELECT
		END SELECT

		MAINT_GROUP = MO_MAIN_MODEL_LINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_MAKESIZE.ID

		MAINT_GROUP = MO_MAIN_MAKESIZE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_MAKECLASS.ID

		MAINT_GROUP = MO_MAIN_MAKECLASS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_MODELCODE.ID

		MAINT_GROUP = MO_MAIN_MODELCODE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_OPTGROUP.ID

		MAINT_GROUP = MO_MAIN_OPTGROUP(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_OPTION.ID

		MAINT_GROUP = MO_MAIN_OPTION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:MODEL_OPTION
	!	^*MODEL - Model/Option\*
	!	.p
	!	The ^*Model/Option\* screen accesses the
	!	routine where information concerning a particular Model and
	!	its' associated Options is maintained.
	!	.p
	!	The ^*Model Master\* screen contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Model Code
	!	.le
	!	(02) Make Size
	!	.le
	!	(03) Make Class
	!	.le
	!	(04) Inventory Product Number
	!	.els
	!	.p
	!	.lm -10
	!	The ^*Options\* portion
	!	maintains attachment of Options to a particular Model.  The
	!	Options window is displayed by accessing the "Option" function in the
	!	COMMAND menu.
	!	.p
	!	The ^*Options\* window contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Option Group Code
	!	.le
	!	(02) Option Code
	!	.els
	!
	! Index:
	!	.x Maintenance>Model
	!	.x Model>Maintenance
	!
	!--
