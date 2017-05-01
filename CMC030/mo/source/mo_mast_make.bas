1	%TITLE "Make Master Maintenance"
	%SBTTL "MO_MAST_MAKE"
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
	!	The ^*Make Master\* screen accesses the
	!	routine where information concerning a particular Make and the
	!	associated Model(s) is maintained.
	!
	! Index:
	!	.x Make>Maintenance
	!	.x Maintenance>Make
	!
	! Option:
	!
	!	MO_MAIN_MAKE$HELP
	!	MO_MAIN_MAKE_LINE$HELP
	!
	! Author:
	!
	!	02/27/91 - Val James Allen
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAST_MAKE
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_MAST_MAKE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_MAST_MAKE.OBJ;*
	!
	! Modification history:
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
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
	COM (CH_MO_MAKE) &
		MO_MAKE.CH%, &
		MO_MAKE.READONLY%

	COM (CH_MO_MAKELINE) &
		MO_MAKELINE.CH%, &
		MO_MAKELINE.READONLY%

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

	V% = MAIN_WINDOW(MO_MAIN_MAKE.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP (MO_MAKE)		MO_MAKE_CDD		MO_MAKE
	MAP (MO_MAKE_OLD)	MO_MAKE_CDD		MO_MAKE_OLD, MO_MAKE2
	MAP (MO_MAKE_ONE)	MO_MAKE_CDD		MO_MAKE_ONE

	EXTERNAL LONG FUNCTION MO_MAIN_MAKE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKE_LINE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKETYPE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKESIZE
	EXTERNAL LONG FUNCTION MO_MAIN_MAKECLASS
	EXTERNAL LONG FUNCTION MO_MAIN_MODELCODE
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE MO_MAIN_MAKE.ID

		MAINT_GROUP = MO_MAIN_MAKE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Models"

		CASE OPT_MOREMENU
			MO_MAKE_ONE = MO_MAKE

			SELECT EDIT$(MVALUE, -1%)

			CASE "MODELS"
				MAINT_GROUP = &
					MAIN_WINDOW(MO_MAIN_MAKE_LINE.ID,"")

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Add records
			!
			CASE "Add"
				!
				! Add line items also
				!
				MAINT_GROUP  = &
					MAIN_WINDOW(MO_MAIN_MAKE_LINE.ID, "A")

			CASE "Change", "Blank", "Initialize"
				IF MO_MAKE::MAKE + MO_MAKE::YEAR + &
					MO_MAKE::MTYPE + MO_MAKE::MSIZE <> &
					MO_MAKE_OLD::MAKE + MO_MAKE_OLD::YEAR + &
					MO_MAKE_OLD::MTYPE + &
					MO_MAKE_OLD::MSIZE
				THEN
					MO_MAKE_ONE = MO_MAKE_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						MO_MAIN_MAKE_LINE.ID, "C")
				END IF

			CASE "Erase"
				MO_MAKE_ONE = MO_MAKE
				MAINT_GROUP = MAIN_WINDOW (MO_MAIN_MAKE_LINE.ID, "E")

			END SELECT

		END SELECT

	CASE MO_MAIN_MAKE_LINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = MO_MAKE_ONE::MAKE + &
				MO_MAKE_ONE::YEAR + &
				MO_MAKE_ONE::MTYPE + &
				MO_MAKE_ONE::MSIZE

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = MO_MAKE::MAKE + &
					MO_MAKE::YEAR + &
					MO_MAKE::MTYPE + &
					MO_MAKE::MSIZE

			CASE ELSE
				MVALUE = MO_MAKE_ONE::MAKE + &
					MO_MAKE_ONE::YEAR + &
					MO_MAKE_ONE::MTYPE + &
					MO_MAKE_ONE::MSIZE
			END SELECT

		END SELECT

		MAINT_GROUP = MO_MAIN_MAKE_LINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_MAKETYPE.ID

		MAINT_GROUP = MO_MAIN_MAKETYPE(SMG_WINDOW, &
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
	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:MAKE_MODEL
	!	^*MAKE - Make/Model\*
	!	.p
	!	The ^*Make/Model\* screen accesses the
	!	routine where information concerning particular Make and
	!	its' associated Model(s) is maintained.
	!	.p
	!	The ^*Make Master\* screen contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Make
	!	.le
	!	(02) Year
	!	.le
	!	(03) Type
	!	.le
	!	(04) Size
	!	.le
	!	(05) Class
	!	.le
	!	(06) Cut Tubing
	!	.le
	!	(07) Front Slant
	!	.le
	!	(08) Overall Length
	!	.le
	!	(09) Narrow Front Flag
	!	.le
	!	(10) Narrow Back Flag
	!	.els
	!	.p
	!	.lm -10
	!	The ^*Model\* portion
	!	maintains attachment of Models to a particular Make.  The
	!	Model window is displayed by accessing the "Model" function in the
	!	COMMAND menu.
	!	.p
	!	The ^*Model\* window contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Model
	!	.le
	!	Model Description
	!	.els
	!
	! Index:
	!	.x Maintenance>Make
	!	.x Make>Maintenance
	!
	!--
