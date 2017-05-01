1	%TITLE "Price Type"
	%SBTTL "PC_MAST_PRCTYPE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Price Type\* option
	!	accesses the routine where records are entered and
	!	maintained relative to various price types.
	!	.b
	!	For example, there could be different prices for different geographical
	!	locations, therefore, there might be price types such as "AK" for Alaska,
	!	"HI" for Hawaii, or "CA" for California.
	!	.b
	!	Price Types can be alphanumerical.
	!	.lm -5
	!
	! Index:
	!	.x Price>Type
	!	.x Cost>Type
	!	.x Type>Price _& Cost
	!
	! Option:
	!	PC_MAIN_PRCTYPE$HELP
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAST_PRCTYPE/LINE
	!	$ LINK/EXE=PC_EXE: PC_MAST_PRCTYPE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_MAST_PRCTYPE.OBJ;*
	!
	! Author:
	!
	!	07/23/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"

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

	V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PC_MAIN_PRCTYPE.ID
		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
