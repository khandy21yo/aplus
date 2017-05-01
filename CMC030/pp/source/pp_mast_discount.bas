1	%TITLE "Pacific Pride Discount Maintenance"
	%SBTTL "PP_MAST_DISCOUNT"
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
	!	The ^*Discount Maintenance\* option
	!	accesses the file where the Pacific Pride Discount records are
	!	maintained.  Data in this file includes a discount code,
	!	discount description, discount method, and a discount table.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	PP_MAIN_DISCOUNT$HELP
	!
	! Compile:
	!
	!	$ BAS SOURCE:[PP.SOURCE]PP_MAST_DISCOUNT/LINE
	!	$ LINK/EXEC:PP_EXE PP_MAST_DISCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_MAST_DISCOUNT.OBJ;*
	!
	! Author:
	!
	!	12/21/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
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

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PP_MAIN_DISCOUNT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"

	EXTERNAL LONG FUNCTION PP_MAIN_DISCOUNT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PP_MAIN_DISCOUNT.ID

		MAINT_GROUP = PP_MAIN_DISCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
