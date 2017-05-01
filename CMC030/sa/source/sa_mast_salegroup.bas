1	%TITLE "Broker and Salesman Master"
	%SBTTL "SA_MAST_SALEGROUP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Broker Group>Maintain
	!	.x Salesman Group>Maintain
	!	.x Maintain>Broker Group
	!	.x Maintain>Salesman Group
	!
	! Option:
	!
	!	SA_MAIN_SALESMAN$HELP
	!	SA_MAIN_BUDGET$HELP
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAST_SALEGROUP/LINE
	!	$ LINK/EXE=SA_EXE: SA_MAST_SALEGROUP,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_MAST_SALEGROUP.OBJ;*
	!
	! Author:
	!
	!	06/27/96 - Kevin Handy
	!
	! Modification history:
	!
	!	10/02/96 - Kevin Handy
	!		Added lookup into salesman table
	!
	!	08/22/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"

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

	V% = MAIN_WINDOW(SA_MAIN_SALEGROUP.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION SA_MAIN_SALEGROUP
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION SA_MAIN_TYPE
	EXTERNAL LONG FUNCTION SA_MAIN_CLASS
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE SA_MAIN_SALEGROUP.ID

		MAINT_GROUP = SA_MAIN_SALEGROUP(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID

		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_TYPE.ID

		MAINT_GROUP = SA_MAIN_TYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_CLASS.ID

		MAINT_GROUP = SA_MAIN_CLASS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
