1	%TITLE "Purchase Order Control File Maintenance"
	%SBTTL "PO_CNTR_CONTROL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	!	The ^*Purchase Order Control File Maintenance\*
	!	establishes the "Last Purchase Order"
	!	and Codes to identify the Purchase Order Transaction Types
	!	and Receive Transaction Types. Once this file is set up, it
	!	normally is not necessary to access it again.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Control Record
	!	.x Control Record>Maintain
	!	.x Maintain>Control
	!	.x Control>Maintain
	!
	! Option:
	!
	!	PO_MAIN_CONTROL$HELP
	!
	! Author:
	!
	!	07/23/88 - Aaron Redd
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_CNTR_CONTROL/LINE
	!	$ LINK/EXEC = PO_EXE:*.EXE PO_CNTR_CONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_CNTR_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	02/01/92 - Frank F. Starman
	!		Remove UTL_TRANSTYPE
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Define PO_MAIN_CONTROL in PO_WINDOW.INC
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
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

	%INCLUDE"FUNC_INCLUDE:PO_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!******************************************************************
	! Initialize maintainence
	!******************************************************************

	CALL READ_INITIALIZE

	!******************************************************************
	! Handle the main file
	!******************************************************************

	V% = MAIN_WINDOW(PO_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE"FUNC_INCLUDE:PO_WINDOW.INC"

	EXTERNAL LONG	FUNCTION PO_MAIN_CONTROL

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PO_MAIN_CONTROL.ID
		MAINT_GROUP = PO_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
