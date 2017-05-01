1	%TITLE "Note Codes Table"
	%SBTTL "PO_MAST_NOTES"
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
	!	The ^*Notes Codes Table\* option
	!	accesses the file where PO Notes are defined.
	!
	! Index:
	!	.x Table>PO Notes
	!	.x PO Notes>Table
	!
	! Option:
	!	PO_MAIN_NOTES$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAST_NOTES/LINE
	!	$ LINK/EXE=PO_EXE: PO_MAST_NOTES,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_MAST_NOTES.OBJ;*
	!
	! Author:
	!
	!	05/31/90 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	!++
	! Abstract:COMMAND
	!	^*NOTES\*
	!	.p
	!	Access the file where PO Notes are defined.
	!	.p
	!	^*Format: NOTES\*
	!
	! Index:
	!	.x PO NOTES
	!
	! Option:
	!	PO_MAIN_NOTES$HELP
	!	PO_RPRT_NOTES$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

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
	V% = MAIN_WINDOW(PO_MAIN_NOTES.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	EXTERNAL LONG FUNCTION PO_MAIN_NOTES

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PO_MAIN_NOTES.ID
		MAINT_GROUP = PO_MAIN_NOTES(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
