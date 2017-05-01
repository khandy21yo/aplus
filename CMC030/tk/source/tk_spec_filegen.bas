1	%TITLE "Proto Type a New File"
	%SBTTL "TK_SPEC_FILEGEN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!	.P
	!	The Files maintenance application is an inventory management
	!	package used to maintain the inventory of files.  It will
	!	track the file name, the different types of opens that are
	!	used, and the file layout.  All of the file opens and the
	!	file layout can be maintained by using the window option.
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	08/17/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_FILEGEN
	!	$ LINK/EXEC=TK_EXE:*.EXE TK_SPEC_FILEGEN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_FILEGEN.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/27/95 - Kevin Handy
	!		Move modification history section to bottom of
	!		the comment.
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/12/99 - Kevin Handy
	!		Don't disable broadcast trapping
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

 !	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(TK_MAIN_FILEDICT.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	RESUME HelpError

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	RESUME ExitProgram

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	EXTERNAL LONG	FUNCTION TK_MAIN_FILEDICT
	EXTERNAL LONG   FUNCTION TK_MAIN_ELEMENT
	EXTERNAL LONG   FUNCTION TK_MAIN_FOREIGN

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE TK_MAIN_FILEDICT.ID

		MAINT_GROUP = TK_MAIN_FILEDICT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE TK_MAIN_ELEMENT.ID

		MAINT_GROUP = TK_MAIN_ELEMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE TK_MAIN_FOREIGN.ID

		MAINT_GROUP = TK_MAIN_FOREIGN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
