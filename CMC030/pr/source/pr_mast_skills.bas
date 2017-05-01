1	%TITLE "Maintain Skills Table"
	%SBTTL "PR_MAST_SKILLS"
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
	! or reliability ofits software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.p
	!	The ^*Maintain Skills Table\* defines skills.
	!
	! Index:
	!	.x Skills>Table
	!	.x Tables>Skills
	!
	! Option:
	!	PR_MAIN_SKILLS$HELP
	!
	! Author:
	!
	!	09/09/96 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_SKILLS
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_MAST_SKILLS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_SKILLS.OBJ;*
	!
	! Modification history:
	!
	!	09/23/97 - Kevin Handy
	!		Fix titles/help from 'operation' to 'skill'.
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

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

400	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_SKILLS.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PR_MAIN_SKILLS

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Process the Skills maintenance window
	!
	CASE PR_MAIN_SKILLS.ID
		MAINT_GROUP = PR_MAIN_SKILLS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
