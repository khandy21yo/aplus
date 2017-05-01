1	%TITLE "GL ACCOUNT MASK MAINTENANCE"
	%SBTTL "GL_MAST_OBJECT"
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
	!	The ^*Maintain/Copy Account Object\* routine allows for the entering,
	!	maintaining, and copying of the mask and account object files.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Maintenance
	!	.x Maintain>Mask
	!	.x Add>Mask
	!	.x Change>Mask
	!	.x Erase>Mask
	!	.x Mask>Add
	!	.x Mask>Change
	!	.x Mask>Erase
	!	.x Object>Maintenance
	!	.x Maintain>Object
	!	.x Add>Object
	!	.x Change>Object
	!	.x Erase>Object
	!	.x Object>Add
	!	.x Object>Change
	!	.x Object>Erase
	!
	! Option:
	!
	!	GL_MAIN_MASK$HELP
	!	GL_MAIN_OBJECT$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_OBJECT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_OBJECT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_OBJECT.OBJ;*
	!
	! Author:
	!
	!	06/24/88 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.HB"
	MAP (GL_OBJECT)		GL_OBJECT_CDD	GL_OBJECT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	COM (CH_GL_OBJECT) &
		GL_OBJECT.CH%, &
		GL_OBJECT.READONLY%

	%PAGE

100	!*******************************************************************
	! Initialization section - Prepare to do anything
	!*******************************************************************

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

200	!*******************************************************************
	! Open GL_CHART and GL_OBJECT files
	!*******************************************************************

	!
	! Open Chart file (existing) for modification
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Open Object file (existing) for modification
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.CRE"
	USE
		FILENAME$ = "GL_OBJECT"
		CONTINUE HelpError
	END WHEN

1000	!*******************************************************************
	! Handle main function
	!*******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_MASK.ID, "")

 ExitProgram:
	!******************************************************************
	! Exit GL_MAST_OBJECT
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19990	!******************************************************************
	! End of GL_MAST_OBJECT
	!******************************************************************
	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_MAIN_OBJECT
	EXTERNAL LONG	FUNCTION GL_MAIN_MASK

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Process the Account Object maintenance window
	!
	CASE GL_MAIN_OBJECT.ID

		MAINT_GROUP = GL_MAIN_OBJECT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Process the Account Mask maintenance window
	!
	CASE GL_MAIN_MASK.ID

		MAINT_GROUP = GL_MAIN_MASK(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
