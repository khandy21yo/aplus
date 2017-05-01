1	%TITLE "MAINT - Maintain Journal Entries"
	%SBTTL "GL_MAST_GJMNT"
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
	!	.B
	!	.LM +5
	!	This routine will accommodate entry and maintenance of Regular,
	!	Recurring, Reversing, and Recurring/Reversing type
	!	General Journals.
	!
	! Index:
	!	.x General Journal>Regular
	!	.x General Journal>Recurring
	!	.x General Journal>Reversing
	!	.x Regular>General Journal
	!	.x Recurring>General Journal
	!	.x Reversing>General Journal
	!	.x Maintain>Journal Entries
	!	.x Journal Entries>Maintenance
	!
	! Option:
	!	GL_MAST_GJMNT$JNAME
	!	GL_MAIN_GJ_LINE$HELP
	!	GL_MAIN_GJ_LINE$JOURNAL_TOTAL
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_GJMNT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_GJMNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_GJMNT.OBJ;*
	!
	! Author:
	!
	!	04/30/87 - Kevin Handy
	!
	! Modification History:
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changed in ENTR_ENTER.
	!
	!	10/05/90 - Kevin Handy
	!		Added Recurring/Reversing journal type.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/14/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/01/2000 - Kevin Handy
	!		Lose useless error trap
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

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_GJ_LINE) &
		GL_GJ_LINE.CH%, &
		GL_GJ_LINE.READONLY%
	COM (TT_GL_GJ_LINE) &
		JRL_TYPE$ = 1%, &
		GL_GJ_LINE.JOURNAL$ = 20%

	%PAGE

100	!*****************************************************************
	! Initialization section - Prepare to do anything
	!*****************************************************************

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	GL_GJ_LINE.CH% = -4%

	SCOPE::PRG_ITEM = "FLD001"

200	!*****************************************************************
	! Ask for a batch number
	!*****************************************************************

	!
	! Paint screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"1=Regular, 2=Recurring, 3=Reversing, 4=Recurring/Reversing", &
		6%, 18%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Select Journal type - Then Press <DO> ", 8%, 24%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

230	!
	! Get the journal name
	!
	JRL_TYPE$ = " " &

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		8%, 62%, JRL_TYPE$, -1%, 16%)

	!++
	!
	! Abstract:FLD001
	!	.x General Journal>Journal Type
	!	^*Select - Journal Types\*
	!	.b
	!	.lm +5
	!	Valid types are:
	!	.TABLE 3,25
	!	.TE
	!	^*1\* - Regular Journal
	!	.TE
	!	^*2\* - Recurring Journal
	!	.TE
	!	^*3\* - Reversing Journal
	!	.TE
	!	^*4\* - Recurring/Reversing Journal
	!	.end table
	!	An entry is required.
	!	.lm -5
	!
	! Index:
	!	.x Journal Type>Regular
	!	.x Journal Type>Recurring
	!	.x Journal Type>Reversing
	!	.x Journal Type>Recurring/Reversing
	!
	!--

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 230
	END SELECT

	SELECT JRL_TYPE$

	CASE "1"
		GL_GJ_LINE.JOURNAL$ = "General"

	CASE "2"
		GL_GJ_LINE.JOURNAL$ = "Recurring"

	CASE "3"
		GL_GJ_LINE.JOURNAL$ = "Reversing"

	CASE "4"
		GL_GJ_LINE.JOURNAL$ = "Recurring/Reversing"

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Please enter 1, 2, 3, or 4", 0%)
		GOTO 230
	END SELECT

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_GJ_LINE.ID, "")

	!******************************************************************
	! Exit GL_MAST_GJMNT
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	!******************************************************************
	! End of GL_MAST_GJMNT
	!******************************************************************
	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_MAIN_CHART
	EXTERNAL LONG	FUNCTION GL_MAIN_GJ_LINE

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
	! Process the Chart of Accounts maintenance window
	!
	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Process the General Journal maintenance window
	!
	CASE GL_MAIN_GJ_LINE.ID
		MAINT_GROUP = GL_MAIN_GJ_LINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
	!+-+-+
	!++
	! Abstract:JNAME
	!	.x General Journal>Journal Type
	!	Select: ^*Journal Types\*
	!	.lm +5
	!	.b
	!	Valid types are:
	!	.table 3,25
	!	.te
	!	^*1\* - Regular Journal
	!	.te
	!	^*2\* - Recurring Journal
	!	.te
	!	^*3\* - Reversing Journal
	!	.te
	!	^*4\* - Recurring/Reversing Journal
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Journal Type>Regular
	!	.x Journal Type>Recurring
	!	.x Journal Type>Reversing
	!	.x Journal Type>Recurring/Reversing
	!
	!--
