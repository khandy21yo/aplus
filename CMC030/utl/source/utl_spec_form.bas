1	%TITLE "Edit Forms"
	%SBTTL "UTL_SPEC_FORM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	The ^*Edit Forms\* maintains
	!	specific forms to fit the needs of the user.
	!	.lm -5
	!
	! Index:
	!	.x Edit Forms
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_SPEC_FORM/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_SPEC_FORM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_SPEC_FORM.OBJ;*
	!
	! Author:
	!
	!	03/02/88 - Kevin Handy
	!
	! Modification history:
	!
	!	06/27/90 - Frank F. Starman
	!		Replace HELP_3MESSAGE WITH HELP_34MESSAGE function.
	!
	!	01/13/93 - Kevin Handy
	!		Added Getmaster and Putmaster options.
	!
	!	06/17/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	07/16/99 - Kevin Handy
	!		Take quotes off of status result in call to
	!		entr_message call.
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

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! External functions
	!
	EXTERNAL STRING FUNCTION LIBR_SELECT
	EXTERNAL LONG READ_3BROADCAST

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	ASK$ = "N"
	TEMP$ = SYS('7'C)
	IF ASCII(TEMP$) = 14%
	THEN
		TEMP1% = INSTR(1%, TEMP$, '15'C)
		IF TEMP1%
		THEN
			CUR_SYS$ = SEG$(TEMP$, 2%, TEMP1% - 1%)
			REPORT$  = EDIT$(RIGHT(TEMP$, TEMP1% + 1%), -1%)
		ELSE
			ASK$ = "Y"
		END IF

	ELSE
		ASK$ = "Y"
	END IF

	IF ASK$ = "Y"
	THEN
		PRINT "Curent System: "; CUR_SYS$; "Report: "; REPORT$;
		INPUT REP$
		REPORT$ = REP$ IF REP$ <> ""
	END IF

	CALL  READ_DEVICE(REPORT$, SYS_FORM.DEV$, STAT%)

 FileExists:
	IF FIND_FILEEXISTS(SYS_FORM.DEV$ + REPORT$ + ".TLB", 0%) = 0%
	THEN
		SCOPE::PRG_ITEM = "CREATE_" + REPORT$

		INP$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			"Confirm creation of new " + LEFT(REPORT$, 2) + &
			" forms file - then press <Do> ", &
			"N", 0%, "", ""), -1%)

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO FileExists

		END SELECT

		IF INP$ <> "Y"
		THEN
			GOTO ExitProgram
		END IF

		CALL ENTR_3MESSAGE(SCOPE, &
			"Creating " + LEFT(REPORT$, 2%) + " forms file", 1%)

		IF FIND_FILEEXISTS("CMC:" + REPORT$ + ".TLB", 0%) = 0%
		THEN
			SCOPE::PRG_ITEM = "MISSING_" + REPORT$

			CALL ENTR_3MESSAGE(SCOPE, LEFT(REPORT$, 2%) + &
				" form on CMC is missing", 0%)

			GOTO FormEdit
		END IF

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

		SMG_STATUS% = LIB$SPAWN("COPY " + &
			"CMC:" + REPORT$ + ".TLB " + &
			SYS_FORM.DEV$ + REPORT$ + ".TLB")

		IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
		THEN
			CALL HELP_34MESSAGE(SCOPE, "unable to copy file", &
				"F", SCOPE::PRG_PROGRAM, &
				NUM1$(SMG_STATUS%), "COPY")
			GOTO ExitProgram
		END IF

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST), LOC(SCOPE))

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
	END IF

 FormEdit:
	!***************************************************
	! Edit REPORT forms
	!***************************************************
	TEMP$ = LIBR_SELECT(SYS_FORM.DEV$ + REPORT$, &
		LEFT(REPORT$, 2%) + " Forms", SCOPE::PRG_PROGRAM, &
		"Maintain Create Getmaster Putmaster Help eXit")

	!++
	! Abstract:MAINTAIN
	!	^*Maintain\*
	!	.b
	!	.lm +5
	!	The ^*Maintain\* allows for viewing and modifying
	!	of an existing form.
	!	.lm -5
	!
	! Index:
	!	.x Form>Maintain
	!	.x Maintain>Form
	!
	!--

	!++
	! Abstract:CREATE_AP_FORM
	!	^*Create new AP form file\*
	!	.p
	!	This function creates a new forms file by copying the
	!	accounts payable forms template from the logical device
	!	CMC.  Once this file has been copied, the user can
	!	modify the various form drives to match the forms.
	!	.p
	!	Confirm creating a new form file by typing a ^*Y\*, or abort
	!	by typing an ^*N\*.
	!
	! Index:
	!
	!--

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
