1	%TITLE "Journal Entry"
	%SBTTL "AD_JOUR_JOURNAL"
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
	!	The ^*Journal Entry\* option
	!	maintains Depreciation Units for each file with an assigned batch number.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Journal Entry
	!	.x Maintain>Depreciation Units
	!	.x Batch Number>User
	!	.x User Batch Number
	!
	! Option:
	!	AD_JOUR_JOURNAL$BATCH
	!	AD_MAIN_JOURNAL$HELP
	!	AD_MAIN_JOURNAL$COPY
	!	AD_MAIN_UNITS$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_JOUR_JOURNAL/LINE
	!	$ LINK/EXE=AD_EXE: AD_JOUR_JOURNAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_JOUR_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	12/10/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER
	!
	!	03/18/92 - Dan Perkins
	!		Commented out OPTION$ which is used only once in
	!		entire program.
	!
	!	04/04/95 - Kevin Handy
	!		Update source code to V3.6
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/97 - Kevin Handy
	!		Change 'VAL(' to 'VAL%('
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/07/2000 - Kevin Handy
	!		Lose stupid error trapping (on error goto 19000/
	!		19000 on error goto 0)
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM (CH_AD_JOURNAL) &
		BATCH_NO$ = 2%, &
		AD_JOURNAL.CH%

	COM (CH_AD_UNITS) &
		AD_UNITS.CH%

	COM (CH_AD_UNITS2) &
		AD_UNITS2.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAINT_GROUP

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(AD_UNITS2.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("AD_JOURNAL", AD_JOURNAL.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(AD_JOURNAL.DEV$ + "AD_JOURNAL_*.JRL", &
		AD_JOURNAL_FILE$(), 16%, "", "")

	AD_JOURNAL_FILE% = VAL%(AD_JOURNAL_FILE$(0%))

	IF AD_JOURNAL_FILE%
	THEN
		AD_JOURNAL_FILE$(LOOP%) = &
			MID(AD_JOURNAL_FILE$(LOOP%), 12%, 2%) &
				FOR LOOP% = 1% TO AD_JOURNAL_FILE%

		TEMP$ = "Units Journal Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", AD_JOURNAL_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(AD_JOURNAL_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01BATCH"

	!++
	! Abstract:FLD01BATCH
	!	^*Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch _#\* field will be bypassed if a Batch _# is
	!	selected in the Units Journal File screen.  If a new batch
	!	is to be created, the Batch _# would be entered here.
	!	.lm -5
	!
	! Index:
	!	.x Journal>Batch _#
	!	.x Batch _#>Journal
	!
	!--
	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(AD_MAIN_JOURNAL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION AD_MAIN_JOURNAL
	EXTERNAL LONG FUNCTION AD_MAIN_UNITS
	EXTERNAL LONG FUNCTION AD_MAIN_OBJECT
	EXTERNAL LONG FUNCTION AD_MAIN_ASSET

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_JOURNAL.ID

		MAINT_GROUP = AD_MAIN_JOURNAL(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_UNITS.ID

		MAINT_GROUP = AD_MAIN_UNITS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_OBJECT.ID

		MAINT_GROUP = AD_MAIN_OBJECT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_ASSET.ID

		MAINT_GROUP = AD_MAIN_ASSET(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
