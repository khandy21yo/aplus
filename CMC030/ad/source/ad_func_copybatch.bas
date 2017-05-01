1	%TITLE "Copy Unit of Production Journal"
	%SBTTL "AD_FUNC_COPYBATCH"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_FUNC_COPYBATCH
	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function copies the units of production worksheet
	!	batch to the units depreciation journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_FUNC_COPYBATCH/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_FUNC_COPYBATCH
	!	$ DELETE AD_FUNC_COPYBATCH.OBJ;*
	!
	! Author:
	!
	!	12/11/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 standards.
	!		Define SMG_COPY
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::Scope_exit.
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Lose HelpError routine, which was never called
	!
	!	06/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Trap more errors.
	!		Display failure messages.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Map statements
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.HB"
	MAP (AD_JOURNAL)	AD_JOURNAL_CDD	AD_JOURNAL

	%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.HB"
	MAP (AD_UNITS)	AD_UNITS_CDD	AD_UNITS

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
	DECLARE LONG SMG_COPY

	%PAGE

	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_COPY, &
		SMG$M_BORDER &
	)

300	!
	! Get info required for main file
	!
	GOTO 305 IF AD_UNITS.CH% > 0%

	CALL ASSG_CHANNEL(AD_UNITS.CH%, STAT%)
	IF STAT%
	THEN
		AD_FUNC_COPYBATCH = 1%
		GOTO ExitFunction
	END IF

305	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.CRE"
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Copy Failed op open AD_UNITS", 1%)
		CONTINUE ExitFunction
	END WHEN

310	!
	! Open Batch template file
	!
	!======================================================================
	! AD_UNITS file (open read only)
	!======================================================================

	FLAG% = 0%
	BATCH$ = ENTR_3STRING(SCOPE, SMG_COPY, "23;05", "Batch number ", &
		SPACE$(2%), FLAG%, "'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	END SELECT

	STORE$ = ENTR_3STRING(SCOPE, SMG_COPY, "23;05", "Object ", &
		SPACE$(20%), FLAG%, "'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	END SELECT

	WHEN ERROR IN
		OPEN AD_UNITS.DEV$ + "AD_UNITS_" + BATCH$ + ".JRL" &
			FOR INPUT AS FILE AD_UNITS2.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AD_UNITS, &
			PRIMARY KEY &
			( &
				AD_UNITS::DEP_OBJECT, &
				AD_UNITS::ACTION_DATE &
			)	DUPLICATES, &
			ACCESS READ, ALLOW MODIFY
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Copy Failed on open on AD_UNITS", 1%)
		CONTINUE ExitFunction
	END WHEN

500	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_COPY, &
		"Copying Batch " + BATCH$ &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Asset# ", 6%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Object ", 7%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Copy
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Copying ... ", 1%)

1010	!
	! Get info required for main file
	!
	WHEN ERROR IN
		RESET #AD_UNITS2.CH%
	USE
		CONTINUE 1500
	END WHEN

1020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_UNITS2.CH%, REGARDLESS
	USE
		CONTINUE 1500
	END WHEN

	GOTO 1020 IF COMP_STRING(EDIT$(AD_UNITS::DEP_OBJECT, -1%), STORE$) = 0% &
		AND STORE$ <> "" OR &
		AD_JOURNAL::DEP_OBJECT = AD_UNITS::DEP_OBJECT

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, AD_UNITS::ASSET_NUM, &
		6%, 15%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, AD_UNITS::DEP_OBJECT, &
		7%, 15%)

1040	!
	! Add copy from AD_UNITS (BATCH)
	!
	AD_UNITS::DEP_OBJECT   = AD_JOURNAL::DEP_OBJECT
	AD_UNITS::ACTION_DATE  = AD_JOURNAL::ACTION_DATE

	PUT #AD_UNITS.CH%

	GOTO 1020

	%PAGE

1500	CLOSE AD_UNITS2.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Copy completed ", 1%)
	AD_FUNC_COPYBATCH = 0%

 ExitFunction:

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_COPY)

32767	END FUNCTION
