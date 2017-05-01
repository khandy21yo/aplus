1	%TITLE "Kill Cycle Count Journal"
	%SBTTL "IC_SPEC_KILLCYCLE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions
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
	! Software Solutions.
	!
	! Software Solutions assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program will remove any unwanted cycle count
	!	journals that have been created, but have not been
	!	posted.
	!	.lm -5
	!
	! Index:
	!	.x Delete Journal
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_KILLCYCLE/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_KILLCYCLE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_KILLCYCLE.OBJ;*
	!
	! Author:
	!
	!	03/27/95 - Kevin Handy
	!
	! Modification history:
	!
	!	04/10/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!		Change last parameter to ENTR_3YESNO from 0% to "".
	!
	!	04/14/95 - Kevin Handy
	!		Fix last parameter to ENTR_3CHOICE.
	!
	!	05/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	11/18/97 - Kevin Handy
	!		Reverse out any balances that have been calculated.
	!		(Assumes coded as CC journal)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!
	!	09/25/2003 - Kevin Handy
	!		Handle files with more than two digits in name
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST) IC_JOURADJUST_CDD IC_JOURADJUST

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_IC_CYCLEJOUR) &
		IC_CYCLEJOUR.CH%, &
		IC_CYCLEJOUR.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_WRIT_35BALANCE

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

300	!
	! Query user for year of file
	!
	CALL READ_DEVICE("IC_CYCLEJOUR", IC_CYCLEJOUR.DEV$, STAT%)
	CALL FIND_FILE(IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_*.JRL", &
		IC_CYCLEJOUR_FILE$(), 16%, "", "")

	IC_CYCLEJOUR_FILE% = VAL%(IC_CYCLEJOUR_FILE$(0%))

	IF IC_CYCLEJOUR_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "There are no open journals", 0%)
		GOTO ExitProgram
	END IF

	FOR LOOP% = 1% TO IC_CYCLEJOUR_FILE%
		LE% = INSTR(1%, IC_CYCLEJOUR_FILE$(LOOP%), ".")
		LE% = LEN(IC_CYCLEJOUR_FILE$(LOOP%)) + 1% &
			IF LE% < 1%
		IC_CYCLEJOUR_FILE$(LOOP%) = &
			SEG$(IC_CYCLEJOUR_FILE$(LOOP%), 14%, LE% - 1%)
	NEXT LOOP%

	TEMP$ = "Cycle Counting Journal Files"

	X% = ENTR_3CHOICE(SCOPE, "", "", IC_CYCLEJOUR_FILE$(), "", &
		0%, TEMP$, "", 0%)

	IF X% > 0%
	THEN
		BATCH_NO$ = EDIT$(IC_CYCLEJOUR_FILE$(X%), -1%)
		GOTO 400
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram
	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Please select the batch number", 0%)
	GOTO 300

400	CONFIRM$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
		"", "Confirm Deleting " + BATCH_NO$, &
		"N", 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram
	END SELECT

	GOTO ExitProgram IF CONFIRM$ <> "Y"

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

 !	KILL IC_CYCLEJOUR.DEV$ + &
 !		"IC_CYCLEJOUR_" + &
 !		BATCH_NO$ + &
 !		".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_" + &
		BATCH_NO$ + ".JRL;*")

2000	!******************************************************************
	! Handle the adjustment file
	!******************************************************************

 !	CALL READ_DEVICE("IC_JOURADJUST", IC_JOURADJUST.DEV$, STAT%)

	!
	! First we need to back out ant adjustments
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.MOD"
		RESET #IC_JOURADJUST.CH%
	USE
		CONTINUE 3000
	END WHEN

2010	WHEN ERROR IN
		GET #IC_JOURADJUST.CH%
	USE
		CONTINUE 2900 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	V% = IC_WRIT_35BALANCE(IC_JOURADJUST::PRODUCT, &
		IC_JOURADJUST::LOCATION, &
		"CC", &
		-IC_JOURADJUST::QUANTITY)

 !	DELETE #IC_JOURADJUST.CH%

	GOTO 2010

2900	CLOSE #IC_JOURADJUST.CH%

	!
	! Kill the journal
	!
 !	KILL IC_JOURADJUST.DEV$ + &
 !		"IC_JOURADJUST_" + &
 !		BATCH_NO$ + &
 !		".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(IC_JOURADJUST.DEV$ + &
		"IC_JOURADJUST_" + BATCH_NO$ + ".JRL;*")

3000	!******************************************************************
	! Handle the count file
	!******************************************************************

	CALL READ_DEVICE("IC_JOURCOUNT", IC_JOURCOUNT.DEV$, STAT%)

 !	KILL IC_JOURCOUNT.DEV$ + &
 !		"IC_JOURCOUNT_" + &
 !		BATCH_NO$ + &
 !		".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(IC_JOURCOUNT.DEV$ + &
		"IC_JOURCOUNT_" + BATCH_NO$ + ".JRL;*")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
