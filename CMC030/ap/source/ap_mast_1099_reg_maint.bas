1	%TITLE "Maintain 1099 Work File"
	%SBTTL "AP_MAST_1099_REG_MAINT"
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
	!	The ^*Maintain 1099 Work File\* option
	!	edits records in the 1099 Work File. This report contains the
	!	following fields:
	!	.table 3,25
	!	.te
	!	Vendor Number	Summary Flag
	!	.te
	!	Vendor Name	Code
	!	.te
	!	Description	Transaction Number
	!	.te
	!	Invoice Number	Invoice Date
	!	.te
	!	Check Number	Check Date
	!	.te
	!	Check Amount	1099 Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Maintain 1099 Work File
	!
	! Option:
	!	AP_MAIN_1099_MAINT$HELP
	!
	! Author:
	!
	!	07/30/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_1099_REG_MAINT
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_MAST_1099_REG_MAINT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_1099_REG_MAINT.OBJ;*
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	01/24/89 - Kevin Handy
	!		Fixed year number passing.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change last parameter on ENTER_3CHOICE from "" to 0%
	!
	!	10/09/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Do an EDIT on YEAR_1099$ before checking its length
	!		(MAP variable is always the same length)
	!
	!	12/06/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_AP_1099_YYYY) &
		AP_1099_YYYY.CH%, &
		AP_1099_YYYY.READONLY%, &
		YEAR_1099$ = 4%

	DIM AP_1099_YYYY_FILE$(100)

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_1099_YYYY", AP_1099_YYYY.DEV$, STAT%)

	!
	! Get info required for main file
	!
300	!
	! Query user for year of file
	!
	CALL FIND_FILE( AP_1099_YYYY.DEV$ + "AP_1099_*.HIS", &
		AP_1099_YYYY_FILE$(), &
		16%, "", "")

	AP_1099_YYYY_FILE% = VAL%(AP_1099_YYYY_FILE$(0%))

	IF AP_1099_YYYY_FILE%
	THEN
		AP_1099_YYYY_FILE$(LOOP%) = &
			MID(AP_1099_YYYY_FILE$(LOOP%), 9%, 4%) &
				FOR LOOP% = 1% TO AP_1099_YYYY_FILE%

		TEMP$ = "AP 1099 Register"

		X% = ENTR_3CHOICE(SCOPE, "", "", AP_1099_YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YEAR_1099$ = EDIT$(AP_1099_YYYY_FILE$(X%), -1%)
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "1099 Year:", 11%, 30%)

320	INP$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, INP$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	YEAR_1099$ = EDIT$(INP$, -1%)

	YEAR_1099$ = LEFT(DATE_TODAY, 2%) + INP$ &
		IF LEN(INP$) = 2%

	IF LEN(EDIT$(YEAR_1099$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the 1099 register year in YYYY format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AP_MAIN_1099_MAINT.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG	FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG	FUNCTION AP_MAIN_1099_TABLE
	EXTERNAL LONG	FUNCTION AP_MAIN_1099_MAINT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_1099_MAINT.ID

		MAINT_GROUP = AP_MAIN_1099_MAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD001
	!	^*(01) Vendor Number\*
	!	.P
	!	The ^*Vendor Number\* field assigns an identification
	!	number of one (1) to ten (10) alphanumeric characters to be used
	!	for referencing a vendor. It is recommended that all vendor numbers
	!	be the same length.
	!
	! Index:
	!	.x Vendor Number
	!	.x Vendor Number
	!	.x Vendor Number
	!
	!--
