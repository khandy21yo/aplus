1	%TITLE "Journal Entry"
	%SBTTL "IC_JOUR_JOURNAL"
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
	!	The ^*Journal Entry\* option
	!	maintains journal transactions.
	!	.b
	!	After selecting this option, a screen will appear for the selection
	!	of an existing batch number. If a new batch number is to be created,
	!	press ^*Return\* to bypass the existing batch number screen. A new screen
	!	will appear accommodating entry of a new batch number.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Journal>Maintenance
	!	.x Transaction Journal>Add
	!	.x Transaction Journal>Erase
	!	.x Transaction Journal>Change
	!	.x Maintain>Transaction Journal
	!	.x Add>Transaction Journal
	!	.x Erase>Transaction Journal
	!	.x Change>Transaction Journal
	!
	! Option:
	!
	!	IC_MAIN_JOURNAL$HELP
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_JOUR_JOURNAL/LINE
	!	$ LINK/EXE=IC_EXE: IC_JOUR_JOURNAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_JOUR_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	09/11/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/29/87 - Frank F. Starman
	!		Added IC_JOURNAL::OPERATION field
	!
	!	04/12/88 - Frank F. Starman
	!		Changed file layout
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	11/27/91 - Dan Perkins
	!		Added Unit of Measure Display to
	!		QTY A field.
	!
	!	12/17/91 - Dan Perkins
	!		Added a TO LOCATION field to allow transfers
	!		from one location to another in same record.
	!		This is a change in file layout as well.
	!
	!	12/26/91 - Dan Perkins
	!		Changed unit price to cost.  Default cost to
	!		product and location.
	!
	!	01/03/92 - Frank F. Starman
	!		Add IC_WRIT_35BALANCE function.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	03/24/92 - Dan Perkins
	!		Added formating option to qunatity fields.
	!		If COST is hard defaulted we don't get cost
	!		from PC COST file.
	!
	!	02/12/93 - Dan Perkins
	!		Display inventory quantities on the screen.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/29/94 - Kevin Handy
	!		Changed cost field from two digits to five,
	!		to match the cost file.
	!
	!	02/28/94 - Kevin Handy
	!		Create dimension for IC_JOURNAL_FILE$()
	!
	!	03/24/95 - Kevin Handy
	!		Added additional keys to file. Primary_ref,
	!		Cross_ref, Subaccount.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter of entr_3choice.
	!
	!	08/11/95 - Kevin Handy
	!		Reformat select statements. Was really weird.
	!
	!	08/14/95 - Kevin Handy
	!		Open chart of accounts read/only.
	!
	!	01/26/96 - Kevin Handy
	!		Changed STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	10/12/97 - Kevin Handy
	!		Clean out old commented out code
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		TRM$ the BATCH_NO$ before taking its length
	!
	!	06/09/2000 - Kevin Handy
	!		Lose MAP for UTL_ACCOUNT, which is never used.
	!		Use WHEN ERROR IN
	!--

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_IC_JOURNAL) &
		BATCH_NO$ = 2%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAINT_GROUP

	DIM IC_JOURNAL_FILE$(100%)

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Set up for help
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "IC_JOUR_JOURNAL"
	SCOPE::PRG_ITEM = ""

	!
	! Get info required for main file
	!
	CALL READ_DEVICE("IC_JOURNAL", IC_JOURNAL.DEV$, STAT%)

290	!
	! Open chart of accounts read/only, so it doesn't get open
	! read/write later.
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 300
	END WHEN

	GL_CHART.READONLY% = -1%

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(IC_JOURNAL.DEV$ + "IC_JOURNAL_*.JRL", &
		IC_JOURNAL_FILE$(), 16%, "", "")

	IC_JOURNAL_FILE% = VAL%(IC_JOURNAL_FILE$(0%))

	IF IC_JOURNAL_FILE%
	THEN
		IC_JOURNAL_FILE$(LOOP%) = &
			MID(IC_JOURNAL_FILE$(LOOP%), 12%, 2%) &
				FOR LOOP% = 1% TO IC_JOURNAL_FILE%

		TEMP$ = "Inventory Journal Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", IC_JOURNAL_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(IC_JOURNAL_FILE$(X%), -1%)
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	BATCH_NO$ = "01"
	SCOPE::PRG_ITEM = "BATCHNUMBER"
	!++
	! Abstract:BATCHNUMBER
	!	^*Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field allows the user to create a new batch for entry.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		11%, 43%, BATCH_NO$, -1%, 0%)

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
	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(IC_MAIN_JOURNAL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_TRANSTYPE
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PD_MAIN_ACCOUNT
	EXTERNAL LONG FUNCTION IC_MAIN_JOURNAL

	%PAGE

	ON ERROR GOTO 29000

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE IC_MAIN_JOURNAL.ID

		MAINT_GROUP = IC_MAIN_JOURNAL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_TRANSTYPE.ID

		MAINT_GROUP = UTL_MAIN_TRANSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

 !	CASE UTL_MAIN_ACCOUNT.ID
 !
 !		MAINT_GROUP = UTL_MAIN_ACCOUNT(SMG_WINDOW, &
 !			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_ACCOUNT.ID

		MAINT_GROUP = PD_MAIN_ACCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
