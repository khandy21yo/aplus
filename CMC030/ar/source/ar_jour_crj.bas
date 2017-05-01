1	%TITLE "Cash Receipt Journal Maintenance"
	%SBTTL "AR_JOUR_CRJ"
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
	!	The ^*Maintain Cash Receipts Journal\* option
	!	maintains cash receipt transactions. After selecting
	!	this option, a screen will appear for selection of a batch number.
	!	If a new batch number is to be created, press ^*Do\* to bypass the
	!	existing batch number screen. A screen will appear accommodating
	!	entry of a new batch number.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Maintenance
	!	.x Maintenance>Cash Receipts
	!	.x Journal>Cash Receipts
	!	.x Cash Receipts>Journal
	!
	! Option:
	!	AR_MAIN_CRJ$HELP
	!	AR_MAIN_CRJ_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_JOUR_CRJ/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_JOUR_CRJ,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_JOUR_CRJ.OBJ;*
	!
	! Author:
	!
	!	02/17/88 - Aaron Redd
	!
	! Modification history:
	!
	!	03/11/88 - Kevin Handy
	!		Pull customer/patient/client from control file.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	11/09/92 - Dan Perkins
	!		Added functions from AR_MAST_35CUSTOM so F-17 key will
	!		work when trying to add a new customer.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change last param of entr_3choices from "" to 0%.
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/17/2000 - Kevin Handy
	!		Open chart of accounts in R/O mode so that it
	!		won't lock out other postings/updates.
	!
	!	02/11/2002 - Kevin Handy
	!		Added form stuff
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

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

	!
	! Common statements
	!
	COM (CH_AR_CONTROL) &
		AR_CONTROL.CH%

	COM (CH_AR_CRJH) &
		AR_CRJH.CH%

	COM (CH_AR_CRJL) &
		AR_CRJL.CH%

	COM (TT_AR_CRJ) &
		BATCH_NO$ = 2%

	!
	! Dimension statements
	!
	DIM AR_CRJH_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

200	!
	! Read in information from control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpFile
	END WHEN

210	!
	! Open GL_CHART in such a way it doesn't lock out everything
	! else
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpFile
	END WHEN

300	!******************************************************************
	! Get batch number
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_CRJH", AR_CRJH.DEV$, STAT%)

	!
	! Find out what is already there
	!
	CALL FIND_FILE(AR_CRJH.DEV$ + "AR_CRJH_*.JRL", AR_CRJH_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	!
	! If any files are in the list, then do a query screen
	!
	AR_CRJH_FILE% = VAL%(AR_CRJH_FILE$(0%))

	IF AR_CRJH_FILE%
	THEN
		!
		! Get ONLY the batch number of the file
		!
		FOR LOOP% = 1% TO AR_CRJH_FILE%
			TEMP$ = RIGHT(AR_CRJH_FILE$(LOOP%), 9%)
			I% = INSTR(1%, TEMP$, ".")
			I% = LEN(TEMP$) + 1% IF I% = 0%
			AR_CRJH_FILE$(LOOP%) = LEFT(TEMP$, I% - 1%)
		NEXT LOOP%

		!
		! Query the user
		!
		X% = ENTR_3CHOICE(SCOPE, "", "", AR_CRJH_FILE$(), "", &
			0%, "AR Cash Receipts Journal Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(AR_CRJH_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key ?
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	!
	! Ask user for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

310	!
	! Print background
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Cash Receipts Journal Maintenance", 10%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch:", 12%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for batch number
	!
	BATCH_NO$ = ""
	SCOPE::PRG_ITEM = "FLD01BATCH"

	!++
	! Abstract:FLD01BATCH
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		12%, 39%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the BATCH number in XX format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)


	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_CRJ.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpERROR

 HelpFile:
 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION AR_MAIN_CRJ
	EXTERNAL LONG FUNCTION AR_MAIN_CRJ_LINE
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION AR_MAIN_LEDMAINT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER


	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_CRJ.ID

		MAINT_GROUP = AR_MAIN_CRJ(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CRJ_LINE.ID

		MAINT_GROUP = AR_MAIN_CRJ_LINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID

		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_LEDMAINT.ID

		MAINT_GROUP = AR_MAIN_LEDMAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID

		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID

		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID

		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID

		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

	END FUNCTION

	!*******************************************************************
	! Loadvar routine
	!*******************************************************************

30000	SUB FORM_LOADVAR(P1$, P2, P3$)

	CALL AR_OUTP_CRJ_LOADVAR(P1$, P2, P3$)

32767	END SUB
