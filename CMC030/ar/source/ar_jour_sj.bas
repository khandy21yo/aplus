1	%TITLE "Maintain Sales Journal"
	%SBTTL "AR_JOUR_SJ"
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
	!	The ^*Maintain Sales Journal\* option
	!	maintains regular sales and service charge journals.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!	AR_MAIN_SJ$HELP
	!	AR_MAIN_SJ_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_JOUR_SJ/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_JOUR_SJ,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_JOUR_SJ.OBJ;*
	!
	! Author:
	!
	!	08/03/88 - Kevin Handy
	!
	! Modification history:
	!
	!	03/11/88 - Kevin Handy
	!		Modified to get customer/client/patient from
	!		control file.
	!
	!	05/14/88 - Lance Williams
	!		Modified the header.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER
	!
	!	09/15/92 - Dan Perkins
	!		Added AR_MAIN_CUSTYPE and OE_MAIN_CATEGORY so that
	!		the F-17 and F-14 keys will work.
	!
	!	12/21/92 - Kevin Handy
	!		Added more tables to that customer names could
	!		be added from inside journal.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Change last param of entr_3choices from "" to 0%
	!
	!	05/02/95 - Kevin Handy
	!		Added FORM_LOADVAR section so that this bugger will
	!		be able to find the variables for the form.
	!
	!	11/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	11/14/95 - Kevin Handy
	!		Modify to open chart of accounts in read-only mode.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	05/19/98 - Kevin Handy
	!		Lose a lot of help messages that belonged to
	!		AR_MAIN_SJ.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Common statements
	!
	COM (CH_AR_CONTROL) &
		AR_CONTROL.CH%

	COM (TT_AR_SJ) &
		BATCH_NO$ = 2%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	!
	! Dimension statements
	!
	DIM AR_SJH_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

200	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

300	!******************************************************************
	! Get batch number
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("AR_SJH", AR_SJH.DEV$, STAT%)

	!
	! Find out what is already there
	!
	CALL FIND_FILE(AR_SJH.DEV$ + "AR_SJH_*.JRL", AR_SJH_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	!
	! If any files are in the list, then do a query screen
	!
	AR_SJH_FILE% = VAL%(AR_SJH_FILE$(0%))

	IF AR_SJH_FILE%
	THEN
		!
		! Get ONLY the batch number of the file
		!
		FOR LOOP% = 1% TO AR_SJH_FILE%
			TEMP$ = RIGHT(AR_SJH_FILE$(LOOP%), 8%)
			I% = INSTR(1%, TEMP$, ".")
			I% = LEN(TEMP$) + 1% IF I% = 0%
			AR_SJH_FILE$(LOOP%) = LEFT(TEMP$, I% - 1%)
		NEXT LOOP%

		!
		! Query the user
		!
		X% = ENTR_3CHOICE(SCOPE, "", "", AR_SJH_FILE$(), "", &
			0%, "AR Sales Journal Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(AR_SJH_FILE$(X%), -1%)
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
		"AR Sales Journal Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch:   " + BATCH_NO$, &
		12%, 31%)

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
	!
	! Index:
	!
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

760	!
	! Open chart of accounts read/only
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 800
	END WHEN

	GL_CHART.READONLY% = -1%

800	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_SJ.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%Page

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"

	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_SJ
	EXTERNAL LONG FUNCTION AR_MAIN_SJ_LINE
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION AR_MAIN_CONTACT
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_SJ.ID
		MAINT_GROUP = AR_MAIN_SJ(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_SJ_LINE.ID
		MAINT_GROUP = AR_MAIN_SJ_LINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID
		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID
		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CONTACT.ID
		MAINT_GROUP = AR_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID
		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, &
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

	CASE SA_MAIN_SALESMAN.ID
		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

	END FUNCTION

	!*******************************************************************
	! Loadvar routine
	!*******************************************************************

30000	SUB FORM_LOADVAR(P1$, P2, P3$)

	CALL AR_OUTP_SJ_LOADVAR(P1$, P2, P3$)

32767	END SUB

	!+-+-+
	!++
	! Abstract:BATCH
	!	^*BATCH\*
	!	.lm +5
	!	.b
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:LINE_ITEMS
	!	^*Line__items\*
	!	.b
	!	.lm +5
	!	The ^*Line__items\* portion of this screen indicates how
	!	the transaction is to be allocated to various accounts and sub__codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be made.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Line Items
	!	.x Line Items>Sales Journal
	!
	!--
