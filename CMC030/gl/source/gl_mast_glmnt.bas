1	%TITLE "General Ledger Period File"
	%SBTTL "GL_MAST_GLMNT"
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
	!	The ^*General Ledger Maintenance\* function allows correction
	!	of any record in the General Ledger file.
	!	.b
	!	^*Note:  It is strongly recommended
	!	that the amount field not be changed.\*
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Maintenance
	!	.x Maintain>General Ledger
	!
	! Option:
	!	GL_MAST_GLMNT$PERIOD
	!	GL_MAST_GLMNT$YEAR
	!	GL_MAIN_GLMNT$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_GLMNT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_GLMNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_GLMNT.OBJ;*
	!
	! Author:
	!
	!	01/12/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	06/24/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Fix last param to ENTR_3CHOICE.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/30/2001 - Kevin Handy
	!		Reverse the list of folders
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
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions
	!
	COM (TT_GL_GLMNT) &
		YYYY_PP$ = 7%

	!
	! Dimension statements
	!
	DIM GL_YYYY_PP_FILE$(1000%)

	%PAGE

100	!******************************************************************
	! Initialization section - Prepare to do anything
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	%PAGE

200	!******************************************************************
	! Select GL Period file  (Which YYYY and which PP?)
	!******************************************************************

	!
	! Open up Control file, and grab record
	!
210	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to open period definition file", 0%)
		CONTINUE ExitProgram
	END WHEN

220	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to read period definition file", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Close GL Control file
	!
	CLOSE GL_PERIOD.CH%

	%PAGE

300	!
	! Get information needed to open GL Period file (GL_YYYY_PP)
	!
	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE%
	THEN
		GL_YYYY_PP_FILE$(LOOP%) = &
			MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%) &
			FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

		!
		! Reverse the list
		!
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE% / 2%
			TEMP$ = GL_YYYY_PP_FILE$(LOOP%)
			GL_YYYY_PP_FILE$(LOOP%) = &
				GL_YYYY_PP_FILE$(GL_YYYY_PP_FILE% - LOOP% + 1%)
			GL_YYYY_PP_FILE$(GL_YYYY_PP_FILE% - LOOP% + 1%) = &
				TEMP$
		NEXT LOOP%

		TEMP$ = "GL Period Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", GL_YYYY_PP_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY_PP$ = EDIT$(GL_YYYY_PP_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
			GOTO ExitProgram

	END SELECT

	!
	! Ask for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

310	TEMP_PP% = GL_PERIOD::LASTPERCLO + 1%
	WHEN ERROR IN
		TEMP_YY% = VAL%(GL_PERIOD::YEAR)
	USE
		TEMP_YY% = 0%
	END WHEN

	IF TEMP_PP% > GL_PERIOD::FPFY
	THEN
		TEMP_PP% = 1%
		TEMP_YY% = TEMP_YY% + 1%
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"GL Period File Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Year:   " + FORMAT$(TEMP_YY%, "<0>###"), &
		12%, 31%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Period: " + FORMAT$(TEMP_PP%, "<0>#"), &
		13%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for year of file
	!
	SCOPE::PRG_ITEM = "FLD001"

	GL_YYYY_PP.YEAR$ = FORMAT$(TEMP_YY%, "<0>###")

	!++
	!
	! Abstract:FLD001
	!	^*Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.b
	!	The format for entry is YYYY.
	!	.lm 5
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		12%, 39%, GL_YYYY_PP.YEAR$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 330

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	GL_YYYY_PP.YEAR$ = EDIT$(GL_YYYY_PP.YEAR$, -1%)

	GL_YYYY_PP.YEAR$ = LEFT(DATE_TODAY, 2%) + GL_YYYY_PP.YEAR$ &
		IF LEN(EDIT$(GL_YYYY_PP.YEAR$, -1%)) = 2%

	IF LEN(EDIT$(GL_YYYY_PP.YEAR$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the G/L year in YYYY format", 0%)
		GOTO 320
	END IF

330	SCOPE::PRG_ITEM = "FLD002"

	GL_YYYY_PP.PERIOD$ = FORMAT$(TEMP_PP%, "<0>#")

	!++
	! Abstract:FLD002
	!	^*Period\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		13%, 39%, GL_YYYY_PP.PERIOD$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_DOWN
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 330
	END SELECT

	WHEN ERROR IN
		GL_YYYY_PP.PERIOD% = VAL%(GL_YYYY_PP.PERIOD$)
	USE
		CONTINUE 330
	END WHEN

	IF GL_YYYY_PP.PERIOD% > GL_PERIOD::FPFY OR &
		GL_YYYY_PP.PERIOD% < 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"The period must be between 1 and " + &
			NUM1$(GL_PERIOD::FPFY), 0%)
		GOTO 330
	END IF

	YYYY_PP$ = GL_YYYY_PP.YEAR$ + "_" + GL_YYYY_PP.PERIOD$

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

1000	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_GLMNT.ID, "")

	!******************************************************************
	! Exit GL_MAST_GLMNT
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	%PAGE

19990	!******************************************************************
	! End of GL_MAST_GLMNT
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
	EXTERNAL LONG	FUNCTION GL_MAIN_GLMNT

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
	! Process the General Ledger maintenance window
	!
	CASE GL_MAIN_GLMNT.ID

		MAINT_GROUP = GL_MAIN_GLMNT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLDPERIOD
	!	^*GL Period\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLDYEAR
	!	^*GL Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:PERIOD
	!	^*GL Period File Maintenance\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:YEAR
	!	^*GL Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
