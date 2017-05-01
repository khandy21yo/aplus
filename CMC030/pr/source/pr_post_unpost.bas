1	%TITLE "UNPOST - Unpost the GL"
	%SBTTL "PR_POST_UNPOST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	!	This program will unpost from the GL and set the
	!	posting flag in the payroll folder to an unposted
	!	state. The set update flags of posting are:
	!	.table 3,25
	!	.te
	!	2	Accrued Post
	!	.te
	!	4	Final Post
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Payroll Unpost Option
	!	.x Unpost Option
	!
	! Option:
	!
	!	PR_POST_UNPOST$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_POST_UNPOST/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_POST_UNPOST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_POST_UNPOST.OBJ;*
	!
	! Author:
	!
	!	05/15/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to EMTR_3CHOICE
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$()
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	!
	! Dimension
	!
	DIM REVERSE_LIST$(400%), DATE_FILE$(400), GL_YYYY_PP_FILE$(400)

	!
	! State table
	!
	STATE$ = "0"
	STATE$(0%) = "2"
	STATE$(1%) = "0 - No Posting Status"
	STATE$(2%) = "2 - Accrued Post"

	ON ERROR GOTO 19000

	%PAGE

	!
	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************
	CALL READ_INITIALIZE

	!
	! Create display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

100	!******************************************************************
	! Get date for file name
	!******************************************************************

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL  READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE( PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", DATE_FILE$(), &
		16%, "", "")

	DATE_FILE% = VAL%(DATE_FILE$(0%))

	IF DATE_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No payroll folder found", 0%)
		GOTO ExitProgram
	END IF

	REVERSE_LIST$(DATE_FILE% - LOOP% + 1%) = &
		MID(DATE_FILE$(LOOP%), 16%, 2%) + "/" + &
		MID(DATE_FILE$(LOOP%), 18%, 2%) + "/" + &
		MID(DATE_FILE$(LOOP%), 12%, 4%) &
		FOR LOOP% = DATE_FILE% TO 1% STEP -1%

110	!
	! Ask for the payroll folder date
	!
	TEMP$ = "Payroll Folder Dates"

	X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), &
		"", 0%, TEMP$, "", 0%)

	IF X% > 0%
	THEN
		BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
			LEFT(REVERSE_LIST$(X%), 2%) + &
			MID(REVERSE_LIST$(X%), 4%, 2%)
		GOTO 300
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 110
	END SELECT

	%Page

300	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

340	!
	! Get GL_PERIOD file info
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CLOSE_YYYY_PP$ = GL_PERIOD::YEAR + "_" + FORMAT$( &
		GL_PERIOD::LASTPERCLO, "<0>#")

370	!******************************************************************
	! Get period for batch
	!******************************************************************

	CALL FIND_FILE( GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No GL Period Files found", 0%)
		GOTO ExitProgram
	END IF

	GL_YYYY_PP_FILE$(LOOP%) = &
		MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%) &
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

375	!
	! Select the gl period file
	!
	TEMP$ = "GL Period Files"

	X% = ENTR_3CHOICE(SCOPE, "", "", GL_YYYY_PP_FILE$(), "", &
		0%, TEMP$, "", 0%)

	IF X% > 0%
	THEN
		YYYY_PP$ = EDIT$(GL_YYYY_PP_FILE$(X%), -1%)
		IF YYYY_PP$ <= CLOSE_YYYY_PP$
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"This period has been closed", 0%)
			GOTO 375
		ELSE
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	GOTO 375

390	!
	! Open GL period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		FILENAME$ = "GL_YYYY_PP.LED"
		CONTINUE HelpError
	END WHEN

	%PAGE

400	!
	! Ask for the batch number to be removed
	!

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "FLDBATCHNUM"
	!++
	! Abstract:FLDBATCHNUM
	!	^*Batch Number\*
	!	.p
	!	The ^*Batch Number\* field refers to the number of the batch that is desired
	!	to be unposted.
	!
	! Index:
	!	.x Batch Number>Unpost Option
	!	.x Unpost Option>Batch Number
	!
	!--

	BATCH_KEY$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Batch number to delete", "      ", &
		0%, "", ""), -1%)

	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		GOTO ExitProgram
	END IF

500	!
	! Ask for state to change post flag
	!

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "FLDPOSTFLAG"
	!++
	! Abstract:FLDPOSTFLAG
	!	^*Post Flag\*
	!	.p
	!	The ^*Post Flag\* field refers to the flag indicating the type of posting
	!	that has been done. The posting flags are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	2 - Accrued Post
	!	.le
	!	4 - Final Post
	!	.els
	!
	! Index:
	!	.x Post Flags>Unpost Option
	!	.x Unpost Option>Post Flags
	!
	!--

	STATE$ = EDIT$(ENTR_3STRINGLIST(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter the Post flag", STATE$, 8%, "'", &
		"", STATE$(), "Post State", "005"), -1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 500
	END SELECT

	STATE% = VAL%(STATE$)

	!
	! Check states
	!
	GOTO 500 IF STATE% <> 2% AND STATE% <> 0%

	SCOPE::PRG_ITEM = "CONFIRM"

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Payroll Unpost process  - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

1000	CALL ENTR_3MESSAGE(SCOPE, "Processing the pay file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

1010	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%

		PR_TRN_PAY::UPDATE_FLAG = STATE%

		UPDATE #PR_TRN_PAY.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 1010

2000	CALL ENTR_3MESSAGE(SCOPE, "Processing the deduction file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_DED.CH%
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #PR_TRN_DED.CH%
		PR_TRN_DED::UPDATE_FLAG = STATE%
		UPDATE #PR_TRN_DED.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 2010

3000	CALL ENTR_3MESSAGE(SCOPE, "Processing the check file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

3010	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%
		PR_TRN_CHECK::UPDATE_FLAG = STATE%
		UPDATE #PR_TRN_CHECK.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 4000 IF ERR = 11%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	GOTO 3010

4000	CALL ENTR_3MESSAGE(SCOPE, "Processing the GL folder file", 1%)

	!
	! Remove the batch for the GL folder
	!
	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #4% GE BATCH_KEY$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "GL_YYYY_PP.LED"
		CONTINUE HelpError
	END WHEN

4010	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
		GOTO ExitProgram IF BATCH_KEY$ <> GL_YYYY_PP::BTHNUM
		DELETE #GL_YYYY_PP.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP.LED"
		CONTINUE HelpError
	END WHEN

	GOTO 4010

 ExitProgram:
	CLOSE PR_TRN_CHECK.CH%, PR_TRN_DED.CH%, PR_TRN_PAY.CH%

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.p
	!	The ^*Confirm\* field confirms the unposting process.
	!
	! Index:
	!	.x Confirm
	!
	!--
	!+-+-+
	!++
	! Abstract:BATCH_NO
	!	^*Batch Number\*
	!	.p
	!	The ^*Batch Number\* field specifies the batch that is
	!	to be unposted.
	!
	! Index:
	!	.x Batch Number>Unpost Option
	!	.x Unpost Option>Batch Number
	!
	!--
