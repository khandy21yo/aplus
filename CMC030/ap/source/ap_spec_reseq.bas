1	%TITLE "Accounts Payable Resequence Check Numbers"
	%SBTTL "AP_SPEC_RESEQ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	This program resequences check numbers
	!	in the Accounts Payable Open file and the General Ledger file.
	!	.lm -5
	!
	! Index:
	!	.x Resequence Check Numbers
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_RESEQ/LINE
	!	$ LINK/EXE=AP_EXE: AP_SPEC_RESEQ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_RESEQ.OBJ;*
	!
	! Author:
	!
	!	12/15/88 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/11/92 - Kevin Handy
	!		Modified to pull in GL_YYYY_PP.DEV$ when the
	!		GL files don't exist on users account.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_RESEQ to SMG_RESEQ%.
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/27/97 - Kevin Handy
	!		Open both files before trying to update.
	!		Open files MOD unstead of UPD.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 5%

	!
	! Dimension
	!
	DIM GL_YYYY_PP_FILE$(1000%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!******************************************************************
	! Get period for batch
	!******************************************************************

	CALL READ_DEVICE("GL_YYYY_PP",GL_YYYY_PP.DEV$, STAT%)
	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No GL Period Files found", 0%)
		GOTO ExitProgram
	END IF

	GL_YYYY_PP_FILE$(LOOP%) = MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%) &
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

	%PAGE

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	YYYY_PP$ = SPACE$(7%)
	BATCH$ = SPACE$(6%)
	FROM_CHK$ = SPACE$(6%)
	TO_CHK$ = SPACE$(6%)
	OFFSET = 0.0

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( &
		18%, 78%, SMG_RESEQ%, SMG$M_BORDER)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_RESEQ%, &
		"Resequence the Check Numbers in AP_OPEN and GL_YYYY_PP")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_RESEQ%, &
		SCOPE::SMG_PBID, 2%, 2%)

	%PAGE

 Menu:
1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change Blank Go Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO Menu

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	SELECT OPT$

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to change", &
			0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO Menu

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO Menu

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1% IF LOOP% > 1%
			GOTO Changer1

		!
		! SMG$K_TRM_DOWN
		!
		CASE SMG$K_TRM_DOWN
			LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
			GOTO Changer1

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO Changer

	CASE "B"
 BlankR:	!*****************************************************
		! Blank information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to Blank", 0.0, &
			4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO Menu

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Blankr IF LOOP% < 1% OR LOOP% > MAX_ITEM

		SELECT LOOP%

		CASE 1%
			LSET YYYY_PP$ = SPACE$(7%)
		CASE 2%
			LSET BATCH$ = SPACE$(6%)
		CASE 3%
			LSET FROM_CHK$ = SPACE$(6%)
		CASE 4%
			LSET TO_CHK$ = SPACE$(6%)
		CASE 5%
			OFFSET = 0.0

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		IF EDIT$(YYYY_PP$, -1%) = ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE,  "GL_YYYY_PP Unknown - " + &
				"reenter !!!", 0%)
			GOTO Menu
		END IF

		SCOPE::PRG_ITEM = "CONFIRM"
		INP$ = ENTR_3YESNO(SCOPE, SMG_RESEQ%, "", &
			"Confirm Resequence file - " + &
			"then press <Do> ", "N", 0%, "", "")

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			SPACE$(80%), 1%, 1%)

		GOTO Menu IF INP$ <> "Y"

		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, &
			"Examining Check #", 12%, 5%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, &
			"Changing Check #", 13%, 5%)

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		GOSUB OpenFiles

		GOSUB Change_AP
		GOTO Menu IF AP_FLAG%

		GOSUB Change_GL
		GOTO Menu IF GL_FLAG%

	!
	! Help
	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")
		GOTO Menu

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO Menu

	%PAGE

 ExitProgram:
1560	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 OpenFiles:
1600	!
	! Open AP_OPEN file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

1610	!
	! Open GL_YYYY_PP file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

1690	RETURN

 Change_AP:
2000	!******************************************************************
	! Change check numbers for AP
	!******************************************************************
	AP_FLAG% = 0%

	RESET #AP_OPEN.CH%

2010	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #2% EQ BATCH$
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 2080 IF ERR=155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

 AP_loop:
2020	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 2090 IF ERR=11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 2090 IF AP_OPEN::BATCH <> BATCH$

	GOTO 2020 IF (FROM_CHK$ <> "" AND AP_OPEN::CKNUM < FROM_CHK$) OR &
		(TO_CHK$ <> "" AND AP_OPEN::CKNUM > TO_CHK$)

2030	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, SPACE$(35%), 12%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, AP_OPEN::CKNUM, 12%, &
		40%, , SMG$M_BOLD)

	A% = OFFSET
	STAT% = FUNC_NUMBERADD(AP_OPEN::CKNUM, A%)

	SELECT	STAT%
	CASE > 0%	! Error - AP_OPEN::CKNUM went negitive.

		CALL ENTR_3MESSAGE(SCOPE, "Error - Negitive Check numbers " + &
			"check your offset.", 0%)
		AP_FLAG% = -1%
		GOTO 2090

	CASE 0%	! Error - no number given in AP_OPEN::CKNUM field.

		CALL ENTR_3MESSAGE(SCOPE, "Error - No Check number.", 0%)
		AP_FLAG% = -1%
		GOTO 2090

	CASE ELSE	! Success.

		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, SPACE$(35%), 13%, 40%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, AP_OPEN::CKNUM, 13%, &
			40%, , SMG$M_BOLD)

2040		WHEN ERROR IN
			UPDATE #AP_OPEN.CH%
		USE
			FILENAME$ = "AP_OPEN_UPDATE"
			CONTINUE HelpError
		END WHEN

		GOTO AP_loop

	END SELECT

2080	!
	! Error return
	!
	CALL ENTR_3MESSAGE(SCOPE, "Batch number not found in AP.", 0%)

2090	RETURN

 Change_GL:
3000	!******************************************************************
	! Change check numbers for GL
	!******************************************************************
	GL_FLAG% = 0%

	RESET #GL_YYYY_PP.CH%

3010	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCH$
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3080 IF ERR=155%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

 GL_loop:
3020	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3090 IF ERR=11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 3090 IF GL_YYYY_PP::BTHNUM <> BATCH$

	GOTO 3020 IF (FROM_CHK$ <> "" AND GL_YYYY_PP::CKNO < FROM_CHK$) OR &
		(TO_CHK$ <> "" AND GL_YYYY_PP::CKNO > TO_CHK$)

3030	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, SPACE$(35%), 12%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, GL_YYYY_PP::CKNO, 12%, &
		40%, , SMG$M_BOLD)

	A% = OFFSET
	STAT% = FUNC_NUMBERADD(GL_YYYY_PP::CKNO, A%)

	SELECT	STAT%
	CASE > 0%	! Error - GL_YYYY_PP::CKNO went negitive.

		CALL ENTR_3MESSAGE(SCOPE, "Error - Negitive Check numbers " + &
			"check your offset.", 0%)
		GL_FLAG% = -1%

	CASE 0%	! Error - no number given in GL_YYYY_PP::CKNO field.

		CALL ENTR_3MESSAGE(SCOPE, "Error - No Check number.", 0%)
		GL_FLAG% = -1%

	CASE ELSE	! Success.

		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, SPACE$(35%), 13%, 40%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, GL_YYYY_PP::CKNO, 13%, &
			40%, , SMG$M_BOLD)

3040		WHEN ERROR IN
			UPDATE #GL_YYYY_PP.CH%
		USE
			FILENAME$ = "GL_YYYY_PP_UPDATE"
			CONTINUE HelpError
		END WHEN

		GOTO GL_loop

	END SELECT

3080	!
	! Error return
	!
	CALL ENTR_3MESSAGE(SCOPE, "Batch number not found in GL.", 0%)

3090	RETURN


 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	5,15, "(01) GL Period File(YYYY_PP)", &
		6,15, "(02) Batch #", &
		7,15, "(03) From Check #", &
		8,15, "(04) To   Check #", &
		9,15, "(05) Offset", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_RESEQ%, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%
	CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) General Ledger Period File\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Period File\* refers to the file period in the General
	!	Ledger which contains the check numbers that are to be resequenced. The
	!	format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger Period File
	!
	!--
		YYYY_PP$ = ENTR_3STRINGLIST(SCOPE,  SMG_RESEQ%, "5;45", &
			"GL Period File", YYYY_PP$, FLAG%, &
			"'E", DEFLT$, GL_YYYY_PP_FILE$(), &
			"GL Period Files", "")

	CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field determines which batch file will
	!	be resequenced. The ^*Batch Number\* field must contain a valid value, a
	!	batch number which contains data.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Resequence Check Numbers
	!	.x Resequence Check Numbers>Batch Number
	!
	!--
		BATCH$ = ENTR_3STRING(SCOPE, SMG_RESEQ%, "6;45", &
			"Batch #", BATCH$, FLAG%, "'E", DEFLT$)

	CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) From Check Number\*
	!	.b
	!	.lm +5
	!	The ^*From Check Number\* field determines the check number
	!	with which the checks will begin resequencing. If this setting is blank,
	!	the report will begin with the first check number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Check Number>Resequence Check Numbers
	!	.x Resequence Check Numbers>From Check Number
	!	.x From>Check Number
	!	.x Check Number>From
	!
	!--
		FROM_CHK$ = ENTR_3STRING(SCOPE, SMG_RESEQ%, "7;45", &
			"From Check #", FROM_CHK$, FLAG%, "'E", DEFLT$)
	CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) To Check Number\*
	!	.b
	!	.lm +5
	!	The ^*To Check Number\* field determines the last check number
	!	which will be resequenced. If this field in blank, the resequencing will
	!	end with the last transaction number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Check Number>Resequence Check Numbers
	!	.x Resequence Check Numbers>To Check Number
	!	.x To>Check Number
	!	.x Check Number>To
	!
	!--
		TO_CHK$ = ENTR_3STRING(SCOPE, SMG_RESEQ%, "8;45", &
			"To   Check #", TO_CHK$, FLAG%, "'E", DEFLT$)
	CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Offset\*
	!	.b
	!	.lm +5
	!	The ^*Offset\* field enters the number that will offset
	!	the check numbers to establish the correct numbers on the checks and within
	!	the program.
	!	.lm -5
	!
	! Index:
	!	.x Offset>Resequence Check Numbers
	!	.x Resequence Check Numbers>Offset
	!
	!--
		OFFSET = ENTR_3NUMBER(SCOPE, SMG_RESEQ%, "9;45", &
			"Offset", OFFSET, FLAG%, &
			"#########", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
