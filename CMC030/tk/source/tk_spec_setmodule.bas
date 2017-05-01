1	%TITLE "Set Modules Desc End Their Relation"
	%SBTTL "TK_SPEC_SETMODULE"
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
	! any other copies therof may not be provided or otherwise made
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
	!	.p
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	01/20/88 - Frank F. Starman
	!
	! Compile directions:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_SETMODULE.BAS/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_SETMODULE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_SETMODULE.OBJ;*
	!
	! Modification history:
	!
	!	06/26/89 - Kevin Handy
	!		Fixed bug in wildcard names where spaces were
	!		prepended.
	!
	!	03/26/91 - Kevin Handy
	!		Attempt to make compile after Frank mangled the
	!		source code, then refused to fix his damage.
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	06/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE)	TK_MODULE_CDD	TK_MODULE

	!
	! External functions
	!
	EXTERNAL LONG   READ_3BROADCAST

	!
	! Dimension variables
	!
	DIM FILE_NAME$(4000%), &
		DIR_NAME$(4000%), &
		DIR$(500%, 20%), &
		S%(500%, 20%), &
		E%(500%, 20%)

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 2%

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(SOURCE.CH%, STAT%)
	CALL ASSG_CHANNEL(COM_FILE.CH%, STAT%)

	!
	! Set error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set variables
	!
	DIR_PREFIX$ = "SOURCE:["
	DIR_SUFFIX$ = "]*.DIR"

	!
	! Look up all directories for the source code
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up directories.", 1% + 16%)

	!
	! Look up root directory
	!
	CALL FIND_FILE(DIR_PREFIX$ + "*.-" + DIR_SUFFIX$, &
		DIR_NAME$(), 16%, "", "")

	L% = 1%

	L%(L%) = VAL%(DIR_NAME$(0%))

	DIR$(LOOP%, L%) = DIR_NAME$(LOOP%) FOR LOOP% = 1% TO L%(L%)

	!
	! Do all of the subdirectories
	!
	TEST% = -1%

	WHILE TEST%
		L_CNT% = 0%
		FOR X% = 1% TO L%(L%)
			SUB_DIR$ = DIR$(X%, L%)

			CALL FIND_FILE(DIR_PREFIX$ + SUB_DIR$ + DIR_SUFFIX$, &
				DIR_NAME$(), 16%, "", "")

			CNT% = VAL%(DIR_NAME$(0%))

			IF CNT%
			THEN
				S%(X%, L%) = L_CNT% + 1%
				E%(X%, L%) = L_CNT% + CNT%
				DIR$(L_CNT% + LOOP%, L% + 1%) = &
					SUB_DIR$ + "." + DIR_NAME$(LOOP%) &
					FOR LOOP% = 1% TO CNT%
				L_CNT% = L_CNT% + CNT%
			END IF
		NEXT X%

		IF L_CNT%
		THEN
			L% = L% + 1%
			L%(L%) = L_CNT%
		ELSE
			TEST% = 0%
		END IF

	NEXT

	!
	! Now create indendent directory
	!
	DIR_CNT% = 0%

	FOR X% = 1% TO L%(1%)
		DIR_CNT% = DIR_CNT% + 1%
		DIR_NAME$(DIR_CNT%) = DIR$(X%, 1%)

		FOR Y% = 1% TO L% - 1%
			FOR Z% = S%(X%, Y%) TO E%(X%, Y%)
				IF DIR$(Z%, Y% + 1%) <> ""
				THEN
					DIR_CNT% = DIR_CNT% + 1%
					DIR_NAME$(DIR_CNT%) = SPACE$(2% * Y%) + &
						DIR$(Z%, Y% + 1%)
				END IF
			NEXT Z%
		NEXT Y%
	NEXT X%

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Set the number of element in the array in to element zero
	!
	DIR_NAME$(0%) = NUM1$(DIR_CNT%)

	!
	! If there is no directory then abort out of program
	!
	IF DIR_CNT% = 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Directories do not exist.  Aborting. . .", 0%)
		GOTO ExitProgram
	END IF

	!
	! Open main file (existing) for modification
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.CRE"
	USE
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

	%PAGE

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	DIRECT$ = "*" + SPACE$(38%)
	MODULE$ = "*" + SPACE$(38%)

900	!
	! Paint the background, and confirm update
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Set modules description", &
		SMG$K_TOP &
	)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change Go eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

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

		LOOP% = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, "", &
			"Item to change", 0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

		LOOP1% = LOOP%

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

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

	CASE "G"
		GOSUB SetMod

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	7, 10, "(01) Directory", &
		9, 10, "(02) Module", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, ATEXT$, &
			XLONG, YLONG)
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
		DIRECT$ = ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, &
			"7;35", "Directory", &
			LEFT(DIRECT$ + SPACE$(39%), 39%), &
			FLAG% + 2048%, "'E", "", &
			DIR_NAME$(), "Directories", "")

	CASE 2%
		MODULE$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"9;35", "Module", MODULE$, &
			FLAG%, "'E", "")

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 SetMod:
2000	!*****************************************************
	! Update modules
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm updating - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO RetSetMod
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Processing Please wait. . . ", 1%)

	COM_FILE$ = "TEMP.COM"

	WHEN ERROR IN
		OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
			RECORDSIZE 132%

		PRINT #COM_FILE.CH%, "$ @CMC:LOGICALS "
		PRINT #COM_FILE.CH%, "$ SET NOON "
		PRINT #COM_FILE.CH%, "$ DELETE PROGRAM_ERROR.LOG;*"
	USE
		FILENAME$ = COM_FILE$
		CONTINUE HelpError
	END WHEN

	ID% = 9999%

	!
	! Search for next available module number
	!
	WHEN ERROR IN
		RESET #TK_MODULE.CH%, KEY #0%
	USE
		CONTINUE 2020
	END WHEN

2005	WHEN ERROR IN
		GET #TK_MODULE.CH%
	USE
		CONTINUE 2010 IF ERR = 11%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

	ID% = VAL%(TK_MODULE::MODNUM)

	GOTO 2005

2010	WHEN ERROR IN
		RESET #TK_MODULE.CH%
	USE
		CONTINUE 2020
	END WHEN

2020	ID% = ID% + 1%

	MODTYPE$ = "P"

	!
	! Program modules
	!
	IF DIRECT$ = "*"
	THEN
		END_CNT% = DIR_CNT%
	ELSE
		END_CNT% = 1%
	END IF

	FOR J% = 1% TO END_CNT%

		IF DIRECT$ = "*"
		THEN
			PREFIX$ = "SOURCE:[" + EDIT$(DIR_NAME$(J%), 2%) + "]"
			CATEGORY$ = DIR_NAME$(J%)
		ELSE
			PREFIX$ = "SOURCE:[" + TRM$(DIRECT$) + "]"
			CATEGORY$ = DIRECT$
		END IF

		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".BAS", &
			FILE_NAME$(), 16%, "", "")

		LOOP% = VAL%(FILE_NAME$(0%))
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			TRM$(CATEGORY$) + " " + FILE_NAME$(0%) + &
			" Modules   ", 4%, 5%)

		!
		! Now let's look up all of the code for the various languages
		!

		!
		! We start with the basic language
		!
		FOR I% = 1% TO LOOP%

			EXTENSION$ = "BAS"
			LANGUAGE$ = "BASIC"

2100			WHEN ERROR IN
				OPEN PREFIX$ + FILE_NAME$(I%) + ".BAS" FOR INPUT &
					AS FILE SOURCE.CH%, &
					ACCESS READ, ALLOW MODIFY
			USE
				FILENAME$ = FILE_NAME$(I%)
				CONTINUE HelpError
			END WHEN

			CALL ENTR_3MESSAGE(SCOPE, "Looking up " + &
				FILE_NAME$(I%), 1%)

			!
			! Set default module type
			!
			MODTYPE$ = "EXEC"

			FOUND_TITLE% = 0%
			SHAREABLE$ = "N"

 BasicLine:
2110			WHEN ERROR IN
				LINPUT #SOURCE.CH%, TEXT$
			USE
				CONTINUE 2120 IF ERR = 11%
				FILENAME$ = FILE_NAME$(I%)
				CONTINUE HelpError
			END WHEN

			TEXT$ = EDIT$(TEXT$, 8% + 128%)

			GOTO BasicLine IF TEXT$ = "" OR (TEXT$ = "!")

			!
			! Read description
			!
			IF FOUND_TITLE% = 0%
			THEN
 !
 ! This is where Frank mangled the program.  I have no idea what this
 ! line was originally, but this is my bust guess as to what is supposed
 ! to be here.
 !
 !1	%TITLE "%TITLE"
				TEST% = INSTR(1%, TEXT$, "%TITLE")
				IF TEST%
				THEN
					FROM_DASH% = INSTR(1%, TEXT$,'"')
					TEXT$ = RIGHT(TEXT$, FROM_DASH% + 1%)
					TO_APO% = INSTR(1%, TEXT$, '"')
					TEXT$ = LEFT(TEXT$, TO_APO% - 1%)
					DESCRIPTION$ = EDIT$(TEXT$, 8%)
					FOUND_TITLE% = -1%
				END IF
			END IF

			IF TEXT$ = "!--"
			THEN
				GOTO 2120
			END IF

			IF INSTR(1%, TEXT$, "CMC_3VECTOR/REP")
			THEN
				SHAREABLE$ = "Y"
			END IF

			!
			! Set module type if it can be found
			!
			IF (LEFT(TEXT$, 3%) = "SUB")
			THEN
				MODTYPE$ = "SUBR"
			END IF

			IF (LEFT(TEXT$, 8%) = "FUNCTION")
			THEN
				MODTYPE$ = "FUNC"
			END IF

			GOTO BasicLine

2120			GOSUB 18000

			CLOSE SOURCE.CH%

			PRINT #COM_FILE.CH%, &
				"$ BAS/NOOBJ/NOLIST/NOOPT/CROSS " + PREFIX$ + &
				FILE_NAME$(I%)
			PRINT #COM_FILE.CH%, &
				"$ RENAME " + FILE_NAME$(I%) + ".LIS " + &
				FILE_NAME$(I%) + ".CRO "
			PRINT #COM_FILE.CH%, &
				"$ COPY " + PREFIX$ + FILE_NAME$(I%) + &
				".BAS " + FILE_NAME$(I%) + ".BAS_TEMP"
			PRINT #COM_FILE.CH%, &
				"$ RUN CMC$ROOT:[TK]TK_SPEC_SETRELATION"
			PRINT #COM_FILE.CH%, &
				"$ RUN CMC$ROOT:[TK]TK_SPEC_CHECKVAR"
			PRINT #COM_FILE.CH%, &
				"$ DELETE " + FILE_NAME$(I%) + ".CRO;*"
			PRINT #COM_FILE.CH%, &
				"$ DELETE " + FILE_NAME$(I%) + ".BAS_TEMP;*"

		NEXT I%

		CALL ENTR_3MESSAGE(SCOPE, "Looking up " + FILE_NAME$(I%), 1%)

 NextJ:
	NEXT J%

	CLOSE #COM_FILE.CH%

	!
	! Submit command file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Submitting command file", 1%)

	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	SMG_STATUS% = LIB$SPAWN("SUBMIT " + COM_FILE$ + "/NOPRINT/NOTIFY")

	IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
			" has occured", 0%)
	END IF

	SLEEP 1%

	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

 RetSetMod:
	RETURN

	%Page

18000	!******************************************************************
	! This subroutine is used to change or add a module to the module
	! file.  If the module exists the the record is update this the
	! most current information, otherwise a new record is added.
	!******************************************************************
	TEST% = INSTR(1%, CATEGORY$, ".")
	IF TEST%
	THEN
		CATEGORY$ = LEFT(CATEGORY$, TEST% - 1%)
	END IF

	WHEN ERROR IN
		FIND #TK_MODULE.CH%, KEY #0% EQ FILE_NAME$(I%)
		GET #TK_MODULE.CH%
	USE
		CONTINUE AddModule IF ERR = 155%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

	TK_MODULE::DESCRIPTION	= DESCRIPTION$
	TK_MODULE::CATEGORY	= CATEGORY$
	TK_MODULE::EXTENSION	= EXTENSION$
	TK_MODULE::LANGUAGE	= LANGUAGE$
	TK_MODULE::DIRECTORY	= PREFIX$
	TK_MODULE::MODTYPE	= MODTYPE$
	TK_MODULE::CDATE	= DATE_TODAY
	TK_MODULE::CTIME	= TIME_NOW
	TK_MODULE::SHAREABLE	= SHAREABLE$

	WHEN ERROR IN
		UPDATE #TK_MODULE.CH%
	USE
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

	GOTO ComeBack

 AddModule:
	TK_MODULE::MODNAME	= FILE_NAME$(I%)
	TK_MODULE::DESCRIPTION	= DESCRIPTION$
	TK_MODULE::EXTENSION	= EXTENSION$
	TK_MODULE::LANGUAGE	= LANGUAGE$
	TK_MODULE::CATEGORY	= CATEGORY$
	TK_MODULE::DIRECTORY	= PREFIX$
	TK_MODULE::MODTYPE	= MODTYPE$
	TK_MODULE::MODNUM	= FORMAT$(ID%, "<0>#####")
	TK_MODULE::CDATE	= DATE_TODAY
	TK_MODULE::CTIME	= TIME_NOW
	TK_MODULE::SHAREABLE	= SHAREABLE$

	WHEN ERROR IN
		PUT #TK_MODULE.CH%
	USE
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN
	ID% = ID% + 1%

 ComeBack:
	RETURN

	%Page

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

	FILENAME$ = ""
	RESUME HelpError

32767	END
