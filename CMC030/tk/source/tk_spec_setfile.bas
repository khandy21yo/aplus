1	%TITLE "Set Record Structures Desc"
	%SBTTL "TK_SPEC_SETFILE"
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
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	01/26/88 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_SETFILE.BAS/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_SETFILE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_SETFILE.OBJ;*
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.HB"
	MAP (TK_FILE)	TK_FILE_CDD	TK_FILE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD LOC_DDL

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_OPN.HB"
	DECLARE  SMG_OPN_CDD LOC_OPN

	!
	! Declarations
	!
	DECLARE	STRING	LOC_DATABASE, &
		LOC_STRUCTNAME, &
		LOC_SEQUENCE, &
		LOC_STRUCTLIST(4000%)

	DECLARE INTEGER CONSTANT LOC_MAXITEM = 1%

	DECLARE LONG	XLONG, YLONG

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

 !	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	!
	! Open main file (existing) for modification
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.CRE"
	USE
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
	! Look up list of databases in the CDD and select one from that
	! list.
	!******************************************************************
	CALL TK_SUBR_DDLLIST("*", "", LOC_STRUCTLIST())

400	!
	! Ask for database
	!
	LOC_DBLOOP% = ENTR_3CHOICE(SCOPE, "", "", LOC_STRUCTLIST(), "", &
		0%, "Database", "", 0%)

	IF LOC_DBLOOP% > 0%
	THEN
		LOC_DATABASE = EDIT$(LOC_STRUCTLIST(LOC_DBLOOP%), -1%)
	ELSE
		GOTO 400
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!******************************************************************
	! Look up list of structures in the CDD.
	!******************************************************************
	LOC_STRCNT% = VAL%(LOC_STRUCTLIST(0%))
	LOC_STRUCTLIST(LOC_STRLOOP%) = "" FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%

	CALL TK_SUBR_DDLLIST(LOC_DATABASE, "*", LOC_STRUCTLIST())

	LOC_STRCNT% = VAL%(LOC_STRUCTLIST(0%))

	!
	! Add a wildcard element to the array
	!
	LOC_STRCNT% = LOC_STRCNT% + 1%
	LOC_STRUCTLIST(LOC_STRCNT%) = "*"
	LOC_STRUCTLIST(0%) = NUM1$(LOC_STRCNT%)

	!******************************************************************
	! Declare defaults for screen
	!******************************************************************
	LOC_STRUCTNAME = "*"

	!
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
		"Set file structure description and relation", &
		SMG$K_TOP &
	)

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
		GOTO Changer IF LOOP% < 1% OR LOOP% > LOC_MAXITEM

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
			LOOP% = LOOP% + 1% IF LOOP% < LOC_MAXITEM
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
		GOTO ExitProgram

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

	DATA	7, 10, "(01) Structure name", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, ATEXT$, &
			XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%

	GOSUB DataEntry FOR LOOP% = 1% TO LOC_MAXITEM

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
		LOC_STRUCTNAME = ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, &
			"7;35", "Structure name", &
			LEFT(LOC_STRUCTNAME + SPACE$(39%), 39%), &
			FLAG%, "'E", "", &
			LOC_STRUCTLIST(), "Structure List", "")

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 SetMod:
2000	!*****************************************************
	! Update file structures
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm updating - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!
	! Test structure name, If wild card then process all structures,
	! otherwise only do one.
	!
	IF LOC_STRUCTNAME = "*"
	THEN
		!
		! Loop thur entire array
		!
		FOR LOC_STRLOOP% = 1% TO LOC_STRCNT% - 1%
			LOC_STRUCTNAME = LOC_STRUCTLIST(LOC_STRLOOP%)
			GOSUB DDLExtract
		NEXT LOC_STRLOOP%
	ELSE
		GOSUB DDLExtract
	END IF

	RETURN

	%PAGE

 DDLExtract:
	!****************************************************************
	! Extract ddl, determine record size, extract open
	!****************************************************************
	REC_SIZE = 0.

	!
	! Get the ddl file
	!
	CALL TK_SUBR_DDLEXTRACT(LOC_DDL, &
		LOC_DATABASE, &
		LOC_STRUCTNAME, &
		FDE_STATUS%)

	LOC_FLDCNT% = LOC_DDL::FIELD_NUM

	!
	! if there are fields then process this structure
	!
	IF LOC_FLDCNT%
	THEN
		GOSUB 18030

		!
		! Read info about record
		!
		CALL TK_SUBR_EXTRACTOPEN(LOC_OPN, &
			LOC_STRUCTNAME, &
			"CRE", &
			"SOURCE:[" + LOC_DATABASE + ".OPEN]")

		FOR LOC_FLDLOOP% = 1% TO LOC_FLDCNT%
			LOC_SEQUENCE = FORMAT$(LOC_FLDLOOP%, "<0>##")
			GOSUB 18000
			REC_SIZE = REC_SIZE + TK_FILE::DATASIZE
		NEXT LOC_FLDLOOP%

	END IF

	LOC_SEQUENCE = "000"
	GOSUB 18020

	RETURN

	%PAGE

18000	!
 GoUpdate:
	WHEN ERROR IN
		GET #TK_FILE.CH%, KEY #0% EQ &
			LEFT(LOC_STRUCTNAME + SPACE$(39%), 39%) + LOC_SEQUENCE
	USE
		CONTINUE AddStruct IF ERR = 155%
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

	TK_FILE::FLDNAME	= LOC_DDL::FIELD_NAME(LOC_FLDLOOP%)
	TK_FILE::DESCRIPTION	= LOC_DDL::FIELD_DESC(LOC_FLDLOOP%)
	TK_FILE::DATABASE	= LOC_DATABASE
	TK_FILE::CLASSIFIER	= LOC_DDL::FIELD_ELEMENT(LOC_FLDLOOP%)
	TK_FILE::DATAARRAY	= LOC_DDL::FIELD_ATTRIBUTE(LOC_FLDLOOP%)
	TK_FILE::DATETYPE	= LOC_DDL::FIELD_TYPE(LOC_FLDLOOP%)
	TK_FILE::DATASIZE	= VAL%(LOC_DDL::FIELD_SIZE(LOC_FLDLOOP%))
	TK_FILE::CDATE		= DATE_TODAY
	TK_FILE::CTIME		= TIME_NOW

	UPDATE  #TK_FILE.CH%
	GOTO Ret18000

 AddStruct:
	TK_FILE::STRUCT		= LOC_STRUCTNAME
	TK_FILE::SEQUENCE	= LOC_SEQUENCE

	TK_FILE::FLDNAME	= LOC_DDL::FIELD_NAME(LOC_FLDLOOP%)
	TK_FILE::DESCRIPTION	= LOC_DDL::FIELD_DESC(LOC_FLDLOOP%)
	TK_FILE::DATABASE	= LOC_DATABASE
	TK_FILE::CLASSIFIER	= LOC_DDL::FIELD_ELEMENT(LOC_FLDLOOP%)
	TK_FILE::DATAARRAY	= LOC_DDL::FIELD_ATTRIBUTE(LOC_FLDLOOP%)
	TK_FILE::DATETYPE	= LOC_DDL::FIELD_TYPE(LOC_FLDLOOP%)
	TK_FILE::DATASIZE	= VAL%(LOC_DDL::FIELD_SIZE(LOC_FLDLOOP%))
	TK_FILE::CDATE		= DATE_TODAY
	TK_FILE::CTIME		= TIME_NOW

	PUT #TK_FILE.CH%

 Ret18000:
	RETURN

18020	!
	WHEN ERROR IN
		GET #TK_FILE.CH%, KEY #0% EQ &
			LEFT(LOC_STRUCTNAME + SPACE$(39%), 39%) + LOC_SEQUENCE
	USE
		CONTINUE AddRec IF ERR = 155%
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

	TK_FILE::FLDNAME	= ""
	TK_FILE::DESCRIPTION	= LOC_DDL::DESCR
	TK_FILE::DATABASE	= LOC_DATABASE
	TK_FILE::CLASSIFIER	= ""
	TK_FILE::DATAARRAY	= ""
	TK_FILE::DATETYPE	= LOC_OPN::ORGNIZATION
	TK_FILE::DATASIZE	= REC_SIZE
	TK_FILE::CDATE		= DATE_TODAY
	TK_FILE::CTIME		= TIME_NOW

	UPDATE  #TK_FILE.CH%
	GOTO Ret18020

 AddRec:
	TK_FILE::STRUCT		= LOC_STRUCTNAME
	TK_FILE::SEQUENCE	= LOC_SEQUENCE
	TK_FILE::FLDNAME	= ""
	TK_FILE::DESCRIPTION	= LOC_DDL::DESCR
	TK_FILE::DATABASE	= LOC_DATABASE
	TK_FILE::CLASSIFIER	= ""
	TK_FILE::DATAARRAY	= ""
	TK_FILE::DATETYPE	= LOC_OPN::ORGNIZATION
	TK_FILE::DATASIZE	= REC_SIZE
	TK_FILE::CDATE		= DATE_TODAY
	TK_FILE::CTIME		= TIME_NOW

	PUT #TK_FILE.CH%

 Ret18020:
	RETURN

18030	!
	WHEN ERROR IN
		FIND #TK_FILE.CH%, KEY #0% GE TRM$(LOC_STRUCTNAME)
	USE
		CONTINUE Ret18030 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

 NextRelat:
	WHEN ERROR IN
		GET #TK_FILE.CH%
	USE
		CONTINUE Ret18030 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

	GOTO Ret18030 IF TRM$(TK_FILE::STRUCT) <> TRM$(LOC_STRUCTNAME)

	DELETE #TK_FILE.CH%
	GOTO NextRelat

 Ret18030:
	RETURN

	%PAGE

 HelpError:
	!*******************************************************************
	! Help errors
	!*******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram


19000	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
