1	%TITLE "Insert Help Message from a Text Library in the Source Module"
	%SBTTL "HELP_INSERTSOURCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG HELP_INSERTSOURCE(LIBR.NAME$,KEY.NAME$)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!
	! Abstract:HELP
	!	.p
	!	This function will extract help message from a text
	!	library and insert it in the corresponding source
	!	module.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_INSERTSOURCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP HELP_INSERTSOURCE
	!	$ DELETE HELP_INSERTSOURCE.OBJ;*
	!
	! Author:
	!
	!	12/12/89 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/15/90 - Frank F. Starman
	!		If cannot find key in the source code then append
	!		help text to the end. Translate exclamation point to
	!		double exclamation point in the source code.
	!
	!	03/15/92 - Kevin Handy
	!		Modified 18000 to include "SYS$LOGIN:" onto file
	!		name so that code doesn't get confused when user
	!		does a "SET DEF".
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
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
	! External functions
	!
	EXTERNAL LONG FUNCTION LIBR_EXTRACT

	DIM INDEX$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Figure out name of the source code and help message type
	!
	PREF% = INSTR(1%, KEY.NAME$, "$")
	SUF%  = INSTR(PREF% + 1%, KEY.NAME$, "$")

	PROG.NAME$ = MID(KEY.NAME$, PREF% + 1%, SUF% - PREF% - 1%)
	ITEM.NAME$ = RIGHT(KEY.NAME$, SUF% + 1%)

	SELECT LEFT(KEY.NAME$, 1%)
	CASE "E"
		HELP.TYPE$ = "Error:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	CASE "F"
		HELP.TYPE$ = "FatalError:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	CASE "S"
		HELP.TYPE$ = "Success:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	CASE "I"
		HELP.TYPE$ = "Information:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	CASE "W"
		HELP.TYPE$ = "Warning:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	CASE ELSE
		HELP.TYPE$ = "Abstract:" + EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%)
	END SELECT

	!
	! Figure out directory name and source code type
	!
	PREF% = INSTR(1%, PROG.NAME$ + "_", "_")

	!
	! Which is it in?
	!
	IF PREF% = 1%
	THEN
		UNDER% = INSTR(1%, ITEM.NAME$ + "_", "_")
		DIR.NAME$ = LEFT(ITEM.NAME$, UNDER% - 1%)
		PROG.NAME$ = DIR.NAME$
	ELSE
		DIR.NAME$  = LEFT(PROG.NAME$, PREF% - 1%)
	END IF

	DIR.NAME$  = "UTL" IF DIR.NAME$ = "UT"

1000	!
	! Read original copy of the source code
	!
	CALL ASSG_CHANNEL(READ.FILE.CH%, STAT%)
	WHEN ERROR IN
		OPEN "SOURCE:[" + DIR.NAME$ + ".SOURCE]" + PROG.NAME$ + ".BAS" &
			FOR INPUT AS FILE READ.FILE.CH%
	USE
		IF ERR = 1% AND DIR.NAME$ <> "CMCFUN"
		THEN
			DIR.NAME$ = "CMCFUN"
			RETRY
		END IF

		CONTINUE 1010 IF ERR = 5%
		FILENAME$ = PROG.NAME$
		CONTINUE HelpError
	END WHEN

	LOCATION$ = "SOURCE:[" + DIR.NAME$ + ".SOURCE]"
	EXTEN$ = "BAS"
	GOTO 1050

1010	!
	! Read original copy of the file help
	!
	WHEN ERROR IN
		OPEN "SOURCE:[" + DIR.NAME$ + ".OPEN]" + PROG.NAME$ + ".HLP" &
			FOR INPUT AS FILE READ.FILE.CH%
	USE
		CONTINUE 1020 IF ERR = 5%
		FILENAME$ = PROG.NAME$
		CONTINUE HelpError
	END WHEN

	LOCATION$ = "SOURCE:[" + DIR.NAME$ + ".OPEN]"
	EXTEN$ = "HLP"
	GOTO 1050

1020	GOTO ExitFunction IF DIR.NAME$ <> PROG.NAME$

	!
	! Read original copy of the menu help
	!
	WHEN ERROR IN
		OPEN "SOURCE:[" + DIR.NAME$ + "]" + PROG.NAME$ + ".HLP" &
			AS FILE READ.FILE.CH%
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = PROG.NAME$
		CONTINUE HelpError
	END WHEN

	LOCATION$ = "SOURCE:[" + DIR.NAME$ + "]"
	EXTEN$ = "HLP"

1050	!
	! Create new copy of the source code
	!
	CALL ASSG_CHANNEL(WRIT.FILE.CH%, STAT%)
	WHEN ERROR IN
		OPEN LOCATION$ + PROG.NAME$ + ".TMP" &
			FOR OUTPUT AS FILE WRIT.FILE.CH%, &
			RECORDSIZE 255%
	USE
		CONTINUE InsertDone IF ERR = 11%
		FILENAME$ = PROG.NAME$
		CONTINUE HelpError
	END WHEN

	CALL HELP_34MESSAGE(SCOPE, "insert in the source", "I", PROG.NAME$, &
		"", EDIT$(RIGHT(KEY.NAME$, SUF% + 1%), -1%))

	WHEN ERROR IN
		LINPUT #READ.FILE.CH%, TEXT$
	USE
		CONTINUE InsertDone IF ERR = 11%
		FILENAME$ = PROG.NAME$
		CONTINUE HelpError
	END WHEN

	IF HELP.TYPE$ = "Abstract:HELP"
	THEN

		!
		! name temp file
		!
		TEMP.FILE$ = "HELP_INSERTSOURCE" + READ_SYSJOB + ".TMP"

		!
		! Extract help from library
		!
		ST% = LIBR_EXTRACT(LIBR.NAME$, TEMP.FILE$, KEY.NAME$)

		CALL ASSG_CHANNEL(TEMP.FILE.CH%, STAT%)
		WHEN ERROR IN
			OPEN TEMP.FILE$ FOR INPUT AS FILE TEMP.FILE.CH%
		USE
			CONTINUE InsertDone IF ERR = 11%
			FILENAME$ = PROG.NAME$
			CONTINUE HelpError
		END WHEN

		WHEN ERROR IN
			LINPUT #TEMP.FILE.CH%, HELPTEXT$
		USE
			CONTINUE InsertDone IF ERR = 11%
			FILENAME$ = PROG.NAME$
			CONTINUE HelpError
		END WHEN

		POS1% = INSTR(1%, HELPTEXT$, "*") + 1%
		POS2% = INSTR(POS1%, HELPTEXT$, "\")
		TEXT$ = SEG$(HELPTEXT$, POS1%, POS2% - 1%)

		IF POS1% = 1% OR POS2% = 0%
		THEN
			CALL HELP_34MESSAGE(SCOPE, &
				"missing title on the first line", "W", &
				"HELP_INSERTSOURCE", "", "MISSTITLE")
			GOTO ExitFunction
		END IF

		TEXT$ = '1	%TITLE "' + TEXT$ + '"'
	END IF

	PRINT #WRIT.FILE.CH%, EDIT$(TEXT$, 4%)

	!
	! Read line by line and search for help message block
	!
	WHILE 1%
1100		WHEN ERROR IN
			LINPUT #READ.FILE.CH%, TEXT$
		USE
			CONTINUE InsertDone IF ERR = 11%
			FILENAME$ = DIR.NAME$ + PROG.NAME$
			CONTINUE HelpError
		END WHEN

		PRINT #WRIT.FILE.CH%, EDIT$(TEXT$, 4%)

		IF EDIT$(TEXT$, 2% + 4%) = "!" + HELP.TYPE$ AND &
			ASCII(RIGHT(TEXT$, INSTR(1%, TEXT$, "!") + 1%)) = 32%
		THEN
			GOSUB InsertHelp
			CALL HELP_34MESSAGE(SCOPE, "...inserted ", "I", &
				SCOPE::PRG_PROGRAM, "", HELP.TYPE$)
			INSRT.FLAG% = -1%

 ReadSource:
			WHEN ERROR IN
				LINPUT #READ.FILE.CH%, TEXT$
			USE
				CONTINUE InsertDone IF ERR = 11%
				FILENAME$ = DIR.NAME$ + PROG.NAME$
				CONTINUE HelpError
			END WHEN

			!
			! Check if insert under a different key - connected
			!
			IF INSTR(1%,EDIT$(TEXT$, -1%), "!CONNECT:")
			THEN
				COLON% = INSTR(1%, EDIT$(TEXT$, -1%), ":")
				TYPE_S$ = RIGHT(EDIT$(TEXT$, -1%), COLON% + 1%)

				COLON% = INSTR(1%, HELP.TYPE$, ":")
				HELP.TYPE$ = LEFT(HELP.TYPE$, COLON%) + TYPE_S$
				INSRT.FLAG% = 0%

				RESTORE #READ.FILE.CH%
				CLOSE WRIT.FILE.CH%

				KILL LOCATION$ + PROG.NAME$ + ".TMP"
				GOTO 1010
			END IF

			GOTO ReadSource IF EDIT$(TEXT$, -1%) = "!INDEX:"

			GOTO ReadSource IF EDIT$(TEXT$, -1%) <> "!--" AND &
				(MID(EDIT$(TEXT$, -1%), &
				LEN(EDIT$(TEXT$, -1%)), 1%) <> ":" OR &
				ASCII(RIGHT(TEXT$, &
				INSTR(1%, TEXT$, "!") + 1%)) <> 32%)

			PRINT #WRIT.FILE.CH%, EDIT$(TEXT$, 4%)

		END IF
	NEXT

 InsertDone:
1120	IF INSRT.FLAG% = 0%
	THEN
		PRINT #WRIT.FILE.CH%,  "	!+-+-+" IF EXTEN$ = "BAS"
		PRINT #WRIT.FILE.CH%,  "	!++"
		PRINT #WRIT.FILE.CH%,  "	! " + HELP.TYPE$
		GOSUB InsertHelp
		PRINT #WRIT.FILE.CH%,  "	!--"

		CALL HELP_34MESSAGE(SCOPE, "key at the end of the source code", &
			"W", "HELP_INSERTSOURCE", HELP.TYPE$, "KEYATEND") &
			IF EXTEN$ = "BAS"

	END IF

	CLOSE READ.FILE.CH%
	CLOSE WRIT.FILE.CH%

	WHEN ERROR IN
		NAME LOCATION$ + PROG.NAME$ + ".TMP" AS &
			LOCATION$ + PROG.NAME$ + "." + EXTEN$

		KILL LOCATION$ + PROG.NAME$ + "." + EXTEN$ + ";-2"
	USE
		CONTINUE ExitFunction
	END WHEN

 ExitFunction:
	!CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	CALL ASSG_FREECHANNEL(READ.FILE.CH%)
	CALL ASSG_FREECHANNEL(WRIT.FILE.CH%)

	EXIT FUNCTION

	%PAGE

 InsertHelp:
18000	SELECT HELP.TYPE$
	CASE "Abstract:HELP"
		!
		! Already open
		!
	CASE ELSE
		!
		! Abstract:FLD,...
		!
		!
		! name temp file
		!
		TEMP.FILE$ = "SYS$LOGIN:HELP_INSERTSOURCE" + &
			READ_SYSJOB + ".TMP"

		!
		! Extract help from library
		!
		ST% = LIBR_EXTRACT(LIBR.NAME$, TEMP.FILE$, KEY.NAME$)

		CALL ASSG_CHANNEL(TEMP.FILE.CH%, STAT%)
		OPEN TEMP.FILE$ FOR INPUT AS FILE TEMP.FILE.CH%

		LINPUT #TEMP.FILE.CH%, HELPTEXT$

		PRINT #WRIT.FILE.CH%,  "	!	" + EDIT$(HELPTEXT$, 4%)
	END SELECT

	IND% = 0%

18010	WHILE 1%
		WHEN ERROR IN
			LINPUT #TEMP.FILE.CH%, HELPTEXT$
		USE
			CONTINUE KillF IF ERR = 11%
			FILENAME$ = TEMP.FILE$
			CONTINUE HelpError
		END WHEN

		GOTO GoNext IF LEFT(HELPTEXT$, 2%) = ".!"

		TEXT.LINE$ = EDIT$(HELPTEXT$, 4%)
		DEXCL% = -1%
 CheckExcl:
		!
		! Search for exclamation points and double them
		!
		DEXCL% = INSTR(DEXCL% + 2%, TEXT.LINE$, "!")
		IF DEXCL%
		THEN
			TEXT.LINE$ = LEFT(TEXT.LINE$, DEXCL%) + &
				RIGHT(TEXT.LINE$, DEXCL%)
			GOTO CheckExcl
		END IF

		IF LEFT(EDIT$(TEXT.LINE$, -1%), 2%) = ".X" OR &
			LEFT(EDIT$(TEXT.LINE$, -1%), 2%) = ".Y"
		THEN
			IND% = IND% + 1%
			INDEX$(IND%) = "	!	" + TEXT.LINE$
		ELSE
			PRINT #WRIT.FILE.CH%,  "	!	" + TEXT.LINE$
		END IF
 GoNext:
	NEXT
 KillF:
	!
	! Write index
	!
	PRINT #WRIT.FILE.CH%,  "	!"
	PRINT #WRIT.FILE.CH%,  "	! Index:"
	PRINT #WRIT.FILE.CH%, INDEX$(K%) FOR K% = 1% TO IND%
	PRINT #WRIT.FILE.CH%,  "	!"

	CLOSE TEMP.FILE.CH%
	KILL TEMP.FILE$
	CALL ASSG_FREECHANNEL(TEMP.FILE.CH%)
	RETURN

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitFunction

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
