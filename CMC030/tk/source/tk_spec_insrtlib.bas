1	%TITLE "Read Help Messages from the Source Code"
	%SBTTL "TK_SPEC_INSRTLIB"
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
	!	.p
	!	Create documentation from source code.
	!
	! Index:
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_INSRTLIB/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_INSRTLIB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_INSRTLIB.OBJ;*
	!
	! Author:
	!
	!	11/21/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	05/29/90 - Lance Williams
	!		Modified the program to accept different types
	!		of programs for extraction.
	!
	!	12/14/90 - Kevin Handy
	!		Modified so that I am not locked out of the
	!		program being scanned.
	!
	!	01/09/91 - Kevin Handy
	!		Added better error trapping at 505 so that
	!		wouldn't give up if a file was deleted
	!		at some point during it's run.
	!
	!	07/27/92 - Kevin Handy
	!		Added somw comments while trying to figure out why
	!		this program insists on eating tabs in source
	!		comments.  (Had NO comments in it of any use)
	!
	!	07/27/92 - Kevin Handy
	!		Fixed bug where it was eating tabs in help messages.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Changed variable 'PROGRAM$' to 'FILENAME$'
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Change "[AP.-]" to "[000000]"
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELET_FILE instead of KILL
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD	UTL_REPORTX

	!
	! External Functions
	!
	EXTERNAL STRING  FUNCTION READ_SYSJOB
	EXTERNAL INTEGER FUNCTION COMP_STRING
	EXTERNAL LONG    FUNCTION LIBR_3INSERT


	!
	! Dimension variables
	!
	DIM	FILE_NAME$(1000%), &
		DIR_NAME$(100%), &
		PREFIX$(3%), &
		TASK$(3%), &
		EXT$(3%), &
		TEXT.LINES$(500%), &
		INDEX.TERM$(50%), &
		INDEX.LINES$(50%), &
		GLOSS.LINES$(50%)

	%PAGE

	!
	! Handle input/output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	ON ERROR GOTO 19000

	W_FILE_NAME$ = READ_SYSJOB + ".TMP"

	!*******************************************************************
	! Get info from the user
	!*******************************************************************

	PRINT "	This program will take the help messages from the source code"
	PRINT "	and place it in the help library on the REF:HELP_<Directory>.TLB."
	INPUT "Directory:   "; DIRECT$
	INPUT "Program, File, or Menu: "; NAME$
	INPUT "Module Name: "; MODULE$

	CALL FIND_FILE("SOURCE:[000000]*.DIR", DIR_NAME$(), 16%, "", "")
	DIR.LOOP% = VAL%(DIR_NAME$(0%))

	TASK$(1%) = "P"
	TASK$(2%) = "F"
	TASK$(3%) = "M"

	EXT$(1%) = ".BAS"
	EXT$(2%) = ".HLP"
	EXT$(3%) = ".HLP"

500	!*******************************************************************
	! Loop through all directories specified
	!*******************************************************************

	FOR J% = 1% TO DIR.LOOP%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%

		PREFIX$(1%) = "SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]"
		PREFIX$(2%) = "SOURCE:[" + DIR_NAME$(J%) + ".OPEN]"
		PREFIX$(3%) = "SOURCE:[" + DIR_NAME$(J%) + "]"

		IF LEN(DIR_NAME$(J%)) > 2%
		THEN
			LIB_NAME$  = "REF:HELP_DEFAULT.TLB"
		ELSE
			LIB_NAME$  = "REF:HELP_" + DIR_NAME$(J%) + ".TLB"
		END IF

	!*******************************************************************
	! Loop through three possible extensions
	!*******************************************************************

	FOR LOOP% = 1% TO 3%

		GOTO NextLoop IF COMP_STRING(TASK$(LOOP%), NAME$) = 0%

		CALL FIND_FILE(PREFIX$(LOOP%) + TRM$(MODULE$) + EXT$(LOOP%), &
			FILE_NAME$(), 16%, "", "")

		I.LOOP% = VAL%(FILE_NAME$(0%))

		!************************************************************
		! Loop through all files found
		!************************************************************

		FOR I% = 1% TO I.LOOP%

			!
			! Program type
			!
			POS1% = INSTR(1%, FILE_NAME$(I%), "_") + 1%
			POS2% = INSTR(POS1%, FILE_NAME$(I%), "_") - 1%
			PROG_TYPE$ = SEG$(FILE_NAME$(I%), POS1%, POS2%)

505			!
			! Open file to read from
			!
			WHEN ERROR IN
				OPEN PREFIX$(LOOP%) + FILE_NAME$(I%) + EXT$(LOOP%) &
					FOR INPUT AS FILE READ_FILE.CH%, &
					RECORDSIZE 132%, &
					ACCESS READ, &
					ALLOW MODIFY
			USE
				PRINT "Unable to open "; FILE_NAME$(I%)
				CONTINUE 540
			END WHEN

			!
			! Open the new file we are going to create
			!
			OPEN W_FILE_NAME$ FOR OUTPUT AS FILE WRIT_FILE.CH%, &
				RECORDSIZE 132%

			PRINT "Extacting from: " + PREFIX$(LOOP%) + &
				FILE_NAME$(I%) + EXT$(LOOP%)
			DESC$ = ""
			IDX%, LIN%, GLS% = 0%

510			!**************************************************
			! Get the file Title in the first line
			!**************************************************

			WHEN ERROR IN
				LINPUT #READ_FILE.CH%, TEXT$
			USE
				CONTINUE 540 IF ERR = 11%
				FILENAME$ = FILE_NAME$
				CONTINUE HelpError
			END WHEN

			IF INSTR(1%, TEXT$, "%TITLE")
			THEN
				TPOS1% = INSTR(1%, TEXT$, '"') + 1%
				TPOS2% = INSTR(TPOS1%, TEXT$, '"') - 1%
				TITLE$ = SEG$(EDIT$(TEXT$, 16%), &
					TPOS1%, TPOS2%)

				LIN% = LIN% + 1%
				TEXT.LINES$(LIN%) = "^*" + TITLE$ + "\*"
			END IF

			SEVERITY$ = ""

			!**************************************************
			! Handle rest of file
			!**************************************************
520			WHILE 1%

				WHEN ERROR IN
					LINPUT #READ_FILE.CH%, TEXT$
				USE
					CONTINUE 540 IF ERR = 11%
					FILENAME$ = FILE_NAME$
					CONTINUE HelpError
				END WHEN

				!
				! Check for message key
				!
				SEVERITY$ = "H" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!ABSTRACT:")
				SEVERITY$ = "W" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!WARNING:")
				SEVERITY$ = "I" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!INFORMATION:")
				SEVERITY$ = "S" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!SUCCESS:")
				SEVERITY$ = "E" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!ERROR:")
				SEVERITY$ = "F" &
					IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"!FATALERROR:")

				IF SEVERITY$ = "" OR ASCII(RIGHT(TEXT$, &
					INSTR(1%, TEXT$, "!") + 1%)) <> 32%
				THEN
					GOTO 530
				END IF

				COLON% = INSTR(1%, EDIT$(TEXT$, -1%), ":")
				TYPE_S$ = RIGHT(EDIT$(TEXT$, -1%), COLON% + 1%)

				KEY_NAME$ = SEVERITY$ + "$" + FILE_NAME$(I%) + &
					"$" + TYPE_S$
				TTL% = 0%

525				WHEN ERROR IN
					LINPUT #READ_FILE.CH%, TEXT$
				USE
					CONTINUE 540 IF ERR = 11%
					FILENAME$ = FILE_NAME$
					CONTINUE HelpError
				END WHEN

				GOTO 525 IF EDIT$(TEXT$, -1%) = "!"

				IF EDIT$(TEXT$, -1%) = "!INDEX:"
				THEN
					IDX% = 1%
					GOTO 525
				END IF

				IF EDIT$(TEXT$, -1%) = "!--" OR &
					(INSTR(1%, TEXT$, ":") <> 0% &
					AND ASCII(RIGHT(TEXT$, &
					INSTR(1%, TEXT$, "!") + 1%)) = 32%)
				THEN
					GOSUB Subcode
					GOTO 530
				END IF

				TEXT.LINE$ = RIGHT(EDIT$(TEXT$, 128%), 4%)
				DEXCL% = 0%
 CheckExcl:
				DEXCL% = INSTR(DEXCL% + 1%, TEXT.LINE$, "!!")
				IF DEXCL%
				THEN
					TEXT.LINE$ = &
						LEFT(TEXT.LINE$, DEXCL%) + &
						RIGHT(TEXT.LINE$, DEXCL% + 2%)
					GOTO CheckExcl
				END IF

				IF IDX% = 0%
				THEN
					LIN% = LIN% + 1%
					TEXT.LINES$(LIN%) = TEXT.LINE$
					IF LOOP% = 3% AND TTL% = 0%
					THEN
						TPOS1% = INSTR(1%, TEXT.LINE$, "^*") + 2%
						TPOS2% = INSTR(TPOS1%, TEXT.LINE$, "\*") - 1%
						TITLE$ = SEG$(EDIT$(TEXT.LINE$, 16%), &
							TPOS1%, TPOS2%)
						TTL% = -1%
					END IF

				ELSE
					IF LEFT(EDIT$(TEXT.LINE$, -1%), 2%) = ".Y"
					THEN
						GLS% = GLS% + 1%
						GLOSS.LINES$(GLS%) = TEXT.LINE$
					ELSE
						SPC% = INSTR(1%, TEXT.LINE$, " ")
						GTS% = INSTR(1%, TEXT.LINE$ + ">", ">")
						INDEX.TERM$(IDX%) = &
							EDIT$(SEG$(TEXT.LINE$, &
							SPC% + 1%, &
							GTS% - 1%), -1%)
						INDEX.LINES$(IDX%) = TEXT.LINE$
						IDX% = IDX% + 1%
					END IF
				END IF

				GOTO 525
530			NEXT

540		NEXT I%

		CLOSE #WRIT_FILE.CH%
		CLOSE #READ_FILE.CH%

 NextLoop:
		NEXT LOOP%
 NextJ:
		NEXT J%


 ExitProgram:
18000	!*******************************************************************
	! Exit program
	!*******************************************************************
 !	WHEN ERROR IN
 !		KILL W_FILE_NAME$
 !	USE
 !		CONTINUE EndProgram
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(W_FILE_NAME$ + ";*")

	GOTO EndProgram

 SubCode:
	!*******************************************************************
	! ??? Who knows ???
	!*******************************************************************
18500	FOR II% = 1% TO LIN%
		PRINT #WRIT_FILE.CH%, TEXT.LINES$(II%)

		IF II% = 1%
		THEN
			FOR III% = 1% TO IDX% - 1%
				PRINT #WRIT_FILE.CH%, INDEX.LINES$(III%)
			NEXT III%
		END IF

	NEXT II%

	FOR II% = 1% TO GLS%
		PRINT #WRIT_FILE.CH%, GLOSS.LINES$(II%)
	NEXT II%
	IDX%,LIN%,GLS% = 0%

	!
	! If help message for menu screen
	!
	IF LOOP% = 3%
	THEN
		PRINT #WRIT_FILE.CH%, ".!!"
		PRINT #WRIT_FILE.CH%, ".!.SCREEN " + TYPE_S$ + "$SCREEN" + &
			"\caption{" + TITLE$ + " Menu Screen}"
		GOTO Opt
	END IF

	HLP% = INSTR(1%, TYPE_S$, "HELP")
	GOTO Opt IF HLP% = 0% AND TYPE_S$ <> "COMMAND"

	IF HLP%>1%
	THEN
		K.EXT$ = "$" + LEFT(TYPE_S$, HLP% - 2%)
	ELSE
		K.EXT$ = ""
	END IF

	SELECT PROG_TYPE$

	CASE "RPRT", "POST", "FORM"

		PRINT #WRIT_FILE.CH%, ".!!"
		PRINT #WRIT_FILE.CH%, ".!.SCREEN " + &
			FILE_NAME$(I%) + K.EXT$ + "$SCREEN" + &
			"\caption{" + TITLE$ + " Screen}"

		PRINT #WRIT_FILE.CH%, ".!.FIELD H$" + &
			FILE_NAME$(I%) + K.EXT$ + "$FLD*"

		PRINT #WRIT_FILE.CH%, ".!!"
		LINE$ = ".!.SCREEN " + FILE_NAME$(I%) + K.EXT$ + &
			"$REPORT" + "\caption{" + TITLE$
		! Add Report to title if not last word in title
		LINE$ = LINE$ +	" Report" IF 0 = &
			INSTR(LEN(TITLE$) - 10%,TITLE$, " Report")

		PRINT #WRIT_FILE.CH%, LINE$ + "}"

	CASE ELSE
		GOTO Opt IF LEN(PROG_TYPE$) <> 4%

		PRINT #WRIT_FILE.CH%, ".!!"
		PRINT #WRIT_FILE.CH%, ".!.SCREEN " + &
			FILE_NAME$(I%) + K.EXT$ + "$SCREEN" + &
			"\caption{" + TITLE$ + " Screen}"

		PRINT #WRIT_FILE.CH%, ".!.FIELD H$" + &
			FILE_NAME$(I%) + K.EXT$ + "$FLD*"

	END SELECT

	!*******************************************************************
	! ??? Who knows ???
	!*******************************************************************
 Opt:
	GOTO PutLib IF EDIT$(TEXT$, -1%) <> "!OPTION:"

18510	LINPUT #READ_FILE.CH%, TEXT$

	GOTO 18510 IF EDIT$(TEXT$, -1%) = "!" &

	IF EDIT$(TEXT$, -1%) = "!--" OR &
		(INSTR(1%, TEXT$, ":") <> 0% AND ASCII(RIGHT(TEXT$, &
		INSTR(1%, TEXT$, "!") + 1%)) = 32%)
	THEN
		GOTO PutLib
	END IF

	PRINT #WRIT_FILE.CH%, ".!!"
	PRINT #WRIT_FILE.CH%, ".!.OPTION H$" + RIGHT(EDIT$(TEXT$, 8%), 3%)

	GOTO 18510

 PutLib:
	!*******************************************************************
	! Place in library
	!*******************************************************************
	CLOSE #WRIT_FILE.CH%

	ST% = LIBR_3INSERT(LIB_NAME$, W_FILE_NAME$, KEY_NAME$)

	PRINT SPACE$(12%) + &
		LEFT(KEY_NAME$ + STRING$(40%, A"."B), 40%) + LIB_NAME$

18550 !	WHEN ERROR IN
 !		KILL W_FILE_NAME$
 !	USE
 !		CONTINUE EndKill
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(W_FILE_NAME$ + ";*")

 !	GOTO 18550

 EndKill:
	!*******************************************************************
	! Reopen the new file we are going to create
	!*******************************************************************
	OPEN W_FILE_NAME$ FOR OUTPUT AS FILE WRIT_FILE.CH%, &
		RECORDSIZE 132%

	SEVERITY$ = ""
	RETURN

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	PRINT ERN$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
		"ERR", FILENAME$, "ERROR" + NUM1$(ERR)

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME HelpError

 EndProgram:
32767	END
