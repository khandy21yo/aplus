1	%TITLE "Read Program Code & Modifiy It"
	%SBTTL "TK_SPEC_MODTITLE"
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
	!	Read a program line by line and change, delete, or add
	!	the lines it reads.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_MODTITLE/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_MODTITLE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_MODTITLE.OBJ;*
	!
	! Author:
	!
	!	11/03/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
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
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Use "[000000]' instead of "[AP.-]"
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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
	EXTERNAL INTEGER FUNCTION LIBR_EXTRACT

	!
	! Dimension variables
	!
	DIM FILE_NAME$(1000%), DIR_NAME$(100%), INDEX$(50%)

	!
	! Handle output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	ON ERROR GOTO 19000

	!
	! Get info from the user
	!
	INPUT "Directory:   "; DIRECT$
	INPUT "From Dir :   "; FROM.DIRECT$
	INPUT "Module Name: "; MODULE$

	CALL FIND_FILE("SOURCE:[000000]*.DIR", DIR_NAME$(), 16%, "", "")
	DIR.LOOP% = VAL%(DIR_NAME$(0%))

	CALL ASSG_CHANNEL(OUTPUT_FILE.CH%, STAT%)
	OPEN "MODTITLE.TMP" FOR OUTPUT AS FILE OUTPUT_FILE.CH%, &
		RECORDSIZE 132%

500	!
	! Look up one file
	!
	FOR J% = 1% TO DIR.LOOP%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%
		GOTO NextJ IF DIR_NAME$(J%) < FROM.DIRECT$

		PREFIX$ = "SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]"
		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".BAS", &
			FILE_NAME$(), 16%, "", "")

		I_LOOP% = VAL%(FILE_NAME$(0%))

		FOR I% = 1% TO I_LOOP%

			PREF% = INSTR(1%, FILE_NAME$(I%), "_")
			SUF%  = INSTR(PREF% + 1%, FILE_NAME$(I%), "_")
			PROG_TYPE$ = MID(FILE_NAME$(I%), PREF% + 1%, &
				SUF% - PREF% - 1%)

			SELECT PROG_TYPE$
			CASE "MAIN", "MAST", "QURY", "SPEC", "CLOS", &
				"CONV", "JOUR", "CNTR", "RPRT", "POST", "FORM"
				!
				! Read help
				!
			CASE ELSE
				GOTO 525
			END SELECT

505			! Open file to read from
			OPEN PREFIX$ + FILE_NAME$(I%) + ".BAS" &
				FOR INPUT AS FILE READ_FILE.CH%, &
				ACCESS READ, ALLOW MODIFY

			!Open the new file we are going to create
			OPEN PREFIX$ + FILE_NAME$(I%) + ".TMP" &
				FOR OUTPUT AS FILE WRIT_FILE.CH%, &
				RECORDSIZE 255%

			PRINT
			PRINT "Extacting from: " + &
				PREFIX$ + FILE_NAME$(I%) + ".BAS"

			!PRINT #OUTPUT_FILE.CH%, "*************** " + &
			!	PREFIX$ + FILE_NAME$(I%) + ".BAS"
			DONE%,EXTR% = 0%

510			WHILE 1%
				WHEN ERROR IN
					LINPUT #READ_FILE.CH%, TEXT$
				USE
					CONTINUE 520 IF ERR = 11%
					CONTINUE HelpError
				END WHEN

				!
				! Here is what needs to be done
				!
				IF MID(TEXT$, LEN(TEXT$), 1%) = ":"
				THEN
					ABSTRACT% = 0%
					INDEX% = 0%
				END IF

				IF EDIT$(TEXT$, -1%) = "!--"
				THEN
					DONE% = -1%
				END IF

				GOTO NextLine IF ABSTRACT% AND EXTR%

				IF INDEX%
				THEN
					GOTO NextLine
				END IF

				IF (INSTR(1%, TEXT$, "Abstract:") &
					OR  INSTR(1%, TEXT$, "ABSTRACT:")) AND &
					DONE% = 0%
				THEN
					ABSTRACT% = -1%
					GOSUB InsertText
					GOTO NextLine IF EXTR%
				END IF

				IF INSTR(1%,TEXT$, "Index:") AND DONE% = 0%
				THEN
					INDEX% = -1%
					GOTO NextLine
				END IF

				PRINT #WRIT_FILE.CH%, EDIT$(TEXT$, 4%)

 NextLine:
			NEXT

520			CLOSE READ_FILE.CH%
			CLOSE WRIT_FILE.CH%
			NAME PREFIX$ + FILE_NAME$(I%) + ".TMP" AS &
				PREFIX$ + FILE_NAME$(I%) + ".BAS"

525		NEXT I%

 NextJ:
	NEXT J%

 ExitProgram:
	GOTO EndProgram

 InsertText:
18000	GOTO Ret IF EXTR%

	SELECT PROG_TYPE$
	CASE "MAIN", "MAST", "QURY", "SPEC", "CLOS", "CONV", "JOUR", "CNTR"
		ST% = LIBR_EXTRACT("REF:HELP_" + DIR_NAME$(J%), "TEMP.TMP", &
			"PROG$" + FILE_NAME$(I%) + "$HELP")

		IF (ST% AND 1%) <> 1%
		THEN
			IF PROG_TYPE$ = "MAIN"
			THEN
				MAST$ = LEFT(FILE_NAME$(I%), PREF%) + "MAST" + &
					RIGHT(FILE_NAME$(I%), SUF%)
				ST% = LIBR_EXTRACT("REF:HELP_" + DIR_NAME$(J%), &
					"TEMP.TMP", "PROG$" + MAST$ + "$HELP")

				IF (ST% AND 1%) <> 1%
				THEN
					CNTR$ = LEFT(FILE_NAME$(I%), PREF%) + "CNTR" + &
						RIGHT(FILE_NAME$(I%), SUF%)
					ST% = LIBR_EXTRACT("REF:HELP_" + &
						DIR_NAME$(J%), "TEMP.TMP", &
						"PROG$" + CNTR$ + "$HELP")
				END IF

				IF (ST% AND 1%) <> 1%
				THEN
					JOUR$ = LEFT(FILE_NAME$(I%),PREF%) + "JOUR" + &
						RIGHT(FILE_NAME$(I%), SUF%)
					ST% = LIBR_EXTRACT("REF:HELP_" + &
						DIR_NAME$(J%), "TEMP.TMP", &
						"PROG$" + JOUR$ + "$HELP")
				END IF

			END IF

			IF (ST% AND 1%) <> 1%
			THEN
				PRINT "Can't find " + FILE_NAME$(I%)
				GOTO Ret
			END IF
		END IF

	CASE "RPRT", "POST", "FORM"

		ST% = LIBR_EXTRACT("REF:HELP_" + DIR_NAME$(J%), "TEMP.TMP", &
			"REPO$" + FILE_NAME$(I%) + "$HELP")

		IF (ST% AND 1%) <> 1%
		THEN
			PRINT "Can't find " + FILE_NAME$(I%)
			GOTO Ret
		END IF

	CASE ELSE

		PRINT "Can't find " + FILE_NAME$(I%)
		GOTO Ret

	END SELECT

	IND%,FLAG.TEXT% = 0%

	OPEN "TEMP.TMP" FOR INPUT AS FILE 50%
	PRINT #WRIT_FILE.CH%,  "	! Abstract:HELP"
	PRINT #WRIT_FILE.CH%,  "	!"
	EXTR% = -1%

18100	WHILE 1%
		WHEN ERROR IN
			LINPUT #50%, TEXT$
		USE
			CONTINUE KillF IF ERR = 11%
			CONTINUE HelpError
		END WHEN

		GOTO KillF IF LEFT(TEXT$, 2%) = ".!"

		IF LEFT(EDIT$(TEXT$, -1%), 2%) = ".X"
		THEN
			IND% = IND% + 1%
			INDEX$(IND%) = "	!	" + EDIT$(TEXT$, 4%)
		END IF

		FLAG.TEXT% = -1% IF LEFT(EDIT$(TEXT$, -1%), 2%) = ".P"

		IF FLAG.TEXT%
		THEN
			PRINT #WRIT_FILE.CH%,  "	!	" + &
				EDIT$(TEXT$, 4%)
		END IF

	NEXT
 KillF:
	PRINT #WRIT_FILE.CH%,  "	!"

	!
	! Write index
	!
	PRINT #WRIT_FILE.CH%,  "	! Index:"
	PRINT #WRIT_FILE.CH%,  "	!"
	PRINT #WRIT_FILE.CH%, INDEX$(K%) FOR K% = 1% TO IND%
	PRINT #WRIT_FILE.CH%,  "	!"
 !	KILL "TEMP.TMP"

	SMG_STATUS% = LIB$DELETE_FILE("TEMP.TMP;*")

 Ret:
	RETURN

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO EndProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 EndProgram:
32767	END
