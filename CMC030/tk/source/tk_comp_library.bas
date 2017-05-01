1	%TITLE "Compile Function and Subroutines"
	%SBTTL "TK_COMP_LIBRARY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a y by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_COMP_LIBRARY/LINE
	!	$ LINK/EXE=TK_EXE: TK_COMP_LIBRARY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_COMP_LIBRARY.OBJ;*
	!
	! Author:
	!
	!	02/25/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Add ACCESS READ to open statements.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	EXTERNAL INTEGER	FUNCTION	COMP_STRING

	!
	! Dimension variables
	!
	DIM FILE_NAME$(1000%), &
		DIR_NAME$(100%)

	%PAGE

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%
	READ_FILE.CH% = 6%

	PRINT
	LINPUT "Name of command file to build <TEMP.COM>"; COM_FILE$

	OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
		DEFAULTNAME "TEMP.COM", &
		RECORDSIZE 132%

	PRINT #COM_FILE.CH%, "$ @CMC:LOGICALS"
	PRINT #COM_FILE.CH%, "$ SET NOON"

100	PRINT

	!
	! Get wildcard directory
	!
	LINPUT "Directory wildcard "; DIRECT$
	GOTO ExitProgram IF EDIT$(DIRECT$, -1%) = ""

	LINPUT "Func & sub wildcard <.BAS>"; MODULE$
	GOTO ExitProgram IF EDIT$(MODULE$, -1%) = ""

	LINPUT "Keep deleting .LIS and .MAP files <no> "; KEEP_CLEAN$
	KEEP_CLEAN$ = LEFT(EDIT$(KEEP_CLEAN$, -1%), 1%)
	NOLIST$ = ""
	NOLIST$ = "/NOLIST" IF KEEP_CLEAN$ = "Y"

	CALL FIND_FILE("$DISK3:[CMC0*]" + "*.DIR", DIR_NAME$(), 16%, "", "")
	DIR_LOOP% = VAL%(DIR_NAME$(0%))

200	!
	! Look up one file
	!
	FOR J% = 1% TO DIR_LOOP%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%

		PREFIX$ = "SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]"
		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".BAS", &
			FILE_NAME$(), 16%, "", "")

		LOOP% = VAL%(FILE_NAME$(0%))

		FOR I% = 1% TO LOOP%
			OPEN PREFIX$ + FILE_NAME$(I%) + ".BAS" &
				FOR INPUT AS FILE READ_FILE.CH%, &
				ACCESS READ, ALLOW MODIFY

			FUNCSUB% = 0%
			GOSUB ReadFile
			IF FUNCSUB% = -1%
			THEN
				PRINT PREFIX$ + FILE_NAME$(I%)
				PRINT #COM_FILE.CH%, '$WRITE SYS$OUTPUT ""'
				PRINT #COM_FILE.CH%, '$WRITE SYS$OUTPUT "Replacing ' + &
					EDIT$(FILE_NAME$(I%), 4%) + '"'
				PRINT #COM_FILE.CH%, "$ BAS " + PREFIX$ + &
					FILE_NAME$(I%) + "/LINE" + NOLIST$
				PRINT #COM_FILE.CH%, "$ LIB/REP SOURCE:[SMGFUN]" + &
					"CMCFUN " + FILE_NAME$(I%)
				PRINT #COM_FILE.CH%, "$ DELETE " + &
					FILE_NAME$(I%) + ".OBJ;*"
				PRINT #COM_FILE.CH%, "$!"
			END IF
			CLOSE READ_FILE.CH%
	NEXT I%

 NextJ:
	NEXT J%

	GOTO 100

 ExitProgram:

	CLOSE COM_FILE.CH%

	GOTO 32767

	!=====================================================================
 ReadFile:

10000	WHEN ERROR IN
		LINPUT #READ_FILE.CH%, INLINE$
	USE
		CONTINUE RetRead IF ERR = 11%
		EXIT HANDLER
	END WHEN

 NextLine:
	IF RIGHT(INLINE$, LEN(INLINE$)) = "&"
	THEN
		LINPUT #READ_FILE.CH%, INLINE1$
		INLINE$ = LEFT(INLINE$, LEN(INLINE$) - 1%) + " " + INLINE1$
		GOTO NextLine
	END IF

	TEMP$ = EDIT$(INLINE$, -1%)

	IF LEFT(TEMP$, 9%) = "!++"
	THEN
		GOTO RetRead
	END IF

	IF LEFT(TEMP$, 8%) = "FUNCTION" OR LEFT(TEMP$, 3%) = "SUB"
	THEN
		FUNCSUB% = -1%
		GOTO RetRead
	END IF

	GOTO ReadFile

 RetRead:
	RETURN

32767	END
