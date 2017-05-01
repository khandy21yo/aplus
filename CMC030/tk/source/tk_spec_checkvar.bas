1	%TITLE "Set Relation Among Models"
	%SBTTL "TK_SPEC_CHECKVAR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This program is used to check variables in source code.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CHECKVAR.BAS/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_CHECKVAR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CHECKVAR.OBJ;*
	!
	! Author:
	!
	!	03/13/92 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/30/2000 - Kevin Handy
	!		Fix bugs, try to make this sucker work again.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Dimension
	!
	DIM CROSS_REF$(1000%)

	IGNOREVAR$ = "~V%~MLOOP~MOPTION~I%~J%~LOOP%~LOOP1%~LOOP2%~" + &
		"SMG_STATUS%~ST%~SYS_STATUS%~I~J~PR_LOOP%~"

10	!
	! Array telling if good (1) or bad (0)
	!
	!	0 - VAR
	!	1 - FUNC
	!	2 - SUB
	!	3 - CONST
	!	4 - RECORD
	!
	!	1 - DEF
	!	2 - WRITE
	!	4 - READ
	!
	DIM GOODBAD%(5%, 7%)

	!
	! VAR
	!
	GOODBAD%(0%, 0%) = -1%
	GOODBAD%(0%, 1%) = -1%
	GOODBAD%(0%, 2%) = -1%
	GOODBAD%(0%, 3%) = -1%
	GOODBAD%(0%, 4%) = -1%
	GOODBAD%(0%, 5%) = -1%
	GOODBAD%(0%, 6%) = 0%
	GOODBAD%(0%, 7%) = 0%

	!
	! FUNC
	!
	GOODBAD%(1%, 0%) = -1%
	GOODBAD%(1%, 1%) = 0%
	GOODBAD%(1%, 2%) = -1%
	GOODBAD%(1%, 3%) = 0%
	GOODBAD%(1%, 4%) = 0%
	GOODBAD%(1%, 5%) = 0%
	GOODBAD%(1%, 6%) = 0%
	GOODBAD%(1%, 7%) = 0%

	!
	! SUB
	!
	GOODBAD%(2%, 0%) = -1%
	GOODBAD%(2%, 1%) = -1%
	GOODBAD%(2%, 2%) = 0%
	GOODBAD%(2%, 3%) = 0%
	GOODBAD%(2%, 4%) = 0%
	GOODBAD%(2%, 5%) = 0%
	GOODBAD%(2%, 6%) = 0%
	GOODBAD%(2%, 7%) = 0%

	!
	! CONST
	!
	GOODBAD%(3%, 0%) = -1%
	GOODBAD%(3%, 1%) = 0%
	GOODBAD%(3%, 2%) = 0%
	GOODBAD%(3%, 3%) = 0%
	GOODBAD%(3%, 4%) = 0%
	GOODBAD%(3%, 5%) = 0%
	GOODBAD%(3%, 6%) = 0%
	GOODBAD%(3%, 7%) = 0%

	!
	! RECORD
	!
	GOODBAD%(4%, 0%) = -1%
	GOODBAD%(4%, 1%) = -1%
	GOODBAD%(4%, 2%) = -1%
	GOODBAD%(4%, 3%) = 0%
	GOODBAD%(4%, 4%) = -1%
	GOODBAD%(4%, 5%) = 0%
	GOODBAD%(4%, 6%) = -1%
	GOODBAD%(4%, 7%) = 0%

	!
	! ANYTHING GOES
	!
	GOODBAD%(5%, 0%) = 0%
	GOODBAD%(5%, 1%) = 0%
	GOODBAD%(5%, 2%) = 0%
	GOODBAD%(5%, 3%) = 0%
	GOODBAD%(5%, 4%) = 0%
	GOODBAD%(5%, 5%) = 0%
	GOODBAD%(5%, 6%) = 0%
	GOODBAD%(5%, 7%) = 0%

100	TEXT.CH% = 6%
	OUTPUT.CH% = 7%

	OPEN "PROGRAM_ERROR.LOG" AS FILE OUTPUT.CH%, &
		ACCESS APPEND

1000	!*******************************************************************
	! In this section the cross reference list will be read
	!*******************************************************************

	CALL FIND_FILE("*.CRO", CROSS_REF$(), 16%, "", "")

	CROSS_CNT% = VAL%(CROSS_REF$(0%))

	!
	! Loop for all cross reference files
	!
	FOR CROSS_LOOP% = 1% TO CROSS_CNT%

1120		!
		! Open cross reference listing
		!
		OPEN CROSS_REF$(CROSS_LOOP%) + ".CRO" FOR INPUT AS FILE TEXT.CH%, &
			ACCESS READ, ALLOW MODIFY

		GOSUB 2000

	NEXT CROSS_LOOP%

	GOTO 32767

2000	!*******************************************************************
	! Scan through cross reference looking for intresting things
	!*******************************************************************

	THIS_VAR$ = ""
	THIS_TYPE% = 5%
	THIS_FLAG% = 0%
	EOFFLAG% = 0%

2010	!
	! Get a line
	!
	GOSUB 8000

	GOTO 2900 IF EOFFLAG%

	GOTO 2900 IF TXT$ = "Common Cross Reference"
	GOTO 2900 IF TXT$ = "Map Cross Reference"
	GOTO 2900 IF TXT$ = "Line Cross Reference"
	GOTO 2900 IF TXT$ = "Label Cross Reference"

	!
	! Handle type if stuff on line
	!
	IF LEFT(TXT$, 1%) = " "
	THEN
		!
		! Line numbers
		!
		GOSUB LineNumbers
	ELSE
		!
		! Variable Names
		!
		GOSUB VarName

		IF (THIS_VAR$ <> VR$)
		THEN
			!
			! New variable name
			!
			IF GOODBAD%(THIS_TYPE%, THIS_FLAG%)
			THEN
				GOSUB PrintFileName
				print #OUTPUT.CH%, "   "; THIS_VAR$
			END IF

			THIS_VAR$ = VR$
			THIS_TYPE% = TY%
			THIS_FLAG% = 0%

		END IF
	END IF

	GOTO 2010

2900	!*******************************************************************
	! Done
	!*******************************************************************

	RETURN

 PrintFileName:
	!*******************************************************************
	! Print out file name if haven't printed it yet
	!*******************************************************************

	IF THIS_NAME$ <> CROSS_REF$(CROSS_LOOP%)
	THEN
		PRINT #OUTPUT.CH%
		PRINT #OUTPUT.CH%, "File: "; CROSS_REF$(CROSS_LOOP%)
	END IF

	THIS_NAME$ = CROSS_REF$(CROSS_LOOP%)

	RETURN

 VarName:
3000	!*******************************************************************
	! Handle Variable names
	!*******************************************************************

	IF (LEFT(TXT$, 4%) = "User")
	THEN
		RETURN
	END IF

	IF (LEFT(TXT$, 6%) = "Symbol")
	THEN
		RETURN
	END IF

3100	X$ = MID(TXT$, 54%, 16%)
	I% = INSTR(1%, TXT$, " ")
	VR$ = LEFT(TXT$, I% - 1%)

	SELECT X$
	CASE "Ext. FUNCTION"
		TY% = 1%

	CASE "Ext. SUB"
		TY% = 2%

	CASE "CONSTANT"
		TY% = 3%

	CASE "RECORD"
		TY% = 4%

	CASE "MAP"
		TY% = 5%

	CASE ELSE
		TY% = 0%
	END SELECT

	!
	! Ignore certain variables that are used in odd ways for
	! generally useful putposes.
	!
	IF INSTR(1%, IGNOREVAR$, "~" + VR$ + "~")
	THEN
		VR$ = ""
		TY% = 5%
	END IF

3990	RETURN

 LineNumbers:
4000	!*******************************************************************
	! Handle line numbers
	!*******************************************************************

	!
	! Skip useless stuff
	!
	RETURN IF THIS_VAR$ = ""

	TXT$ = EDIT$(TXT$, 8%)

4100	!
	! For all line numbers
	!
	WHILE TXT$ <> ""

		!
		! Pull off on line number
		!
		I% = INSTR(1%, TXT$, " ")

		IF I%
		THEN
			LL$ = LEFT(TXT$, I% - 1%)
			TXT$ = EDIT$(RIGHT(TXT$, I% + 1%), 8%)
		ELSE
			LL$ = TXT$
			TXT$ = ""
		END IF

		!
		! Handle type of line
		!
		SELECT RIGHT(LL$, LEN(LL$))

		CASE "#"
			THIS_FLAG% = THIS_FLAG% OR 1%

		CASE "@"
			THIS_FLAG% = THIS_FLAG% OR 2%

		CASE "P"
			THIS_FLAG% = THIS_FLAG% OR 6%

		CASE "R"
			THIS_FLAG% = THIS_FLAG% OR 1%

		CASE ELSE
			THIS_FLAG% = THIS_FLAG% OR 4%

		END SELECT
	NEXT

4990	RETURN

8000	!*******************************************************************
	! Input line (Skip over titles)
	!
	! Return line in TXT$
	!*******************************************************************

8010	WHEN ERROR IN
		INPUT LINE #TEXT.CH%, TXT$
	USE
		EOFFLAG% = -1%
		CONTINUE 8090
	END WHEN

	IF INSTR(1%, TXT$, '12'C)
	THEN
		TXT$ = "X"

		WHILE TXT$ <> ""
			LINPUT #TEXT.CH%, TXT$
		NEXT
	END IF

8020	TXT$ = EDIT$(TXT$, 4% + 128%)

	GOTO 8010 IF TXT$ = ""

8090	RETURN

32767	END
