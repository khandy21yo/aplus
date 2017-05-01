1	%TITLE "Set Relation Among Models"
	%SBTTL "TK_SPEC_SETRELATION"
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
	!	This program is used to set module structure
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_SETRELATION.BAS/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_SETRELATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_SETRELATION.OBJ;*
	!
	! Author:
	!
	!	01/18/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/30/89 - Robert Peterson
	!		Modify for more than one language.
	!		Change counter routine so more than one line
	!		of numbers can be counted.
	!		Change DEFIN$ to be:
	!			0 - Variable
	!			1 - Subroutine
	!			2 - Function
	!			3 - Record Structure
	!			4 - Record Map
	!			5 - Record Field
	!			6 - Parameter
	!			7 - Constant
	!			8 - Common Structure
	!			9 - Common Field
	!			I - Include
	!		Added capability to read source for include statements
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	03/13/92 - Kevin Handy
	!		Unrolled error trap (check)
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
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.HB"
	MAP (TK_RELATION)	TK_RELATION_CDD	TK_RELATION

	!
	! Dimension
	!
	DIM CROSS_REF$(1000%), &
		SOURCE$(1000%)

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(CROSS_REF.CH%, STAT%)
	CALL ASSG_CHANNEL(SOURCE.CH%, STAT%)

300	!
	! Open main file (existing) for modification
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.CRE"

	%PAGE

1000	!*******************************************************************
	! In this section the cross reference list will be read
	!*******************************************************************

	CALL FIND_FILE("*.CRO", CROSS_REF$(), 16%, "", "")

	CROSS_CNT% = VAL%(CROSS_REF$(0%))

	GOTO 2000 IF CROSS_CNT% <= 0%

	!
	! Loop for all cross reference files
	!
	FOR CROSS_LOOP% = 1% TO CROSS_CNT%

1100		!
		! Erase User Identifier from relationship
		!
		WHEN ERROR IN
			FIND #TK_RELATION.CH%, KEY #0% EQ CROSS_REF$(CROSS_LOOP%)
		USE
			CONTINUE 1120 IF ERR = 155%
			EXIT HANDLER
		END WHEN

1110		WHEN ERROR IN
			GET #TK_RELATION.CH%
		USE
			CONTINUE 1120 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		IF EDIT$(TK_RELATION::PARENT, 128%) = CROSS_REF$(CROSS_LOOP%)
		THEN
			DELETE #TK_RELATION.CH% IF TK_RELATION::DEFREF <> "I"

			GOTO 1110
		END IF

1120		!
		! Open cross reference listing
		!
		OPEN CROSS_REF$(CROSS_LOOP%) + ".CRO" FOR INPUT AS &
			FILE CROSS_REF.CH%, &
			ACCESS READ, ALLOW MODIFY

		!
		! Read one line from the cross reference listing
		!
 NextLine:
1130		WHEN ERROR IN
			LINPUT #CROSS_REF.CH%, TEXT$
		USE
			CONTINUE 1200 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		TEXT$ = EDIT$(TEXT$, 4% + 8% + 128%)

		!
		! Find the language type first
		!	1% - Vax basic
		!	2% - Vax c
		!
		IF LANG_TYPE% = 0%
		THEN
			IF INSTR(1%, TEXT$, "VAX BASIC")
			THEN
				LANG_TYPE% = 1%
			END IF

			IF INSTR(1%, TEXT$, "VAX C")
			THEN
				LANG_TYPE% = 2%
			END IF

			GOTO 1140
		END IF

		!
		! Are these the symbols that we want to track for basic?
		!
		IF LANG_TYPE% = 1% AND INSTR(1%, TEXT$, "Cross Reference")
		THEN
			IF INSTR(1%, TEXT$, "User Identifier")
			THEN
				USER_ID% = -1%
			ELSE
				USER_ID% = 0%
			END IF
		END IF

		!
		! Check lang_type
		!
		SELECT LANG_TYPE%

		!
		! Vax Basic Language
		!
		CASE 1%
			IF USER_ID%
			THEN
				DATA_TYPE$ = MID(TEXT$, 43%, 11%)
				NAME_TYPE$ = MID(TEXT$, 54%, 13%)

				DEFIN$ = ""

				SELECT NAME_TYPE$

				CASE "Ext. SUB"
					DEFIN$ = "1"

				CASE "Ext. FUNCTION"
					DEFIN$ = "2"

				CASE "RECORD"
					DEFIN$ = "3"

				CASE "MAP"
					!DEFIN$ = '4'

				CASE "Rec Field"
					!DEFIN$ = '5'

				CASE "Parm."
					!DEFIN$ = '6'

				CASE "CONSTANT"
					!DEFIN$ = '7'

				CASE "COMMON"
					!DEFIN$ = '8'

				CASE "COM"
					!DEFIN$ = '9'

 !				CASE ELSE
 !					SELECT DATA_TYPE$
 !
 !					CASE "STRING", "GFLOAT", "RFA", &
 !						"LONG", "WORD"
 !						DEFIN$ = '0'
 !
 !					END SELECT
				END SELECT

				IF DEFIN$ <> ""
				THEN
					GOSUB 18000
				END IF
			END IF
		END SELECT

1140		GOTO NextLine

1200		CLOSE CROSS_REF.CH%
	NEXT CROSS_LOOP%

2000	!*******************************************************************
	! In this section the source code will be read
	!*******************************************************************

	CALL FIND_FILE("*.BAS_TEMP", SOURCE$(), 16%, "", "")

	SOURCE_CNT% = VAL%(SOURCE$(0%))

	GOTO ExitProgram IF SOURCE_CNT% <= 0%

	!
	! Loop for all source files
	!
	FOR SOURCE_LOOP% = 1% TO SOURCE_CNT%

2100		!
		! Erase User Identifier from relationship
		!
		WHEN ERROR IN
			FIND #TK_RELATION.CH%, KEY #0% EQ SOURCE$(SOURCE_LOOP%)
		USE
			CONTINUE 2120 IF ERR = 155%
			EXIT HANDLER
		END WHEN

2110		WHEN ERROR IN
			GET #TK_RELATION.CH%
		USE
			CONTINUE 2120 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		IF EDIT$(TK_RELATION::PARENT, 128%) = SOURCE$(SOURCE_LOOP%)
		THEN
			DELETE #TK_RELATION.CH% IF TK_RELATION::DEFREF = "I"

			GOTO 2110
		END IF

2120		!
		! Open source listing
		!
		OPEN SOURCE$(SOURCE_LOOP%) + ".BAS_TEMP" FOR INPUT &
			AS FILE SOURCE.CH%, &
			ACCESS READ, ALLOW MODIFY

		LANG_TYPE% = 1%

		!
		! Read one line from the source listing
		!
2130		WHEN ERROR IN
			LINPUT #SOURCE.CH%, TEXT$
		USE
			CONTINUE 2200 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		TEXT$ = EDIT$(TEXT$, 4% + 8% + 128%)

		!
		! Check lang_type
		!
		SELECT LANG_TYPE%

		!
		! Vax Basic Language
		!
		CASE 1%
			GOTO 2130 IF LEFT(TEXT$, 8%) <> "%INCLUDE"
			GOTO 2130 IF INSTR(1%, TEXT$, "%CDD")

			!
			! Look for quotes
			!
			TEST1%, TEST2% = 0%
			TEST1% = INSTR(1%, TEXT$, "'")
			TEST2% = INSTR(1%, TEXT$, '"')

			GOTO 2130 IF TEST1% = 0% AND TEST2% = 0%

			!
			! See which quote is first
			!
			TEST% = TEST1%
			TEST% = TEST2% IF TEST2% < TEST1% AND &
				TEST2% <> 0% OR TEST% = 0%

			!
			! Pull out only quoted text
			!
			TEXT$ = RIGHT(TEXT$, TEST% + 1%)
			TEXT$ = LEFT(TEXT$, LEN(TEXT$) -1%)

			!
			! Search for : and ]
			!
			TEST1%, TEST2% = 0%
			TEST1% = INSTR(1%, TEXT$, "]")
			TEST2% = INSTR(1%, TEXT$, ":")

			!
			! See which is first
			!
			TEST% = TEST1%
			TEST% = TEST2% IF TEST2% > TEST1% AND &
				TEST1% <> 0% OR TEST% = 0%

			!
			! Now we have the directory and the include file
			!
			TEXT$ = RIGHT(TEXT$, TEST% + 1%)

			!
			! This section is here to compensate for the
			! fact that the language is not used as the
			! extension on the include file.  What this
			! section will do, is if there is a '.' in
			! the name and the extension is not 'bas'
			! then the '.' will be change to an '_' and
			! the extension will be treated as part of
			! the include name.
			!
			TEST% = INSTR(1%, TEXT$, ".")
			IF TEST%
			THEN
				TEXT$ = LEFT(TEXT$, TEST% - 1%) + "_" + &
					RIGHT(TEXT$, TEST% + 1%)
			END IF

			QTY% = 1%
			DEFIN$ = "I"
			TK_RELATION::PARENT = SOURCE$(SOURCE_LOOP%)
			TK_RELATION::CHILD = TEXT$

			GOSUB TryFind

		END SELECT

		GOTO 2130

2200		CLOSE SOURCE.CH%
	NEXT SOURCE_LOOP%

	CLOSE TK_RELATION.CH%

	GOTO ExitProgram

	%PAGE

18000	!
	! Prepare information for update
	!
	TK_RELATION::PARENT	= CROSS_REF$(CROSS_LOOP%)
	TK_RELATION::CHILD	= LEFT(TEXT$, 39%)

	GOTO Ret18000 IF TK_RELATION::PARENT = TK_RELATION::CHILD

	QTY% = 0%

	WHILE TEXT$	<>""

		!
		! Read cross reference lines
		!
		WHEN ERROR IN
			LINPUT #CROSS_REF.CH%, TEXT$
		USE
			CONTINUE TryFind IF ERR = 11%
			CONTINUE AddRec IF ERR = 155%
			EXIT HANDLER
		END WHEN

		POUND% = 0%
		POSIT% = INSTR(1%, TEXT$, "#")
		WHILE POSIT%
			POUND% = POUND% + 1%
			POSIT% = INSTR(POSIT% + 1%, TEXT$, "#")
		NEXT

		SELECT LANG_TYPE%

		CASE 1%
			TEXT$ = XLATE(TEXT$, STRING$(46%, 32%) + &
				". 01234567890")

		END SELECT

		TEXT$ = EDIT$(TEXT$, 8% + 16%) + " "

		WHILE INSTR(1%, TEXT$, " ")
			SP% = INSTR(1%, TEXT$, " ")
			TEXT$ = EDIT$(RIGHT(TEXT$, SP% + 1%), 8%)
			QTY% = QTY% + 1%
		NEXT

	NEXT

	QTY% = QTY% - POUND%

	SELECT POUND%

	CASE 0%
		DEFIN$ = "0"
	CASE 1%
		DEFIN$ = "1"
	CASE ELSE
		DEFIN$ = "M"
	END SELECT

 TryFind:
	FIND #TK_RELATION.CH%, &
		KEY #0% EQ TK_RELATION::PARENT + TK_RELATION::CHILD

	GET #TK_RELATION.CH%

	TK_RELATION::QUANTITY	= QTY%
	TK_RELATION::DEFREF	= DEFIN$
	TK_RELATION::CDATE	= DATE_TODAY
	TK_RELATION::CTIME	= TIME_NOW

	UPDATE #TK_RELATION.CH%
	GOTO Ret18000

 AddRec:
	TK_RELATION::QUANTITY	= QTY%
	TK_RELATION::DEFREF	= DEFIN$
	TK_RELATION::CDATE	= DATE_TODAY
	TK_RELATION::CTIME	= TIME_NOW

	PUT #TK_RELATION.CH%

 Ret18000:
	RETURN

 ExitProgram:

32767	END
