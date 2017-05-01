1	%TITLE "Sub Process to Compile CDD File"
	%SBTTL "TK_SUBR_DDLCOMPILE"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_DDLCOMPILE(SMG_DDL_CDD DDL, &
		STRING SYSTEM_NAME, STRING FILE_NAME)

	!
	! COPYRIGHT (C) 1987 BY
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
	!
	! ABSTRACT:HELP
	!	.p
	!	A process to aid in the creation of a ddl file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_DDLCOMPILE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_DDLCOMPILE
	!	$ DELETE TK_SUBR_DDLCOMPILE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	09/12/90 - Kevin Handy
	!		Modified to output a .H file for C programs.
	!
	!	11/16/90 - Kevin Handy
	!		Modified so that I could get an error message
	!		better than "Untrapped error".
	!
	!	03/06/92 - Kevin Handy
	!		Repaired some goofiness in ".h" comments.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/06/95 - Kevin Handy
	!		Added output of a ".hb" file.
	!
	!	12/08/95 - Kevin Handy
	!		Trim field names on output to .h or .hb files.
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	07/24/98 - Kevin Handy
	!		Output .HB arrays as '()' instead of '[]'.
	!
	!	07/24/98 - Kevin Handy
	!		Fix bug generating array information to HB files.
	!		Once an array was defined, everything was an array.
	!
	!	07/24/98 - Kevin Handy
	!		Output 'WORD' instead of 'SHORT' to HB file.
	!
	!	07/25/98 - Kevin Handy
	!		Fixed major bug with arrays with more than one
	!		dimension
	!
	!	08/20/98 - Kevin Handy
	!		Modified to keep DLL files as individual files
	!		instead of relying on CDD to remember them.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Lower case the file name used as the structure
	!		name in C '.h' files.
	!		Added date created to output file.
	!
	!	03/11/99 - Kevin Handy
	!		Fix array subscripts in C pograms to increment
	!		it by one [C uses # elements, not last element]
	!
	!	03/30/99 - Kevin Handy
	!		Try to lose more extra spaces in text fields
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	04/14/99 - Kevin Handy
	!		Fixes for BROADCAST trapping errors
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose need for CDD so I don't have to re-install it.
	!		Other code cleanup.
	!
	!	05/09/2001 - Kevin Handy
	!		Added "pragma nomember_alignment" to generated
	!		header files so that they will be compatible with
	!		Alpha/Basic structures.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"

	!
	! Declare vars
	!
	DECLARE LONG TEXT_FILE.CH
 !	EXTERNAL LONG READ_3BROADCAST

	!
	! Convert to lower case
	!
	DEF FNLOWER$(X$)

		JUNK$ = TRM$(X$)

		FOR JUNK% = 1% TO LEN(JUNK$)

			JUNK1$ = MID(JUNK$, JUNK%, 1%)

			IF (JUNK1$ >= "A") AND (JUNK1$ <= "Z")
			THEN
				JUNK$ = LEFT(JUNK$, JUNK% - 1%) + &
					CHR$(ASCII(JUNK1$) + 32%) + &
					RIGHT(JUNK$, JUNK% + 1%)
			END IF

		NEXT JUNK%

		FNLOWER$ = JUNK$

	END DEF

	!
	! Convert CDD array into C array format
	!
	DEF FNCARRAY$(A$)

		Q9$ = ""
		Q1$ = EDIT$(A$, 8% + 16% + 128%) + " "
		Q1% = INSTR(1%, Q1$, " ")
		WHILE Q1% <> 0%
			Q2% = INSTR(1%, Q1$, ":")
			Q9$ = Q9$ + "[" + &
				NUM1$(VAL%(SEG$(Q1$, Q2% + 1%, Q1% - 1%)) &
				+ 1%) + "]"
			Q1$ = RIGHT(Q1$, Q1% + 1%)
			Q1% = INSTR(1%, Q1$, " ")
		NEXT

		FNCARRAY$ = Q9$
	END DEF

	!
	! Convert CDD array format into BASIC array format
	!
	DEF FNBARRAY$(A$)

		Q9$ = ""
		Q1$ = EDIT$(A$, 8% + 16% + 128%) + " "
		Q1% = INSTR(1%, Q1$, " ")
		WHILE Q1% <> 0%
			Q2% = INSTR(1%, Q1$, ":")
			Q9$ = Q9$ + ", " + SEG$(Q1$, Q2% + 1%, Q1% - 1%)
			Q1$ = RIGHT(Q1$, Q1% + 1%)
			Q1% = INSTR(1%, Q1$, " ")
		NEXT

		FNBARRAY$ = "(" + RIGHT(Q9$, 3%) + ")"
	END DEF

	!
	! Other assignments
	!
 !	JJ$ = READ_SYSJOB

100	!
	! Get the channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(TEXT_FILE.CH)
	GOTO ErrorGetCh IF SMG_STATUS% = LIB$_INSLUN

300	!
	! Create a ".h" file for C programs to use, instead of the
	! CDD files as previously required.  This should give us better
	! portability.
	!
	CALL ENTR_3MESSAGE(SCOPE, "Compiling " + TRM$(FILE_NAME), 1%)

	DDL_FILEC$ = "SOURCE:[" + TRM$(SYSTEM_NAME) + ".OPEN]" + &
		TRM$(FILE_NAME) + ".H"

	OPEN DDL_FILEC$ FOR OUTPUT AS FILE #TEXT_FILE.CH, RECORDSIZE 132%


310	PRINT #TEXT_FILE.CH, "/*"
	PRINT #TEXT_FILE.CH, " * File Layout for: "; &
		TRM$(SYSTEM_NAME); "."; TRM$(FILE_NAME); " on "; DATE$(0%)
	PRINT #TEXT_FILE.CH, " *"
	PRINT #TEXT_FILE.CH, " * "; TRM$(DDL::DESCR)
	PRINT #TEXT_FILE.CH, " */"
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, "#pragma member_alignment save"
	PRINT #TEXT_FILE.CH, "#pragma nomember_alignment"
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, "struct "; FNLOWER$(TRM$(FILE_NAME)); "_cdd"
	PRINT #TEXT_FILE.CH, "{"

	FOR LOOP% = 1% TO DDL::FIELD_NUM
		PRINT #TEXT_FILE.CH, TRM$("/* Element = " + &
			DDL::FIELD_ELEMENT(LOOP%))
		PRINT #TEXT_FILE.CH, TRM$("   Description = " + &
			DDL::FIELD_DESC(LOOP%)); &
			" */"

		SELECT TRM$(DDL::FIELD_TYPE(LOOP%))

		CASE "TEXT"
			PRINT #TEXT_FILE.CH, "	char "; &
				FNLOWER$(TRM$(DDL::FIELD_NAME(LOOP%)));

		CASE "G_FLOATING"
			PRINT #TEXT_FILE.CH, "	double "; &
				FNLOWER$(TRM$(DDL::FIELD_NAME(LOOP%)));

		CASE "SIGNED LONGWORD"
			PRINT #TEXT_FILE.CH, "	long "; &
				FNLOWER$(TRM$(DDL::FIELD_NAME(LOOP%)));

		CASE "SIGNED WORD"
			PRINT #TEXT_FILE.CH, "	int "; &
				FNLOWER$(TRM$(DDL::FIELD_NAME(LOOP%)));

		CASE ELSE
			PRINT #TEXT_FILE.CH, "	"; &
				FNLOWER$(TRM$(DDL::FIELD_TYPE(LOOP%))); " "; &
				FNLOWER$(TRM$(DDL::FIELD_NAME(LOOP%)));

		END SELECT

		IF DDL::FIELD_ATTRIBUTE(LOOP%) <> ""
		THEN
			IF LEFT(DDL::FIELD_ATTRIBUTE(LOOP%), 8%) = "ARRAY 0:"
			THEN
				PRINT #TEXT_FILE.CH, &
					FNCARRAY$(RIGHT(DDL::FIELD_ATTRIBUTE(LOOP%), 7%));
			ELSE
				PRINT #TEXT_FILE.CH, &
					TRM$(DDL::FIELD_ATTRIBUTE(LOOP%));
			END IF
		END IF

		SELECT TRM$(DDL::FIELD_TYPE(LOOP%))

		CASE "TEXT"
			PRINT #TEXT_FILE.CH, &
				"["; EDIT$(DDL::FIELD_SIZE(LOOP%), 2%); "]";
		END SELECT

		PRINT #TEXT_FILE.CH, ";"
	NEXT LOOP%

	PRINT #TEXT_FILE.CH, "};"
	PRINT #TEXT_FILE.CH, "#pragma member_alignment restore"

	CLOSE TEXT_FILE.CH

400	!
	! Create a ".hb" file for Basic programs to use, instead of the
	! CDD files as previously required.  This should give us better
	! portability.
	!
	CALL ENTR_3MESSAGE(SCOPE, "Compiling " + TRM$(FILE_NAME), 1%)

	DDL_FILEB$ = "SOURCE:[" + TRM$(SYSTEM_NAME) + ".OPEN]" + &
		TRM$(FILE_NAME) + ".HB"

	OPEN DDL_FILEB$ FOR OUTPUT AS FILE #TEXT_FILE.CH, RECORDSIZE 132%


410	PRINT #TEXT_FILE.CH, "	!"
	PRINT #TEXT_FILE.CH, "	! File Layout for: "; &
		TRM$(SYSTEM_NAME); "."; TRM$(FILE_NAME); " on "; DATE$(0%)
	PRINT #TEXT_FILE.CH, "	!"
	PRINT #TEXT_FILE.CH, "	! "; TRM$(DDL::DESCR)
	PRINT #TEXT_FILE.CH, "	!"
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, "	RECORD "; TRM$(FILE_NAME); "_CDD"

	FOR LOOP% = 1% TO DDL::FIELD_NUM
		PRINT #TEXT_FILE.CH, TRM$("		! Element = " + &
			DDL::FIELD_ELEMENT(LOOP%))
		PRINT #TEXT_FILE.CH, TRM$("		!   Description = " + &
			DDL::FIELD_DESC(LOOP%))

		IF DDL::FIELD_ATTRIBUTE(LOOP%) <> ""
		THEN
			IF LEFT(DDL::FIELD_ATTRIBUTE(LOOP%), 8%) = "ARRAY 0:"
			THEN
				ARRAY$ = FNBARRAY$(RIGHT(DDL::FIELD_ATTRIBUTE(LOOP%), 7%))
			ELSE
				ARRAY$ = ""
			END IF
		ELSE
			ARRAY$ = ""
		END IF

		SELECT TRM$(DDL::FIELD_TYPE(LOOP%))

		CASE "TEXT"
			PRINT #TEXT_FILE.CH, "		STRING "; &
				TRM$(DDL::FIELD_NAME(LOOP%)); ARRAY$; &
				" = "; TRM$(DDL::FIELD_SIZE(LOOP%));

		CASE "G_FLOATING"
			PRINT #TEXT_FILE.CH, "		GFLOAT "; &
				TRM$(DDL::FIELD_NAME(LOOP%)); ARRAY$;

		CASE "SIGNED LONGWORD"
			PRINT #TEXT_FILE.CH, "		LONG "; &
				TRM$(DDL::FIELD_NAME(LOOP%)); ARRAY$;

		CASE "SIGNED WORD"
			PRINT #TEXT_FILE.CH, "		WORD "; &
				TRM$(DDL::FIELD_NAME(LOOP%)); ARRAY$;

		CASE ELSE
			PRINT #TEXT_FILE.CH, "		"; &
				TRM$(DDL::FIELD_TYPE(LOOP%)); " "; &
				TRM$(DDL::FIELD_NAME(LOOP%)); ARRAY$;

		END SELECT

		PRINT #TEXT_FILE.CH
	NEXT LOOP%

	PRINT #TEXT_FILE.CH, "	END RECORD"

	CLOSE TEXT_FILE.CH

500	!
	! Get the ddl file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Compiling " + TRM$(FILE_NAME), 1%)

	DDL_FILE$ = "SOURCE:[" + TRM$(SYSTEM_NAME) + ".OPEN]" + &
		TRM$(FILE_NAME) + ".DDL"

 !	CALL ENTR_3MESSAGE(SCOPE, "Compiling " + TRM$(DDL_FILE$), 1%)

	WHEN ERROR IN
		OPEN DDL_FILE$ FOR OUTPUT AS FILE #TEXT_FILE.CH, RECORDSIZE 132%
	USE
		CONTINUE 600
	END WHEN

510	PRINT #TEXT_FILE.CH, "DEFINE RECORD CDD$TOP."; TRM$(SYSTEM_NAME); "."; &
		TRM$(FILE_NAME)
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, TAB(8%);"DESCRIPTION IS /*";TRM$(DDL::DESCR); "*/."
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, TAB(8%);TRM$(FILE_NAME); "_CDD STRUCTURE."

	FOR LOOP% = 1% TO DDL::FIELD_NUM
		PRINT #TEXT_FILE.CH
		PRINT #TEXT_FILE.CH, TAB(8%); TRM$("/* Element = " + &
			DDL::FIELD_ELEMENT(LOOP%))
		PRINT #TEXT_FILE.CH, TAB(8%); TRM$("Description = " + &
			DDL::FIELD_DESC(LOOP%)); &
			" */"
		PRINT #TEXT_FILE.CH, TAB(8%); &
			TRM$(DDL::FIELD_NAME(LOOP%)); TAB(32%);

		IF DDL::FIELD_ATTRIBUTE(LOOP%) <> ""
		THEN
			PRINT #TEXT_FILE.CH, &
				TRM$(DDL::FIELD_ATTRIBUTE(LOOP%)); " ";
		END IF

		PRINT #TEXT_FILE.CH, "DATATYPE IS "; &
			TRM$(DDL::FIELD_TYPE(LOOP%));

		IF DDL::FIELD_TYPE(LOOP%) = "TEXT"
		THEN
			PRINT #TEXT_FILE.CH, " SIZE IS "; &
				EDIT$(DDL::FIELD_SIZE(LOOP%), -1%);
		END IF

		PRINT #TEXT_FILE.CH, "."
	NEXT LOOP%

	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, TAB(8%); "END "; TRM$(FILE_NAME); "_CDD STRUCTURE."
	PRINT #TEXT_FILE.CH
	PRINT #TEXT_FILE.CH, "END "; TRM$(FILE_NAME); "."

	CLOSE TEXT_FILE.CH

 !	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)
 !
 !	SMG_STATUS% = LIB$SPAWN("CDDL/REP " + DDL_FILE$)
 !
 !	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, SMG$M_CURSOR_OFF)
 !
 !	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
 !		LOC(READ_3BROADCAST), LOC(SCOPE))
 !
 !	IF (SMG_STATUS% AND 1%) = 0%
 !	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
 !			" has occured", 0%)
 !		GOTO ExitProgram
 !	END IF
 !
 !	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

600	!
	!

 ExitProgram:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	CLOSE TEXT_FILE.CH

	SMG_STATUS% = LIB$FREE_LUN(TEXT_FILE.CH)

	IF (SMG_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Error " + NUM1$(SMG_STATUS%) + " has occured", 0%)
	END IF

	EXIT SUB

 ErrorGetCh:
	CALL ENTR_3MESSAGE(SCOPE, "No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
	GOTO ExitProgram

32767	END SUB
