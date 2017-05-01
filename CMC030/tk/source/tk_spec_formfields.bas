1	%TITLE "Extract the form fields from the Source Code"
	%SBTTL "TK_SPEC_FORMFIELDS"
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
	!	Create documentation from source code. This will extract
	!	the field names in a form that can be used.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_FORMFIELDS/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_FORMFIELDS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_FORMFIELDS.OBJ;*
	!
	! Author:
	!
	!	05/15/91 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
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
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD LOC_DDL

	!
	! External Functions
	!
	EXTERNAL LONG    FUNCTION LIBR_3INSERT

	!
	! Dimension variables
	!
	DIM	FILE_NAME$(1000%), &
		DIR_NAME$(100%), &
		LINE1(140%), &
		LINE2(140%)

	TABLE$ = STRING$(57%, 0%)

	TABLE$ = TABLE$ + CHR$(T%) FOR T% = 57% TO 96%

	!
	! Handle input/output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	ON ERROR GOTO 19000

	W_FILE_NAME$ = READ_SYSJOB + ".TMP"

	!
	! Get info from the user
	!
	PRINT "	This program will take the fields from the source code"
	PRINT "	and create a help file, so people know what they can use "
	INPUT "Directory:   "; DIRECT$
	INPUT "Module Name: "; MODULE$

	CALL FIND_FILE("SOURCE:[000000]*.DIR", DIR_NAME$(), 16%, "", "")
	DIR.LOOP% = VAL%(DIR_NAME$(0%))

	CHECK_FILE_NAME$ = ""

500	FOR J% = 1% TO DIR.LOOP%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%

		IF LEN(DIR_NAME$(J%)) > 2%
		THEN
			LIB_NAME$  = "REF:HELP_DEFAULT.TLB"
		ELSE
			LIB_NAME$  = "REF:HELP_" + DIR_NAME$(J%) + ".TLB"
		END IF

		CALL FIND_FILE("SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]" + &
			TRM$(MODULE$) + ".BAS", FILE_NAME$(), 16%, "", "")

		I_LOOP% = VAL%(FILE_NAME$(0%))

		FOR I% = 1% TO I_LOOP%

			!
			! Program type
			!
			POS1% = INSTR(1%, FILE_NAME$(I%), "_") + 1%
			POS2% = INSTR(POS1%, FILE_NAME$(I%), "_") - 1%
			PROG_TYPE$ = SEG$(FILE_NAME$(I%), POS1%, POS2%)

			GOTO NextI IF PROG_TYPE$ <> "FORM" AND &
				PROG_TYPE$ <> "OUTP"

505			! Open file to read from
			WHEN ERROR IN
				OPEN "SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]" + &
					FILE_NAME$(I%) + ".BAS" &
					FOR INPUT AS FILE READ_FILE.CH%, &
					RECORDSIZE 132%, &
					ACCESS READ, &
					ALLOW MODIFY
			USE
				PRINT "Unable to open "; FILE_NAME$(I%)
				CONTINUE 540
			END WHEN

			! Open the new file we are going to create
			WHEN ERROR IN
				OPEN W_FILE_NAME$ FOR OUTPUT AS FILE WRIT_FILE.CH%, &
					RECORDSIZE 132%
			USE
				PRINT "Unable to open "; W_FILE_NAME$
				CONTINUE 540
			END WHEN

			PRINT "Extacting from: " + &
				"SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]" + &
				FILE_NAME$(I%) + ".BAS"

			REPORT$ = ""

510			!
			! Get the file Title in the first line
			!
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

				PRINT #WRIT_FILE.CH%, "^*" + TITLE$ + "\*"
				PRINT TITLE$

			END IF

520			WHILE 1%

				WHEN ERROR IN
					LINPUT #READ_FILE.CH%, TEXT$
				USE
					CONTINUE 540 IF ERR = 11%
					FILENAME$ = FILE_NAME$
					CONTINUE HelpError
				END WHEN

				!
				! We want to know the report key
				!
				IF (INSTR(1%, EDIT$(TEXT$, -1%), &
					"REPORT$=" + '"')) <> 0%
				THEN
					RPOS1% = INSTR(1%, TEXT$, '"') + 1%
					RPOS2% = INSTR(RPOS1%, TEXT$, '"') - 1%
					REPORT$ = SEG$(TEXT$, RPOS1%, RPOS2%)

					! Now we know the report key we
					! can create the key for the library

					KEY_NAME$ = "H$" + DIR_NAME$(J%) + &
						"_FORM$" + REPORT$

				END IF

				!
				! Skip program part until we get to what we
				! want to look at
				!
				GOTO 530 IF (INSTR(1%, EDIT$(TEXT$, -1%), &
					"SELECTVARNAME$")) = 0%

525				WHEN ERROR IN
					LINPUT #READ_FILE.CH%, TEXT$
				USE
					CONTINUE 540 IF ERR = 11%
					FILENAME$ = FILE_NAME$
					CONTINUE HelpError
				END WHEN

				GOTO 525 IF INSTR(1%, EDIT$(TEXT$, -1%), &
					"CASE") = 0% OR INSTR(1%, EDIT$( &
					TEXT$, -1%), ".ID") <> 0%
				!
				! Extract the variable
				!
				FPOS1% = INSTR(1%, TEXT$, '"')
				FPOS2% = INSTR(FPOS1% + 1%, TEXT$, '"') - 2%
				FIELD1$ = SEG$(EDIT$(TEXT$, 16%), &
					FPOS1%, FPOS2%)

				!
				! Sometimes there is a second variable
				!
				FIELD2$ = ""
				FIELD3$ = ""
				FPOS3% = INSTR(FPOS2% + 3%, TEXT$, '"')
				IF FPOS3% <> 0%
				THEN
					FPOS4% = INSTR(FPOS3% + 1%, &
						TEXT$, '"') - 2%
					FIELD2$ = SEG$(EDIT$( &
						TEXT$, 16%), FPOS3%, FPOS4%)

					!
					! Sometimes there is even a third variable
					!
					FPOS5% = INSTR(FPOS4% + 3%, TEXT$, '"')
					IF FPOS5% <> 0%
					THEN
						FPOS6% = INSTR(FPOS5% + 1%, &
							TEXT$, '"') - 2%
						FIELD3$ = SEG$(EDIT$( &
							TEXT$, 16%), FPOS5%, FPOS6%)

					END IF
				END IF

				! Get ready to take out the File Name
				NPOS1% = INSTR(1%, TEXT$, '"')
				IF INSTR(FPOS1% + 1%, TEXT$, ":") > 0%
				THEN
					NPOS2% = INSTR(FPOS1% + 1%, &
						TEXT$, ":") - 2%
					LOOK$ = "::"

					! Take out the Field Name
					NPOS3% = INSTR(FPOS2% + 1%, TEXT$, '"') - 2%
					FIELD_NAME$ = SEG$(EDIT$(TEXT$, 16%), &
						NPOS2% + 3%, NPOS3%)
				ELSE
					NPOS2% = INSTR(FPOS1% + 1%, &
						TEXT$, ".") - 2%
					LOOK$ = "."

					! Take out the Field Name
					NPOS3% = INSTR(FPOS2% + 1%, TEXT$, '"') - 2%
					FIELD_NAME$ = SEG$(EDIT$(TEXT$, 16%), &
						NPOS2% + 2%, NPOS3%)
				END IF
				! Take out the File Name
				FILE_NAME$ = SEG$(EDIT$(TEXT$, 16%), &
					NPOS1%, NPOS2%)


				IF (CHECK_FILE_NAME$ <> FILE_NAME$ OR &
					CHECK_FILE_NAME$ = "") AND INSTR ( &
					1%, TEXT$, LOOK$) <> 0%

				THEN

					! we've got a first timer or
					! a new file to get descriptions for
					CALL TK_SUBR_DDLEXTRACT(LOC_DDL, &
						LEFT$(FILE_NAME$, &
						INSTR(1%, FILE_NAME$, "_") - 1%), &
						FILE_NAME$, FDE_STATUS%)

					IF CHECK_FILE_NAME$ <> ""
					THEN
						! end the old list
						PRINT #WRIT_FILE.CH%, ".els"
						PRINT #WRIT_FILE.CH%, ".lm -10"
					END IF

					CHECK_FILE_NAME$ = FILE_NAME$

					! Set up a title for the file
					! and a list for the fields
					PRINT #WRIT_FILE.CH%, ".p"
					PRINT #WRIT_FILE.CH%, TRM$(LOC_DDL::DESCR)
					PRINT #WRIT_FILE.CH%, ".lm 10"
					PRINT #WRIT_FILE.CH%, ".b"
					PRINT #WRIT_FILE.CH%, ".list 0, " + &
						CHR$(34%) + "o" + CHR$(34)

				ELSE
					IF NONFIELD% = 0% AND INSTR ( &
						1%, TEXT$, LOOK$) = 0%
					THEN
						! We have non fielded values,
						! Set up a title and a list
						! for them
						PRINT #WRIT_FILE.CH%, ".els"
						PRINT #WRIT_FILE.CH%, ".lm -10"
						PRINT #WRIT_FILE.CH%, ".p"
						PRINT #WRIT_FILE.CH%, "Additional Form Fields"
						PRINT #WRIT_FILE.CH%, ".lm 10"
						PRINT #WRIT_FILE.CH%, ".b"
						PRINT #WRIT_FILE.CH%, ".list " + &
							"0, " + CHR$(34%) + &
							"o" + CHR$(34)
						NONFIELD% = -1%
					END IF

				END IF

				! If programer put a description in the program
				! about the variable we'll use that, otherwise
				! look in DDL array.

				IF INSTR(1%, TEXT$, "!") > 0%
				THEN
					DESCR$ = RIGHT$(TEXT$, &
						INSTR(1%, TEXT$, "!") + 1%)

				ELSE

				! Look through the DDL array for the description
				! that matches the field name.
				LOC% = 0%
				UNTIL LOC_DDL::FIELD_NAME(LOC%) = FIELD_NAME$ &
						OR LOC% = 255%
					LOC% = LOC% + 1%
					DESCR$ = LOC_DDL::FIELD_DESC(LOC%)

				NEXT

				! Couldn't find a description for the field,
				! the field could be an array, so strip off
				! every thing that isn't a letter, hopefully
				! we can find it now.
				IF EDIT$(DESCR$, -1%) = ""
				THEN
					FIELD_NAME$ = XLATE(FIELD_NAME$, &
						TABLE$)
					LOC% = 0%
					UNTIL LOC_DDL::FIELD_NAME(LOC%) = &
							FIELD_NAME$ &
							OR LOC% = 255%
						LOC% = LOC% + 1%
						DESCR$ = LOC_DDL::FIELD_DESC(LOC%)
					NEXT
				END IF

				END IF

				! Print out for log file the fields we found
				PRINT EDIT$(FIELD1$ + " " + FIELD2$ + " " + &
					FIELD3$ + " - " + DESCR$, &
					4% + 8% + 16% + 128%)

				! Put the line together
				FIELD1$ = "^*" + FIELD1$ + "\*" IF FIELD1$ <> ""
				FIELD2$ = " or ^*" + FIELD2$ + "\*" IF FIELD2$ <> ""
				FIELD3$ = " or ^*" + FIELD3$ + "\*" IF FIELD3$ <> ""

				FIELD$ = EDIT$(FIELD1$ + FIELD2$ + FIELD3$ + &
					" - " + DESCR$, 4% + 8% + 16% + 128%)

				!
				! Look for special charaters that need
				! special attention for documentation.
				!
				LPOS% = 1%
				CHANGE FIELD$ TO LINE1

				FOR K% = 1 TO LINE1(0%)

					! Insert Special charter before
					! original charter
					IF LINE1(K%) = A"_"B OR &
						LINE1(K%) = A"&"B OR &
						LINE1(K%) = A"#"B

					THEN
						LINE2(LPOS%) = A"_"B
						LPOS% = LPOS% + 1%
					END IF

					! Put Original Charater back
					LINE2(LPOS%) = LINE1(K%)
					LPOS% = LPOS% + 1%

				NEXT K%

				LINE2(0%) = LPOS% - 1%
				CHANGE LINE2 TO DOC_FLEID$

				PRINT #WRIT_FILE.CH%, ".le"
				PRINT #WRIT_FILE.CH%, DOC_FLEID$

				GOTO 525
530			NEXT

540			PRINT #WRIT_FILE.CH%, ".els"
			PRINT #WRIT_FILE.CH%, ".lm -10"

			CLOSE #WRIT_FILE.CH%
			CLOSE #READ_FILE.CH%

			GOSUB PutLib

 NextI:		NEXT I%

 NextJ:	NEXT J%


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

 PutLib:
	! Place in library
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

 EndKill:
	! Reopen the new file we are going to create
	OPEN W_FILE_NAME$ FOR OUTPUT AS FILE WRIT_FILE.CH%, &
		RECORDSIZE 132%

	RETURN

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	PRINT ERN$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
		"ERR", PROGRAM$, "ERROR" + NUM1$(ERR)

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME HelpError

 EndProgram:
32767	END
