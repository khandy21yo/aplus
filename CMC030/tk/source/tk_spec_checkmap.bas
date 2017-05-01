1	%TITLE "Check Maps for Multi use"
	%SBTTL "TK_SPEC_CHECKMAP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988, 1991 BY
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
	!	This program checks another program to
	!	make sure the maps are declared only once
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CHECKMAP/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_CHECKMAP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CHECKMAP.OBJ;*
	!
	! Author:
	!
	!	07/13/91 - JEFF C. BEARD
	!
	! Modification History:
	!
	!	07/16/91 - Kevin Handy
	!		Unwound external definitions.
	!
	!	07/25/91 - Kevin Handy
	!		Modified to open source file ACCESS READ.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	10/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	DECLARE LONG SYS_STATUS
	DIM STRING NAME_MAP(200)

	MAP (Z) NAME.BUFFER$ = 255%

	%PAGE

	ON ERROR GOTO 19000

	FINAM.CH% = 10%

100	PRINT

	!
	! Get wildcard directory
	!
	LINPUT "Wildcard name to check (return to exit) <.BAS>"; FILE.NAME$

	GOTO ExitProgram IF EDIT$(FILE.NAME$, -1%) = ""

	CONTEXT% = 0%

	JUNK$ = "*.BAS"

200	!
	! Look up one file
	!
	SYS_STATUS = LIB$FIND_FILE(FILE.NAME$, NAME.BUFFER$, CONTEXT%, JUNK$)

	JUNK$ = ""

	GOTO 100 IF (SYS_STATUS AND 1%) = 0%

	FILENAME$ = TRM$(NAME.BUFFER$)

	GOSUB 4000

	!
	! Set up for next file
	!
	GOTO 200

4000	!*******************************************************************
	! Scan one file
	!*******************************************************************

	!
	! NUMBER IN THE ARRAY
	!
	LAST_X% = 0%

	WHEN ERROR IN
		OPEN FILENAME$ FOR INPUT AS FILE FINAM.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CALL ENTR_3MESSAGE(SCOPE, FILENAME$ + "  " + ERT$(ERR), 0%)
		CONTINUE 10000
	END WHEN

4100	WHEN ERROR IN
		LINPUT #FINAM.CH%,TEXT$
	USE
		IF LAST_X% = 0%
		THEN
			CONTINUE 10000 IF ERR = 11%
		ELSE
			CONTINUE 5000 IF ERR = 11%
		END IF

		CONTINUE HelpError
	END WHEN


	!
	! 4	CARRAGE RETURNS, LINE FEEDS, FORM FEEDS, DELETES, ESCAPES, NULLS
	! 8     LEADING SPACES AND TABS
	! 16    MULTIPLE SPACES AND TABS TO A SINGLE SPACE
	! 32    LOWERCASE TO UPPERCASE
	! 128   TRAILING SPACES AND TABS
	!
	TEXT$ = EDIT$(TEXT$, 4% + 8% + 16% + 32% + 128%)

	IF INSTR(1%, TEXT$, "MAP")
	THEN
		V1% = INSTR(1%, TEXT$, "(")
		V2% = INSTR(V1%,TEXT$, ")")
		NAME_MAP(LAST_X%) = SEG$(TEXT$, V1%, V2%)
		LAST_X% = LAST_X% + 1%
	END IF

	GOTO 4100

5000	!
	! SEARCH ARRAY FOR MULTI MAPS AND
	! PRINTS THE NAMES OUT ON THE TERMINAL
	!
	PRINT_STRING$ =  LF + STRING$(79, A"*"B) + LF + CR + &
		FILENAME$ + LF + CR + &
		STRING$(LEN(FILENAME$), A"-"B) + LF + CR
	PRINT_MESS$ = "Map used twice: "

 Print_Loop_J:
	FOR J% = 0% TO LAST_X%
		MAPNAM$ = NAME_MAP(J%)
 Print_Loop_I:
		FOR I% = 0% TO LAST_X%
			ITERATE Print_Loop_I IF I% = J%
			IF NAME_MAP(I%) = MAPNAM$ AND MAPNAM$ <> ""
			THEN
				PRINT_STRING$ = PRINT_STRING$ + &
					PRINT_MESS$ + NAME_MAP(I%) + LF + CR
				PRINT PRINT_STRING$
				PRINT_STRING$ = ""

				!
				! THIS WILL TAKE THE NAMED MAP OUT OF THE ARRAY
				!
				NAME_MAP(K%) = NAME_MAP(K% + 1%) &
					FOR K% = I% TO LAST_X% - 1%
				NAME_MAP(LAST_X%) = ""
				LAST_X% = LAST_X% - 1%

				EXIT Print_Loop_I
			END IF
		NEXT I%
	NEXT J%
10000
	CLOSE #FINAM.CH%
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

 ExitProgram:

	END
