1	%TITLE "Insert a Help File for a Particular System"
	%SBTTL "TK_SPEC_FILEHELP"
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
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_FILEHELP/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_FILEHELP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_FILEHELP.OBJ;*
	!
	! Author:
	!
	!	05/24/90 - Lance Williams
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP (TK_FILEDICT) TK_FILEDICT_CDD TK_FILEDICT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM) UTL_SYSTEM_CDD UTL_SYSTEM

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_OPN.HB"
	DECLARE  SMG_OPN_CDD OPN

	!
	! External Functions
	!
	EXTERNAL INTEGER FUNCTION COMP_STRING

	!
	! Dimension variables
	!
	DIM FILE_NAME$(1000%), &
		DIR_NAME$(100%)

	!
	! Handle input/output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	ON ERROR GOTO 19000

	BEG% = 0%

	!
	! Get info from the user
	!
	INPUT "Directory:   "; DIRECT$
	INPUT "Module Name: "; MODULE$

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.OPN"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.OPN"

500	!
	! Look up one file
	!
 Getrec:

	WHEN ERROR IN
		GET #TK_FILEDICT.CH%, REGARDLESS
	USE
		CONTINUE Exitprogram IF ERR=11%
		FILENAME$ = "TK_FILEDICT"
		CONTINUE HelpError
	END WHEN

	GOTO Getrec IF COMP_STRING(TK_FILEDICT::FILENAME, MODULE$) = 0%
	GOTO Getrec IF COMP_STRING(TK_FILEDICT::SYSTEM, DIRECT$) = 0%

	! Open file to read from
	SYSTEM$ = TK_FILEDICT::SYSTEM
	PREFIX$ = "SOURCE:[" + SYSTEM$ + ".OPEN]"
	FILE_NAME$ = TRM$(TK_FILEDICT::FILENAME)

505	GET #UTL_SYSTEM.CH%, KEY #0% EQ SYSTEM$, REGARDLESS

	!
	! Extract all information possible from open file
	!
	CALL TK_SUBR_EXTRACTOPEN(OPN, FILE_NAME$, "CRE", PREFIX$)

	DESCR$ = TRM$(TK_FILEDICT::DESCR)
	EXT$ = TRM$(OPN::EXTENSION)

510	!
	! Read original copy of the source code
	!
	NEW_FILE% = 0%

	WHEN ERROR IN
		OPEN PREFIX$ + FILE_NAME$ + ".HLP" FOR INPUT &
			AS FILE READ_FILE.CH%
	USE
		IF ERR = 5%
		THEN
			NEW_FLAG% = -1%
			CONTINUE 515
		END IF
		FILENAME$ = FILE_NAME$
		CONTINUE HelpError
	END WHEN

	!
	! Create new copy of the source code
	!
515	OPEN PREFIX$ + FILE_NAME$ +".TMP" &
		FOR OUTPUT AS FILE WRIT_FILE.CH%, &
		RECORDSIZE 132%

	IF NEW_FLAG% = -1%
	THEN
		PRINT #WRIT_FILE.CH%, "	!++"
		GOSUB Entry
		GOTO 550
	END IF

	FLAG% = 0%

	!
	! Read line by line and search for help message block
	!
	WHILE 1%

520		WHEN ERROR IN
			LINPUT #READ_FILE.CH%, TEXT$
		USE
			CONTINUE 550 IF ERR = 11%
			FILENAME$ = FILE_NAME$
			CONTINUE HelpError
		END WHEN

		IF EDIT$(TEXT$, -1%) = "!ERROR:5"
		THEN
			FLAG% = -1%
			GOSUB Entry
		ELSE
			PRINT #WRIT_FILE.CH%, &
				EDIT$(TEXT$, 4%) &
				IF FLAG% = 0%
		END IF

		FLAG% = 0% IF EDIT$(TEXT$, -1%) = "!--"

	NEXT

550	CLOSE WRIT_FILE.CH%
	CLOSE READ_FILE.CH%

	WHEN ERROR IN
		NAME PREFIX$ + FILE_NAME$ + ".TMP" AS &
			PREFIX$ + FILE_NAME$ + ".HLP"
		KILL PREFIX$ + FILE_NAME$ + ".HLP;-2"
	USE
		CONTINUE 560 IF ERR = 5%
		FILENAME$ = FILE_NAME$
		CONTINUE HelpError
	END WHEN

	PRINT "Error:5 in " + PREFIX$ + FILE_NAME$ + ".HLP"

560	GOTO Getrec

 ExitProgram:
18000	!*******************************************************************
	! Exit program
	!*******************************************************************
	GOTO EndProgram

 Entry:
	FILE_NAME1$ = FILE_NAME$

18050	BEG% = INSTR(BEG% + 2%, FILE_NAME1$, "_")
	GOTO 18100 IF BEG% = 0%
	FILE_NAME1$ = LEFT$(FILE_NAME1$, BEG%) + "_" + &
		RIGHT$(FILE_NAME1$, BEG% + 1%)
	GOTO 18050


18100	BEG% = 0%
	PRINT #WRIT_FILE.CH%, "	! Error:5"
	PRINT #WRIT_FILE.CH%, "	!	^*The " + DESCR$ + " is Missing\*"
	PRINT #WRIT_FILE.CH%, "	!	.p"
	PRINT #WRIT_FILE.CH%, "	!	^*Explanation:\*"
	PRINT #WRIT_FILE.CH%, "	!	.p"
	PRINT #WRIT_FILE.CH%, "	!	The ^*" + FILE_NAME1$ + "." + EXT$ + &
		"\* file has not been created."
	PRINT #WRIT_FILE.CH%, "	!	.p"
	PRINT #WRIT_FILE.CH%, "	!	^*User Action:\*"
	PRINT #WRIT_FILE.CH%, "	!	.p"

	SYSTEM_TYPE$ = TRM$(UTL_SYSTEM::DESCRIPTION)

	SELECT EXT$
	CASE "CTR"

		PRINT #WRIT_FILE.CH%, &
			"	!	Create the controling file in the ^*" + &
			SYSTEM_TYPE$ + "\* going"
		PRINT #WRIT_FILE.CH%, "	!	under UTILTY and initialize system controlling fields."
		PRINT #WRIT_FILE.CH%, "	!	Run process again."

	CASE "FRM"

		PRINT #WRIT_FILE.CH%, &
			"	!	Create the form library in the ^*" + &
			SYSTEM_TYPE$ + "\* going"
		PRINT #WRIT_FILE.CH%, "	!	under UTILTY. Make sure to create a form with the right name."

	CASE "JRL"

		PRINT #WRIT_FILE.CH%, &
			"	!	Create the journal file in the ^*" + &
			SYSTEM_TYPE$ + "\* going"
		PRINT #WRIT_FILE.CH%, "	!	under JOURNAL."

	CASE "MAS"

		PRINT #WRIT_FILE.CH%, "	!	Create the file in the ^*" + &
			SYSTEM_TYPE$ + "\* going"
		PRINT #WRIT_FILE.CH%, "	!	under MASTER run process again."

	CASE "TBL"

		PRINT #WRIT_FILE.CH%, "	!	Create the file in the ^*" + &
			SYSTEM_TYPE$ + "\* going"
		PRINT #WRIT_FILE.CH%, "	!	under MASTER and TABLE and run process again."

	CASE ELSE

		PRINT #WRIT_FILE.CH%, "	!	This file is created by posting. Check the report settings "
		PRINT #WRIT_FILE.CH%, "	!	for selected history."
	END SELECT

	PRINT #WRIT_FILE.CH%, "	!"
	PRINT #WRIT_FILE.CH%, "	! Index:"
	PRINT #WRIT_FILE.CH%, "	!	.x " + DESCR$
	PRINT #WRIT_FILE.CH%, "	!--"

	PRINT "FILE " + FILE_NAME1$ + " HAS BEEN CHANGED."

	RETURN

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

	FILENAME$ = ""
	RESUME HelpError

 EndProgram:
32767	END
