1	%TITLE "Check to see that all fields are in source code"
	%SBTTL "TK_SPEC_CREATEREPF"
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
	!
	! Abstract:
	!	^*Check to see that all fields are in source code\*
	!	.p
	!	Check to see that all fields are in source code
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CREATEREPF/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_CREATEREPF, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CREATEREPF.OBJ;*
	!
	! Author:
	!
	!	01/01/88 - Kevin Handy
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
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
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/17/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD	UTL_REPORT
	DECLARE UTL_REPORT_CDD	TEMP_REPORT

	ON ERROR GOTO 19000

	!
	! Handle output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	! Open REPORT file
	!
500	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.MOD"

	RESET #UTL_REPORT.CH%

	!
	! Get Report
	!
510	INPUT "Directory: "; PREFIX$
	INPUT "Report: "; FILE_NAME$

	GOTO 510 IF PREFIX$ = ""

	CALL FIND_FILE("SOURCE:[" + PREFIX$ + ".SOURCE]" + &
		TRM$(FILE_NAME$) + ".BAS", &
		FILENAME$(), 16%, "", "")

	I_LOOP% = VAL%(FILENAME$(0%))

	FOR J% = 1% TO I_LOOP%

	FILE_NAME$ = FILENAME$(J%)


	!Open the new file we are going to create
515	WHEN ERROR IN
		OPEN "SOURCE:[" + PREFIX$ + ".SOURCE]" + FILE_NAME$ + ".BAS" &
			FOR INPUT AS FILE READ_FILE.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 510 IF ERR = 5%
		IF ERR = 1%
		THEN
			PRINT UTL_REPORT::REPNUM
			CONTINUE 510
		END IF

		CONTINUE HelpError
	END WHEN

	UTL_REPORT::PRODEV = "CMC$ROOT:[" + TRM$(PREFIX$) + "]"
	UTL_REPORT::CANSPOOL = "Y"
	UTL_REPORT::CANDISP = "Y"
	UTL_REPORT::CANDEV = "Y"
	UTL_REPORT::CANFILE = "Y"
	UTL_REPORT::CANDET = "Y"
	UTL_REPORT::REPYN = "Y"

	UTL_REPORT::SPOOL = ""
	UTL_REPORT::DEFOUT = ""
	UTL_REPORT::CHAINTO = ""
	UTL_REPORT::PRINTTYPE = "LA120"
	UTL_REPORT::LASTRUNDATE = ""
	UTL_REPORT::LASTRUNTIME = ""
	UTL_REPORT::BASERUNDATE = ""
	UTL_REPORT::RUNFREQ = ""
	UTL_REPORT::SPOOLFORM = ""

	UTL_REPORT::PRONAM = TRM$(FILE_NAME$)
	PREFIX$ = "UTL" IF PREFIX$ = "UT"
	UTL_REPORT::SYSTEM = TRM$(PREFIX$)
	UTL_REPORT::SUBSYS = ""
	UTL_REPORT::REPNUM = ""

	FOR LOOP% = 1% TO 10%
		UTL_REPORT::DESCR(LOOP% - 1%) = ""
		UTL_REPORT::OPTTYPE(LOOP% - 1%) = ""
		UTL_REPORT::OPTLEN(LOOP% - 1%) = 0%
		UTL_REPORT::VALID(LOOP% - 1%) = ""
		UTL_REPORT::REQUIRE(LOOP% - 1%) = "N"
		UTL_REPORT::OPTDEF(LOOP% - 1%) = ""
		UTL_REPORT::ITEMGROUP(LOOP% - 1%) = ""
		UTL_REPORT::ITEM(LOOP% - 1%) = ""
	NEXT LOOP%

	WHILE 1%
520		WHEN ERROR IN
			LINPUT #READ_FILE.CH%,TEXT$
		USE
			CONTINUE 525 IF ERR = 11%
			FILENAME$ = FILE_NAME$
			CONTINUE HelpError
		END WHEN

		IF INSTR(1%, EDIT$(TEXT$, -1%), "!ID:")
		THEN
			POSIT% = INSTR(1%, TEXT$, "ID:") + 3%
			UTL_REPORT::REPNUM = RIGHT$(TEXT$, POSIT%)
		END IF

		IF INSTR(1%, TEXT$, "OUTP_INITFROMFILE")
		THEN
			POSIT% = INSTR(1%,TEXT$, "%") - 3%
			UTL_REPORT::REPWID = VAL%(MID$(TEXT$, POSIT%, 3%))
		END IF

		IF INSTR(1%, TEXT$, "Abstract:FLD")
		THEN
			FLAG% = 0%
			GOSUB InsertText
		END IF

	NEXT

525	CLOSE READ_FILE.CH%
	TEMP_REPORT = UTL_REPORT
	IF UTL_REPORT::REPNUM = ""
	THEN
		PRINT "Report Number not available."
		GOTO ExitProgram
	END IF

	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ UTL_REPORT::REPNUM
	USE
		CONTINUE 527 IF ERR = 155%
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

	FOR LOOP% = 0% TO 9%
		UTL_REPORT::DESCR(LOOP%) = TEMP_REPORT::DESCR(LOOP%)
		UTL_REPORT::OPTTYPE(LOOP%) = TEMP_REPORT::OPTTYPE(LOOP%)
		UTL_REPORT::OPTLEN(LOOP%) = TEMP_REPORT::OPTLEN(LOOP%)
		UTL_REPORT::VALID(LOOP%) = TEMP_REPORT::VALID(LOOP%)
		UTL_REPORT::REQUIRE(LOOP%) = TEMP_REPORT::REQUIRE(LOOP%)
		UTL_REPORT::OPTDEF(LOOP%) = TEMP_REPORT::OPTDEF(LOOP%)
		UTL_REPORT::ITEMGROUP(LOOP%) = TEMP_REPORT::ITEMGROUP(LOOP%)
		UTL_REPORT::ITEM(LOOP%) = TEMP_REPORT::ITEM(LOOP%)
	NEXT LOOP%

	UPDATE #UTL_REPORT.CH%

	GOTO NextJ

527	UTL_REPORT = TEMP_REPORT
	PUT #UTL_REPORT.CH%

 NextJ:
	Next J%

 ExitProgram:
	GOTO EndProgram


 InsertText:

	TYP% = INSTR(1%, TEXT$, "FLD")
	TYP$ = RIGHT$(TEXT$, TYP% + 3%)
	TYP% = VAL%(TYP$) - 1%

529	LINPUT #READ_FILE.CH%, TEXT$

	POS1% = INSTR(1%, TEXT$, "^*")
	IF (POS1% > 0%) AND (FLAG% = 0%)
	THEN
		POS1% = INSTR(1%, TEXT$, ")") + 1%

		IF POS1% = 1%
		THEN
			POS1% = INSTR(1%, TEXT$, "*") + 1%
		END IF

		POS2% = INSTR(POS1%, TEXT$, "\") - POS1%
		UTL_REPORT::DESCR(TYP%) = MID$(TEXT$, POS1%, POS2%)
		FLAG% = -1%
	END IF

530	IF INSTR(1%, TEXT$, "Datatype:") > 0%
	THEN
		POSIT% = INSTR(1%, TEXT$, ":") + 1%

		SELECT RIGHT$(TEXT$, POSIT%)

		CASE "DATE"
			OPTTYPE$ = "D"

		CASE "FLOATING POINT"
			OPTTYPE$ = "F"

		CASE "INTEGER"
			OPTTYPE$ = "I"

		CASE "PERIOD"
			OPTTYPE$ = "P"

		CASE "YN"
			OPTTYPE$ = "Y"

		CASE "TEXT"
			OPTTYPE$ = "$"

		END SELECT

		UTL_REPORT::OPTTYPE(TYP%) = OPTTYPE$

	END IF

	IF INSTR(1%, TEXT$, "Size:")
	THEN
		POSIT% = INSTR(1%, TEXT$, "Size:")
		UTL_REPORT::OPTLEN(TYP%) = VAL%(RIGHT$(TEXT$, POSIT% + 5%))
	END IF

	IF INSTR(1%, TEXT$, "Required:")
	THEN
		POSIT% = INSTR(1%, TEXT$, ":") + 1%
		UTL_REPORT::REQUIRE(TYP%) = RIGHT$(TEXT$, POSIT%)
	END IF

	IF INSTR(1%, TEXT$, "Valid Input:")
	THEN
		POSIT% = INSTR(1%, TEXT$, ":") + 1%
		UTL_REPORT::VALID(TYP%) = RIGHT$(TEXT$, POSIT%)
	END IF

	IF INSTR(1%, TEXT$, "!--")
	THEN
		RETURN
	END IF

	GOTO 529

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	PRINT ERR
	PRINT ERL

	GOTO EndProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME HelpError

 EndProgram:
32767	END
