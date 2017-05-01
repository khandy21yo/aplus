1	%TITLE "Sub Process to Read CDD File"
	%SBTTL "TK_SUBR_DDLEXTRACT"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_DDLEXTRACT(SMG_DDL_CDD DDL, STRING SYSTEM_NAME, &
		STRING FILE_NAME, INTEGER SUB_STATUS)

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
	! COMPILE:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_DDLEXTRACT
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_DDLEXTRACT
	!	$ DELETE TK_SUBR_DDLEXTRACT.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	06/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, SS$
	!
	!	05/14/99 - Kevin Handy
	!		Turn off broadcast trapping around DDL extract
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose reading of CDD, so I don't need to reinstall it.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"

	EXTERNAL STRING	FUNCTION READ_SYSJOB
	EXTERNAL LONG	READ_3BROADCAST

	!
	! Declare vars
	!
	DECLARE LONG TEXT_FILE.CH

	DIM DATA_TYPE$(100%), DATA_FIELD_SIZE$(100%)

	!
	! Other assignments
	!
	SUB_STATUS = SS$_NORMAL

	JJ$ = READ_SYSJOB
	DATA_TYPE_FILE$ = "CMC:TK_DATA_TYPE.TEMPLATE"

100	!
	! Get the channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(TEXT_FILE.CH)
	IF SMG_STATUS% = LIB$_INSLUN
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
		GOTO ExitProgram
	END IF

500	!
	! Get the ddl file
	!
	GOSUB 12000

	CALL ENTR_3MESSAGE(SCOPE, "Extracting DDL " + FILE_NAME, 1%)

	DDL_FILE$ = "SOURCE:[" + TRM$(SYSTEM_NAME) + ".OPEN]" + &
		TRM$(FILE_NAME) + ".DDL"

	WHEN ERROR IN
		OPEN DDL_FILE$ FOR INPUT AS FILE #TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY
	USE
 !		CONTINUE 505
		CONTINUE 600
	END WHEN

	GOTO 507

 !505	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)
 !
 !	SMG_STATUS% = LIB$SPAWN("DMU EXTRACT CDD$TOP." + TRM$(SYSTEM_NAME) + &
 !		"." + TRM$(FILE_NAME) + " " + DDL_FILE$)
 !
 !	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
 !		LOC(READ_3BROADCAST), LOC(SCOPE))
 !	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
 !
 !	IF (SMG_STATUS% AND 1%) = 0%
 !	THEN
 !		SUB_STATUS = SMG_STATUS%
 !
 !		SELECT SMG_STATUS%
 !		CASE SS$_EXBYTLM
 !
 !		CASE ELSE
 !
 !			CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
 !				" has occured", 0%)
 !
 !		END SELECT
 !		GOTO ExitProgram
 !	END IF
 !
 !	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
 !
 !	WHEN ERROR IN
 !		OPEN DDL_FILE$ FOR INPUT AS FILE #TEXT_FILE.CH, &
 !			ACCESS READ, ALLOW MODIFY
 !	USE
 !		CONTINUE 600
 !	END WHEN

507	FC% = 0%
	CMD_END% = 0%

510	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, CURLINE$
	USE
		CONTINUE 590
	END WHEN

	GOTO 510 IF INSTR(1%, CURLINE$, "DEFINE RECORD") &
		OR EDIT$(CURLINE$, -1%) = ""

	IF RIGHT(CURLINE$, LEN(CURLINE$)) <> "."
	THEN
		WORK_LINE$ = WORK_LINE$ + " " + EDIT$(CURLINE$, 4% + 8% + 128%)
		GOTO 510
	END IF

	WORK_LINE$ = WORK_LINE$ + " " + EDIT$(CURLINE$, 4% + 8% + 128%)

	GOSUB Parse

	WORK_LINE$ = ""
	GOTO 510

590	GOSUB Parse
	DDL::DESCR = FILE_DESC$
	DDL::FIELD_NUM = FC%

600	!
	! Paint DDL window
	!
	CLOSE #TEXT_FILE.CH

 !	KILL DDL_FILE$ WHILE (-1)

 ExitProgram:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	CLOSE #TEXT_FILE.CH

	SMG_STATUS% = LIB$FREE_LUN(TEXT_FILE.CH)

	CALL ENTR_3MESSAGE(SCOPE, &
		"Error " + NUM1$(SMG_STATUS%) + " has occured", 0%) &
		IF (SMG_STATUS% AND 1%) = 0%

	EXIT SUB

 Parse:
	!
	! Parse work line
	!
	RETURN IF WORK_LINE$ = ""

	TEMP% = INSTR(TEMP%, WORK_LINE$, "/*")

	WHILE TEMP%
		TEMP% = INSTR(TEMP% + 2%, WORK_LINE$, "/*")
		IF TEMP%
		THEN
			WORK_LINE$ = LEFT(WORK_LINE$, TEMP% - 1%) + &
				RIGHT(WORK_LINE$, TEMP% + 2%)
			TEMP% = INSTR(1%, WORK_LINE$, "/*")
		END IF
	NEXT

	TEMP% = INSTR(TEMP%, WORK_LINE$, "*/")

	WHILE TEMP%
		TEMP1% = INSTR(TEMP% + 2%, WORK_LINE$, "*/")
		IF TEMP1%
		THEN
			WORK_LINE$ = LEFT(WORK_LINE$, TEMP% - 1%) + &
				RIGHT(WORK_LINE$, TEMP% + 2%)
			TEMP% = INSTR(1%, WORK_LINE$, "*/")
		ELSE
			TEMP% = 0%
		END IF
	NEXT



	TEMP$ = "DESCRIPTION IS "
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP$ = "/*"
		TEMP% = INSTR(1%, WORK_LINE$, "/*")
		IF TEMP%
		THEN
			TEMP1% = INSTR(1%, WORK_LINE$, "*/")
			TEMP1% = LEN(WORK_LINE$) IF TEMP1% = 0%
			FILE_DESC$ = TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
				TEMP1% - (TEMP% + LEN(TEMP$))))
		END IF
	END IF

	TEMP% = INSTR(1%, WORK_LINE$, "DATATYPE")
	IF TEMP%
	THEN
		WORK_LINE$ = EDIT$(WORK_LINE$, 16%)

		FIELD_NAME$, FIELD_ATTRIBUTE$, FIELD_TYPE$, FIELD_ELEMENT$, &
			FIELD_SIZE$, FIELD_DESC$ = ""

		TEMP% = INSTR(1%, WORK_LINE$, "/*")
		IF TEMP%
		THEN
			TEMP1% = INSTR(1%, WORK_LINE$, "*/")
			TEMP$ = TRM$(MID(WORK_LINE$, TEMP% + 2%, &
				TEMP1% - (TEMP% + 2%)))
			WORK_LINE$ = LEFT(WORK_LINE$, TEMP% - 1%) + &
				RIGHT(WORK_LINE$, TEMP1% + 2%)
			TEMP% = INSTR(1%, EDIT$(TEMP$, 32%), "ELEMENT =")
			TEMP1% = INSTR(1%, EDIT$(TEMP$, 32%), "DESCRIPTION =")
			IF TEMP% < TEMP1%
			THEN
				FIELD_ELEMENT$ = EDIT$(MID(TEMP$, &
					TEMP% + 10%, &
					TEMP1% - (TEMP% + 10%)), 8%)
				FIELD_DESC$ = EDIT$(RIGHT(TEMP$, &
					TEMP1% + 14%), 8%)
			ELSE
				IF TEMP% = 0% AND TEMP1% = 0%
				THEN
					FIELD_DESC$ = EDIT$(TEMP$, 8%)
				ELSE
					FIELD_ELEMENT$ = EDIT$(RIGHT(TEMP$, &
						TEMP% + 10%), 8%)
					FIELD_DESC$ = EDIT$(MID(TEMP$, &
						TEMP1% + 14%, &
						TEMP% - (TEMP1% + 14%)), 8%)
				END IF
			END IF
		END IF

		WORK_LINE$ = EDIT$(WORK_LINE$, 8%)

		TEMP% = INSTR(1%, WORK_LINE$, " ")
		FIELD_NAME$ = EDIT$(LEFT(WORK_LINE$, TEMP% - 1%), 8% + 128%)
		WORK_LINE$ = RIGHT(WORK_LINE$, TEMP% + 1%)

		TEMP% = INSTR(1%, WORK_LINE$, "DATATYPE IS")

		IF TEMP% > 1%
		THEN
			FIELD_ATTRIBUTE$ = LEFT(WORK_LINE$, TEMP% - 1%)
		END IF

		WORK_LINE$ = EDIT$(RIGHT(WORK_LINE$, TEMP% + 12%), 8%)

		TEMP$ = "SIZE IS "
		TEMP% = INSTR(1, WORK_LINE$, TEMP$)
		TEMP% = INSTR(1%, WORK_LINE$, ".") IF TEMP% = 0%
		TEMP% = LEN(WORK_LINE$) + 1% IF TEMP% = 0%
		FIELD_TYPE$ = LEFT(WORK_LINE$, TEMP% - 1%)

		TEMP2% = INSTR(1%, WORK_LINE$, TEMP$)
		IF TEMP2%
		THEN
			FIELD_SIZE$ = RIGHT(WORK_LINE$, TEMP2% + LEN(TEMP$))
			FIELD_SIZE$ = LEFT(FIELD_SIZE$, LEN(FIELD_SIZE$) - 1%)
		ELSE
			FOR LOOP% = 1% TO DATA_TYPE%
				IF DATA_TYPE$(LOOP%) = FIELD_TYPE$ AND &
					FIELD_TYPE$ <> "TEXT"
				THEN
					FIELD_SIZE$ = DATA_FIELD_SIZE$(LOOP%)
				END IF
			NEXT LOOP%
		END IF

		FC% = FC% + 1%
		DDL::FIELD_NAME(FC%) = FIELD_NAME$
		DDL::FIELD_ATTRIBUTE(FC%) = FIELD_ATTRIBUTE$
		DDL::FIELD_TYPE(FC%) = FIELD_TYPE$
		DDL::FIELD_ELEMENT(FC%) = FIELD_ELEMENT$
		DDL::FIELD_SIZE(FC%) = FIELD_SIZE$
		DDL::FIELD_DESC(FC%) = FIELD_DESC$
	END IF

	RETURN

	%Page

12000	!
	! Open data type template file
	!
	CLOSE TEXT_FILE.CH

	WHEN ERROR IN
		OPEN DATA_TYPE_FILE$ FOR INPUT AS FILE TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 12020
	END WHEN

	DATA_TYPE% = 0%

12010	!
	! Read data type file
	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, INP$
	USE
		CONTINUE 12020
	END WHEN

	INP$ = EDIT$(INP$, 4% + 8% + 16% + 32% + 128%)

	IF INP$ <> ""
	THEN
		TEMP% = INSTR(1%, INP$, "SIZE>")
		IF TEMP%
		THEN
			DATA_TYPE% = DATA_TYPE% + 1%
			DATA_FIELD_SIZE$(DATA_TYPE%) = RIGHT(INP$, TEMP% + 5%)
			DATA_TYPE$(DATA_TYPE%) = TRM$(LEFT(INP$, TEMP% - 1%))
		END IF
	END IF

	GOTO 12010

12020	!
	! Open data element template file
	!
	CLOSE TEXT_FILE.CH

	RETURN

32767	END SUB
