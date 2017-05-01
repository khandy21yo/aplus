1	%TITLE "Sub Process to List CDD"
	%SBTTL "TK_SUBR_DDLLIST"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_DDLLIST(STRING PAR_DATABASE, &
		STRING PAR_STRUCTNAME, &
		STRING PAR_STRUCTLIST())

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
	!	A process creates a structure list.
	!
	! Index:
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_DDLLIST
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_DDLLIST
	!	$ DELETE TK_SUBR_DDLLIST.OBJ;*
	!
	! AUTHOR:
	!
	!	04/06/89 - Robert Peterson
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
	!		Reformst source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, SS$
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Don't use CDD to extract list from.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

 !	EXTERNAL STRING  FUNCTION READ_SYSJOB

	!
	! Declare vars
	!
	DECLARE LONG	LOC_TEXTFILE.CH, &
			LOC_LSTCNT, &
			LOC_SEMICOLON, &
			LOC_STATUS

	DECLARE STRING	LOC_FILE, &
			LOC_LINE, &
			LOC_SEARCHDIR

100	!
	! Get the channels from VMS
	!
	LOC_STATUS = LIB$GET_LUN(LOC_TEXTFILE.CH)
	IF LOC_STATUS = LIB$_INSLUN
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No free channels from VMS:  " + NUM1$(LOC_STATUS), 0%)
		GOTO ExitProgram
	END IF

 LookUpCDD:
	!
	! Get the list
	!
	CALL ENTR_3MESSAGE(SCOPE, "Extracting structure list", 1%)

	LOC_FILE$ = "SOURCE:[" + TRM$(PAR_DATABASE) + ".OPEN]" + &
		TRM$(PAR_STRUCTNAME) + ".DDL"

	!
	! Name temporary file to write list to
	!
 !	LOC_FILE = "TEMP_" + READ_SYSJOB + ".LIS"

	!
	! Create directory to do search on
	!
 !	LOC_SEARCHDIR = "CDD$TOP." + TRM$(PAR_DATABASE)
 !	LOC_SEARCHDIR = LOC_SEARCHDIR + "." + TRM$(PAR_STRUCTNAME) &
 !		IF TRM$(PAR_STRUCTNAME) <> ""
 !
 !	LOC_STATUS = LIB$SPAWN("DMU LIST/OUTPUT=" + LOC_FILE + " " + &
 !		LOC_SEARCHDIR)
 !
 !	IF (LOC_STATUS AND 1%) = 0%
 !	THEN
 !		SELECT LOC_STATUS
 !		CASE SS$_EXBYTLM
 !
 !		CASE ELSE
 !
 !			CALL ENTR_3MESSAGE(SCOPE, &
 !				"Error " + NUM1$(LOC_STATUS) + &
 !				" has occured", 0%)
 !
 !		END SELECT
 !
 !		GOTO ExitProgram
 !	END IF
 !
 !	LOC_STATUS = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

500	!
	! Open temporary work file
	!
	WHEN ERROR IN
		OPEN LOC_FILE FOR INPUT AS FILE #LOC_TEXTFILE.CH, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 600
	END WHEN

510	!
	! Get a line from file
	!
	WHEN ERROR IN
		LINPUT #LOC_TEXTFILE.CH, LOC_LINE
	USE
		CONTINUE 600
	END WHEN

	LOC_LINE = EDIT$(LOC_LINE, -1%)

	LOC_SEMICOLON = INSTR(1%, LOC_LINE, ";")

	IF LOC_SEMICOLON
	THEN
		LOC_LINE = LEFT(LOC_LINE, LOC_SEMICOLON - 1%)
	END IF

	LOC_LSTCNT = LOC_LSTCNT + 1%

	PAR_STRUCTLIST(LOC_LSTCNT) = LOC_LINE

	GOTO 510

600	!
	! store the number of elements in the array in the
	! zero element.
	!
	PAR_STRUCTLIST(0%) = NUM1$(LOC_LSTCNT)

	!
	! Paint DDL window
	!
	CLOSE #LOC_TEXTFILE.CH

 !	WHEN ERROR IN
 !		KILL LOC_FILE WHILE (-1)
 !	USE
 !		CONTINUE ExitProgram
 !	END WHEN

 ExitProgram:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	LOC_STATUS = LIB$FREE_LUN(LOC_TEXTFILE.CH)

	CALL ENTR_3MESSAGE(SCOPE, &
		"Error " + NUM1$(LOC_STATUS) + " has occured", 0%) &
		IF (LOC_STATUS AND 1%) = 0%

	EXIT SUB

32767	END SUB
