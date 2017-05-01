1	%TITLE "File Structure Listing"
	%SBTTL "TK_RPRT_FILE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
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
	! ID:TK008
	!
	! Abstract:HELP
	!	.p
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_FILE/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_FILE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_FILE.OBJ;*
	!
	! AUTHOR:
	!
	!	01/27/87  - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Refrmat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.HB"
	MAP (TK_FILE) TK_FILE_CDD TK_FILE

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION COMP_STRING

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	REC_OR_FLD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	LISTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort (D,N,T)\*
	!	.P
	!	The ^*Sort\* field specifies the order
	!	to print the report in.  'D' will print the report in
	!	database order; 'N' will print the report in
	!	structure name order; 'T' will print the report in type
	!	order.
	!
	! Index:
	!	.x Sort
	!	.x Report Settings>Sort
	!
	!--


	SELECT LISTBY$

	CASE "N"
		LISTBY% = 0%
		LISTBY$ = "NAME"

	CASE "D"
		LISTBY% = 1%
		LISTBY$ = "DATABASE"

	CASE "T"
		LISTBY% = 2%
		LISTBY$ = "DATA  TYPE  AND  SIZE"

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.OPN"
	USE
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "FILE  STRUCTURE  LIST  BY  " + LISTBY$
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Name                                    " + &
		"Description                    " + &
		SPACE$(15%) + &
		"Seq  " + &
		"DB " + &
		"Type " + &
		"DataType             " + &
		"Size "
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_FILE.CH%, KEY #LISTBY%
		ELSE
			FIND #TK_FILE.CH%, KEY #LISTBY% GE FROM_ITEM$
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #TK_FILE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TK_FILE"
		CONTINUE HelpError
	END WHEN

	!
	! If flag set for data items or structure then
	! check to see which one and skip the other
	!
	!GOTO GetNextRec IF COMP_STRING(EDIT$(TK_FILE::FILCLASS, -1%), &
	!	REC_OR_FLD$) = 0% AND REC_OR_FLD$ <> ""

	!
	! Check current record
	!
	SELECT LISTBY%

	CASE 0%
		GOTO ExitTotal IF (TK_FILE::STRUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(TK_FILE::STRUCT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE 1%
		GOTO ExitTotal IF (TK_FILE::DATABASE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(TK_FILE::DATABASE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF TEST_ITEM$ <> TK_FILE::DATABASE AND &
			TEST_ITEM$ <> ""
		TEST_ITEM$ = TK_FILE::DATABASE

	CASE 2%
		GOTO ExitTotal IF (TK_FILE::DATETYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_FILE::DATETYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF TEST_ITEM$ <> TK_FILE::DATETYPE AND &
			TEST_ITEM$ <> ""

		TEST_ITEM$ = TK_FILE::DATETYPE
	END SELECT

17300	!
	! Print out one line
	!
	IF TK_FILE::SEQUENCE = "000"
	THEN
		STR_NAME$ = TK_FILE::STRUCT
	ELSE
		STR_NAME$ = TK_FILE::FLDNAME
	END IF

	TEXT$ = STR_NAME$ + " " + &
		LEFT(TK_FILE::DESCRIPTION, 45%) + " " + &
		TK_FILE::SEQUENCE + "  " + &
		TK_FILE::DATABASE + " " + &
		"     " + &
		TK_FILE::DATETYPE + " " + &
		FORMAT$(TK_FILE::DATASIZE, "#### ")


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
