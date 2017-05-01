1	%TITLE "Date Element Description"
	%SBTTL "TK_RPRT_ELEMENT"
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
	! ID:TK009
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
	!	$ BAS TK_SOURCE:TK_RPRT_ELEMENT/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_ELEMENT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_ELEMENT.OBJ;*
	!
	! AUTHOR:
	!
	!	02/08/88 - Lance Williams
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
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add some REGARDLESS
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_ELEMENT.HB"
	MAP (TK_ELEMENT) TK_ELEMENT_CDD TK_ELEMENT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	LISTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	SELECT LISTBY$

	CASE "N"
		LISTBY% = 0%
		LISTBY$ = "NAME"

	CASE "D"
		LISTBY% = 1%
		LISTBY$ = "DATATYPE"

	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_ELEMENT.OPN"
	USE
		FILENAME$ = "TK_ELEMENT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DATE  ELEMENT  LIST BY " + LISTBY$
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "Name                                    " + &
		"Description                             " + &
		" DataType             Size TestStruct"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_ELEMENT.CH%, KEY #LISTBY%
		ELSE
			FIND #TK_ELEMENT.CH%, &
				KEY #LISTBY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
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
		GET #TK_ELEMENT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TK_ELEMENT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT LISTBY%

	CASE 0%
		GOTO ExitTotal IF (TK_ELEMENT::ELEMENT > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(TK_ELEMENT::ELEMENT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE 1%
		GOTO ExitTotal IF (TK_ELEMENT::ETYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_ELEMENT::ETYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF TEST_ITEM$ <> TK_ELEMENT::ETYPE + &
			FORMAT$(TK_ELEMENT::ESIZE, "#####") AND TEST_ITEM$ <> ""
		TEST_ITEM$ = TK_ELEMENT::ETYPE + &
			FORMAT$(TK_ELEMENT::ESIZE, "#####")

	END SELECT

17300	!
	! Print out one line
	!
	TEXT$ = TK_ELEMENT::ELEMENT + " " + &
		LEFT(TK_ELEMENT::DESCR, 40%) + " " + &
		TK_ELEMENT::ETYPE + " " + &
		FORMAT$(TK_ELEMENT::ESIZE, "##### ") + &
		LEFT(TK_ELEMENT::TESTSTRUCT, 25%)

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
