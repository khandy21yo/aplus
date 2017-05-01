1	%TITLE "Module Listing"
	%SBTTL "TK_RPRT_MODULE"
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
	! ID:TK003
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
	!	$ BAS TK_SOURCE:TK_RPRT_MODULE/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_MODULE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_MODULE.OBJ;*
	!
	! AUTHOR:
	!
	!	01/16/87  - Frank F. Starman
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
	!	11/20/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE) TK_MODULE_CDD TK_MODULE

	%PAGE

30	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	LISTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort (I,N,C,T,L)\*
	!	.P
	!	The ^*Sort\* field specifies the order
	!	to print the report in.  'I' will print the report in
	!	module number order; 'N' will print the report in
	!	module name order; 'C' will print the report in category
	!	order; 'T' will print the report in module type order;
	!	and 'L' will print the report in language order.
	!
	! Index:
	!	.x Sort
	!	.x Report Settings>Sort
	!
	!--


	SELECT LISTBY$

	CASE "I"
		LISTBY% = 3%
		LISTBY$ = "ID NUMBER"

	CASE "N"
		LISTBY% = 0%
		LISTBY$ = "NAME"

	CASE "C"
		LISTBY% = 1%
		LISTBY$ = "CATEGORY"

	CASE "T"
		LISTBY% = 2%
		LISTBY$ = "TYPE"

	CASE "L"
		LISTBY% = 4%
		LISTBY$ = "LANGUAGE"

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.OPN"
	USE
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MODULE  LIST  BY " + LISTBY$
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Module " + &
		"Name                                    " + &
		"Description                              " + &
		SPACE$(10%) + &
		"Catgry " + &
		"Type " + &
		"Language"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_MODULE.CH%, KEY #LISTBY%
		ELSE
			FIND #TK_MODULE.CH%, KEY #LISTBY% GE FROM_ITEM$
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
		GET #TK_MODULE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT LISTBY%

	CASE 0%
		GOTO ExitTotal IF (TK_MODULE::MODNAME > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(TK_MODULE::MODNAME, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE 1%
		GOTO ExitTotal IF (TK_MODULE::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF TEST_ITEM$<>TK_MODULE::CATEGORY AND &
			TEST_ITEM$ <> ""
		TEST_ITEM$ = TK_MODULE::CATEGORY

	CASE 2%
		GOTO ExitTotal IF (TK_MODULE::MODTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::MODTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) &
			IF TEST_ITEM$<>TK_MODULE::MODTYPE AND &
			TEST_ITEM$ <> ""

	CASE 3%
		GOTO ExitTotal IF (TK_MODULE::MODNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::MODNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE 4%
		GOTO ExitTotal IF (TK_MODULE::LANGUAGE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::LANGUAGE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17300	!
	! Print out one line
	!
	TEXT$ = TK_MODULE::MODNUM + " " + &
		TK_MODULE::MODNAME + " " + &
		LEFT(TK_MODULE::DESCRIPTION, 50%) + " " + &
		TK_MODULE::CATEGORY + " " + &
		TK_MODULE::MODTYPE + " " + &
		TK_MODULE::LANGUAGE

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
