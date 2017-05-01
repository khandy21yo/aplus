1	%TITLE "Module Relation Edit List"
	%SBTTL "TK_RPRT_RELATION"
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
	! ID:TK007
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
	!	$ BAS TK_SOURCE:TK_RPRT_RELATION/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_RPRT_RELATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_RELATION.OBJ;*
	!
	! AUTHOR:
	!
	!	09/10/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.HB"
	MAP (TK_RELATION) TK_RELATION_CDD TK_RELATION

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE) TK_MODULE_CDD TK_MODULE

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
	PRINTZERO$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	FROM_CHILD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	TO_CHILD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	WLDCHILD$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.OPN"
	USE
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MODULES  RELATION  EDIT  LIST"
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	SELECT SORT_BY$
	CASE "M"
		TITLE$(4%) = "Module#                                 " + &
			"Description "
		TITLE$(5%) =    "               Submodule                " + &
			"               Description              " + &
			"                                    DefRef" + &
			"  Quantity"
	CASE  "S"
		TITLE$(4%) = "Submodule#                              " + &
			"Description "
		TITLE$(5%) =    "               Module                   " + &
			"               Description              " + &
			"                                    DefRef" + &
			"  Quantity"
	END SELECT

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	SELECT SORT_BY$
	CASE "M"
		WHEN ERROR IN
			IF FROM_ITEM$ = ""
			THEN
				RESET #TK_RELATION.CH%
			ELSE
				FIND #TK_RELATION.CH%, &
					KEY #0% GE FROM_ITEM$, &
					REGARDLESS
			END IF
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to find beginning record!", 0%)
			CONTINUE ExitProgram
		END WHEN

	CASE "S"
		WHEN ERROR IN
			IF FROM_CHILD$ = ""
			THEN
				RESET #TK_RELATION.CH%, KEY #1%
			ELSE
				FIND #TK_RELATION.CH%, &
					KEY #1% GE FROM_CHILD$, &
					REGARDLESS
			END IF
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to find beginning record!", 0%)
			CONTINUE ExitProgram
		END WHEN

	END SELECT

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #TK_RELATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(TK_RELATION::PARENT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(TK_RELATION::CHILD, -1%), WLDCHILD$) = 0% &
		AND WLDCHILD$ <> ""

	SELECT SORT_BY$
	CASE "M"
		GOTO ExitTotal IF (TK_RELATION::PARENT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF (TK_RELATION::CHILD < FROM_CHILD$) &
			AND FROM_CHILD$ <> ""

		GOTO GetNextRec IF (TK_RELATION::CHILD > TO_CHILD$) &
			AND TO_CHILD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (TK_RELATION::CHILD > TO_CHILD$) &
			AND TO_CHILD$ <> ""

		GOTO GetNextRec IF (TK_RELATION::PARENT < FROM_ITEM$) &
			AND FROM_ITEM$ <> ""

		GOTO GetNextRec IF (TK_RELATION::PARENT > TO_ITEM$) &
			AND TO_ITEM$ <> ""
	END SELECT

17300	!
	! Print out one line
	!
	SELECT SORT_BY$
	CASE "M"

		IF PRINTZERO$ = "Y" AND TK_RELATION::QUANTITY = 0.0 OR &
			PRINTZERO$ = "N"
		THEN
			IF TEST_PRODUCT$ <> TK_RELATION::PARENT
			THEN
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					"", 0%) &
					IF TEST_PRODUCT$ <> ""
				TK_MODULE$ = TK_RELATION::PARENT
				GOSUB 18000
				TEXT$ =  TK_RELATION::PARENT + " " + &
					TK_MODULE::DESCRIPTION
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEXT$, 0%)
			END IF

			TK_MODULE$ = TK_RELATION::CHILD
			GOSUB 18000
			TEXT$ =  SPACE$(15%) + &
				TK_RELATION::CHILD + " " + &
				TK_MODULE::DESCRIPTION + " " + &
				TK_RELATION::DEFREF + "       " + &
				FORMAT$(TK_RELATION::QUANTITY, &
				" ###,###")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			TEST_PRODUCT$ = TK_RELATION::PARENT
		END IF

	CASE "S"
		IF PRINTZERO$ = "Y" AND TK_RELATION::QUANTITY = 0.0 OR &
			PRINTZERO$ = "N"
		THEN
			IF TEST_CHILD$ <> TK_RELATION::CHILD
			THEN
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					"", 0%) &
					IF TEST_CHILD$ <> ""

				TK_MODULE$ = TK_RELATION::CHILD
				GOSUB 18000
				TEXT$ = TK_RELATION::CHILD + " " + &
					TK_MODULE::DESCRIPTION
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEXT$, 0%)
			END IF

			TK_MODULE$ = TK_RELATION::PARENT
			GOSUB 18000
			TEXT$ = SPACE$(15%) + &
				TK_RELATION::PARENT + " " + &
				TK_MODULE::DESCRIPTION + " " + &
				TK_RELATION::DEFREF + "       " + &
				FORMAT$(TK_RELATION::QUANTITY, &
				" ###,###")
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			TEST_CHILD$ = TK_RELATION::CHILD
		END IF

	END SELECT

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

18000	!
	! Read description file
	!
	WHEN ERROR IN
		GET #TK_MODULE.CH%, KEY #0% EQ TK_MODULE$, REGARDLESS
	USE
		TK_MODULE::DESCRIPTION = &
			STRING$(LEN(TK_MODULE::DESCRIPTION), A"?"B)

		CONTINUE 18020 IF ERR = 155%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

 Ret18000:
	RETURN

18020	!
	! Check DEC function
	!
	IF INSTR(1%, TK_MODULE$, "$")
	THEN
		TK_MODULE::DESCRIPTION = "DEC Utility routine"
	END IF

	GOTO Ret18000

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
