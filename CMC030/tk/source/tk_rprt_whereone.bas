1	%TITLE "Module Where Used on the Highest Level"
	%SBTTL "TK_RPRT_WHEREONE"
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
	! ID:TK013
	!
	! Abstract:HELP
	!	.p
	!	This program prints out where are modules used on the
	!	highest level
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_WHEREONE/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_WHEREONE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_WHEREONE.OBJ;*
	!
	! AUTHOR:
	!
	!	01/23/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/13/92 - Kevin Handy
	!		Removed excess error trapping (check)
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
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

	DIM RFA RFA_LEVEL(100%)
	DIM REAL QTY_LEVEL(100%)
	DIM STRING TEST_CHILD(100%)

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
	WLDSUBTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	WLDTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	WLDLEVEL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	PRINTZERO$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

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
	TITLE$(1%) = "WHERE  USED  MODULES  ON  THE  HIGHEST  LEVEL"
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "Submodule                               " + &
		"Description                             " + &
		"                     Type"

	TITLE$(5%) = SPACE$(40%) + "Level Type Module           " + &
		"                       Description      " + &
		"                     Qty"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_RELATION.CH%, KEY #1%
		ELSE
			FIND #TK_RELATION.CH%, KEY #1% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
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
		IF COMP_STRING(EDIT$(TK_RELATION::CHILD, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOTO ExitTotal IF (TK_RELATION::CHILD > TO_ITEM$) &
		AND TO_ITEM$ <> ""

17300	!
	! Print out line
	!
	TK_CHILD$ = TK_RELATION::CHILD
	GOSUB 18500

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(TK_MODULE::MODTYPE, -1%), WLDSUBTYPE$) = 0% &
		AND WLDSUBTYPE$ <> ""

	TEXT$ = TK_RELATION::CHILD + " " + &
		TK_MODULE::DESCRIPTION + " " + &
		TK_MODULE::MODTYPE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	!
	TEST_CHILD(LEVEL%) = TK_RELATION::CHILD
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * TK_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(TK_RELATION.CH%)
	GOSUB 18000
	GOTO 17330 IF QTY_LEVEL(LEVEL%) = 0.0
	GOTO ExitProgram IF UTL_REPORTX::STAT

17320	WHEN ERROR IN
		GET #TK_RELATION.CH%, KEY #1% EQ TK_RELATION::PARENT
	USE
		CONTINUE 17330 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	!
	LEVEL% = LEVEL% - 1%
	GOTO 17350 IF LEVEL% = 0%

17330	WHEN ERROR IN
		GET #TK_RELATION.CH%, &
			KEY #1% EQ TEST_CHILD(LEVEL%), &
			REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

	RFA_LEVEL(0%) = GETRFA(TK_RELATION.CH%)

	UNTIL RFA_LEVEL(0%) = RFA_LEVEL(LEVEL%)
		WHEN ERROR IN
			GET #TK_RELATION.CH%, REGARDLESS
		USE
			CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
			FILENAME$ = "TK_RELATION"
			CONTINUE HelpError
		END WHEN

		RFA_LEVEL(0%) = GETRFA(TK_RELATION.CH%)
	NEXT

	WHEN ERROR IN
		GET #TK_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

	IF TK_RELATION::CHILD <> TEST_CHILD(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

17350	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	WHEN ERROR IN
		FIND #TK_RELATION.CH%, &
			KEY #1% GE TEST_CHILD(1%) + CHR$(255%), &
			REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1%)

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

18000	GOTO Ret18000 IF PRINTZERO$ = "Y" AND QTY_LEVEL(LEVEL%) <> 0.0
	IF COMP_STRING(NUM1$(LEVEL%), WLDLEVEL$) <> 0% OR &
		EDIT$(WLDLEVEL$, -1%) = ""
	THEN
		TK_CHILD$ = TK_RELATION::PARENT
		GOSUB 18500
		IF COMP_STRING(EDIT$(TK_MODULE::MODTYPE, -1%), WLDTYPE$) OR &
			WLDTYPE$ <> ""
		THEN
			TEXT$ = TEST_CHILD(1%) + "  " + &
				FORMAT$(LEVEL%,	"####") + " " + &
				TK_MODULE::MODTYPE + " " + &
				TK_RELATION::PARENT + " " + &
				LEFT(TK_MODULE::DESCRIPTION, 35%) + " " + &
				FORMAT$(QTY_LEVEL(LEVEL%), "#,###")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		END IF
	END IF
 Ret18000:
	RETURN

18500	!
	! Read Module File
	!
	WHEN ERROR IN
		GET #TK_MODULE.CH%, KEY #0% EQ TK_CHILD$, REGARDLESS
	USE
		TK_MODULE::DESCRIPTION = &
			STRING$(LEN(TK_MODULE::DESCRIPTION), A"?"B)
		TK_MODULE::MODTYPE = &
			STRING$(LEN(TK_MODULE::MODTYPE), A"?"B)

		CONTINUE 18520 IF ERR = 155%
		FILENAME$ = "TK_MODULE"
		CONTINUE HelpError
	END WHEN

18510	RETURN

18520	!
	! Check DEC function
	!
	IF INSTR(1%, TK_CHILD$, "$")
	THEN
		TK_MODULE::DESCRIPTION = "DEC Utility routine"
		TK_MODULE::MODTYPE ="DEC"
	END IF

	GOTO 18510

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
