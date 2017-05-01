1	%TITLE "Module Size Listing"
	%SBTTL "TK_RPRT_MODULESIZE"
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
	! ID:TK014
	!
	! Abstract:HELP
	!	.p
	!	This program prints out a list of the existing modules
	!	and the number of lines of code in each module.
	!	.note
	!	This report will only print out those files that have been entered
	!	into the modulelist by "TK STRUCT SETMOD."
	!	.end note
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_MODULESIZE/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_MODULESIZE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_MODULESIZE.OBJ;*
	!
	! AUTHOR:
	!
	!	01/16/87  - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	08/28/91 - Kevin Handy
	!		Added subtotals.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/15/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
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
	! Assign a channel for the module source file
	!
	CALL ASSG_CHANNEL(LOC_SOURCE.CH%, STAT%)

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	LISTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

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
	TITLE$(1%) = "MODULE  SIZE LIST  BY " + LISTBY$
	TITLE$(2%) = "Programmers tool kit"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = SPACE$(104%) + &
		"--------Lines--------"

	TITLE$(5%) = "Module Name                    " + &
		"Description                                        " + &
		"Catgry " + &
		"Type " + &
		"Language" + &
		"   Code " + &
		" Commnt " + &
		"  Blank"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_MODULE.CH%, KEY #LISTBY%
		ELSE
			FIND #TK_MODULE.CH%, &
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
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::CATEGORY, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF TEST_ITEM$ <> TK_MODULE::CATEGORY AND &
			TEST_ITEM$ <> ""
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3000%)
		END IF

		TEST_ITEM$ = TK_MODULE::CATEGORY

	CASE 2%
		GOTO ExitTotal IF (TK_MODULE::MODTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(TK_MODULE::MODTYPE, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF TEST_ITEM$ <> TK_MODULE::MODTYPE AND &
			TEST_ITEM$ <> ""
		THEN
			GOSUB SubTotal
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3000%)
		END IF

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

17100	!
	! Open source code file
	!
	LOC_BLANKLINE%, LOC_CODELINE%, LOC_COMMENTLINE% = 0%

	LOC_SOURCE$ = TRM$(TK_MODULE::DIRECTORY) + &
		TRM$(TK_MODULE::MODNAME) + "." + &
		TRM$(TK_MODULE::EXTENSION)

	WHEN ERROR IN
		OPEN LOC_SOURCE$ FOR INPUT AS FILE LOC_SOURCE.CH%, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 17300
	END WHEN

17110	!
	! Read one line of source
	!
	WHEN ERROR IN
		LINPUT #LOC_SOURCE.CH%, LOC_LINE$
	USE
		CONTINUE 17300
	END WHEN

	!
	! Select the language
	!
	SELECT EDIT$(TK_MODULE::LANGUAGE, -1%)

	CASE "BASIC"
		!
		! Because line numbers and labels will compile
		! Line numbers and labels are considered code.
		!

		! Remove all spaces and tabs
		LOC_LINE$ = EDIT$(LOC_LINE$, -1%)
		SELECT LEFT(LOC_LINE$, 1%)
		CASE "!"
			LOC_COMMENTLINE% = LOC_COMMENTLINE% + 1%
		CASE ""
			LOC_BLANKLINE% = LOC_BLANKLINE% + 1%
		CASE ELSE
			LOC_CODELINE% = LOC_CODELINE% + 1%
		END SELECT
	END SELECT

	GOTO 17110

17300	!
	! Print out one line
	!
	TEXT$ = LEFT(TK_MODULE::MODNAME, 30%) + " " + &
		LEFT(TK_MODULE::DESCRIPTION, 50%) + " " + &
		TK_MODULE::CATEGORY + " " + &
		TK_MODULE::MODTYPE + " " + &
		TK_MODULE::LANGUAGE  + &
		FORMAT$(LOC_CODELINE%, "####### ") + &
		FORMAT$(LOC_COMMENTLINE%, "####### ") + &
		FORMAT$(LOC_BLANKLINE%, "#######")

	!
	! Add to total line counts
	!
	LOC_TOTALCODELINE = LOC_TOTALCODELINE + LOC_CODELINE%
	LOC_TOTALCOMMENTLINE = LOC_TOTALCOMMENTLINE + LOC_COMMENTLINE%
	LOC_TOTALBLANKLINE = LOC_TOTALBLANKLINE + LOC_BLANKLINE%

	LOC_SUBTOTALCODELINE = LOC_SUBTOTALCODELINE + LOC_CODELINE%
	LOC_SUBTOTALCOMMENTLINE = LOC_SUBTOTALCOMMENTLINE + LOC_COMMENTLINE%
	LOC_SUBTOTALBLANKLINE = LOC_SUBTOTALBLANKLINE + LOC_BLANKLINE%

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
	SELECT LISTBY%
	CASE 1%, 2%
		GOSUB SubTotal
	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(94%) + &
		"Total   " + &
		FORMAT$(LOC_TOTALCODELINE, "####### ") + &
		FORMAT$(LOC_TOTALCOMMENTLINE, "####### ") + &
		FORMAT$(LOC_TOTALBLANKLINE, "#######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 SubTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(93%) + &
		"Sub Total" + &
		FORMAT$(LOC_SUBTOTALCODELINE, "####### ") + &
		FORMAT$(LOC_SUBTOTALCOMMENTLINE, "####### ") + &
		FORMAT$(LOC_SUBTOTALBLANKLINE, "#######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	LOC_SUBTOTALCODELINE = 0.0
	LOC_SUBTOTALCOMMENTLINE = 0.0
	LOC_SUBTOTALBLANKLINE = 0.0

	RETURN

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
	!+-+-+
	!++
	! Abstract:FLD05
	!	^*(05) Sort By\*
	!	.p
	!	.list 0,"*"
	!	.le
	!	I - File ID
	!	.le
	!	C - Category (ie. System)
	!	.le
	!	N - Source Name
	!	.le
	!	T - Type
	!	.le
	!	L - Language (ie. BASIC or C)
	!	.els
	!
	! Index:
	!
	!--
