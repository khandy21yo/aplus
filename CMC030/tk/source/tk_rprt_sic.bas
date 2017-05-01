1	%TITLE "STANDARD INDUSTRIAL CODES FILE LIST"
	%SBTTL "TK_RPRT_SIC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:TK012
	!
	! Abstract:HELP
	!	.p
	!	This program prints Standard Industrial Codes File List
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_SIC/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_SIC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_SIC.OBJ;*
	!
	! Author:
	!
	!	08/09/88 - J. Shad Rydalch
	!
	! Modification History:
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[TK.OPEN]TK_SIC.HB"
	MAP	(TK_SIC)	TK_SIC_CDD	TK_SIC

	%INCLUDE "SOURCE:[TK.OPEN]TK_DIVISION.HB"
	MAP	(TK_DIVISION)	TK_DIVISION_CDD	TK_DIVISION

	%INCLUDE "SOURCE:[TK.OPEN]TK_BUSINESSTYPE.HB"
	MAP	(TK_BUSINESSTYPE)	TK_BUSINESSTYPE_CDD	TK_BUSINESSTYPE

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!
	!	^* From Item \*
	!	.p
	!	This is test.
	!
	! Index:
	! Datatype:TEXT
	! Size:20
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!
	! Datatype:TEXT
	! Size:20
	!--
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!
	! Datatype:TEXT
	! Size:20
	!--
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!
	! Datatype:TEXT
	! Size:2
	! Required:Y
	! Valid Input: BT,DS,DV,BT
	!--

	SELECT SORT_BY$
	CASE "SI"
		SORT_KEY% = 0%
		ADD_TITLE$ = "By  SIC"
	CASE "DV"
		SORT_KEY% = 1%
		ADD_TITLE$ = "By  Division"
	CASE "DS"
		SORT_KEY% = 2%
		ADD_TITLE$ = "By  Description"
	CASE "BT"
		SORT_KEY% = 3%
		ADD_TITLE$ = "By  Business Type"
	END SELECT


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_SIC.OPN"
	USE
		FILENAME$ = "TK_SIC"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_DIVISION.OPN"
	USE
		FILENAME$ = "TK_DIVISION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_BUSINESSTYPE.OPN"
	USE
		FILENAME$ = "TK_BUSINESSTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Standard Industrial Classification Codes " + &
		ADD_TITLE$
	TITLE$(2%) = "TK System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "SIC    Description                             " + &
		"   BT  DV"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_SIC.CH%, KEY #SORT_KEY%
		ELSE
			FIND #TK_SIC.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
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
		GET #TK_SIC.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TK_SIC"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "SI"
		GOTO ExitTotal IF (TK_SIC::SIC > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TK_SIC::SIC, -1%), WLDCRD$) = 0%

	CASE "DV"
		GOTO ExitTotal IF (TK_SIC::DIVISION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TK_SIC::DIVISION, -1%), WLDCRD$) = 0%

		GOTO DivTest IF TK_SIC::DIVISION = TEST_DIVISION$

		TK_DIVISION::DESCRIPTION = &
			STRING$(LEN(TK_DIVISION::DESCRIPTION), A"?"B)

17200		WHEN ERROR IN
			GET #TK_DIVISION.CH%, &
				KEY #0% EQ TK_SIC::DIVISION, &
				REGARDLESS
		USE
			CONTINUE 17210 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "TK_DIVISION"
			CONTINUE HelpError
		END WHEN

17210		IF TEST_DIVISION$ <> " "
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%)
		END IF

		TEXT$ = "    " + &
			TK_SIC::DIVISION + " " + &
			TK_DIVISION::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 DivTest:
		TEST_DIVISION$ = TK_SIC::DIVISION

	CASE "DS"
		GOTO ExitTotal IF (TK_SIC::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TK_SIC::DESCRIPTION, -1%), WLDCRD$) = 0%

	CASE "BT"
		GOTO ExitTotal IF (TK_SIC::BUSINESSTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(TK_SIC::BUSINESSTYPE, -1%), WLDCRD$) = 0%

		GOTO BusTest IF TK_SIC::BUSINESSTYPE = TEST_BUSINESSTYPE$

		TK_BUSINESSTYPE::DESCRIPTION = &
			STRING$(LEN(TK_BUSINESSTYPE::DESCRIPTION), A"?"B)

17250		WHEN ERROR IN
			GET #TK_BUSINESSTYPE.CH%, &
				KEY #0% EQ TK_SIC::BUSINESSTYPE, &
				REGARDLESS
		USE
			CONTINUE 17260 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "TK_BUSINESSTYPE"
			CONTINUE HelpError
		END WHEN

17260		IF TEST_BUSINESSTYPE$ <> " "
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%)
		END IF

		TEXT$ = "    " + &
			TK_SIC::BUSINESSTYPE + " " + &
			TK_BUSINESSTYPE::DESCRIPTION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 BusTest:
		TEST_BUSINESSTYPE$ = TK_SIC::BUSINESSTYPE

	END SELECT

17300	!
	! Print out one line
	!
	TEXT$ = TK_SIC::SIC + " " + &
		TK_SIC::DESCRIPTION + "   " + &
		TK_SIC::BUSINESSTYPE + "  " + &
		TK_SIC::DIVISION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!

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

