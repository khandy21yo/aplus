1	%TITLE "Pacific Pride Discount List"
	%SBTTL "PP_RPRT_DISCOUNT"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PP003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Pacific Pride Discount List\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Discount Code
	!	.te
	!	Description
	!	.te
	!	Discount Method
	!	.te
	!	Discount Levels
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_RPRT_DISCOUNT/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_DISCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_DISCOUNT.OBJ;*
	!
	! Author:
	!
	!	12/28/92 - Dan Perkins
	!
	! Modification History:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.HB"
	MAP (PP_DISCOUNT)	PP_DISCOUNT_CDD		PP_DISCOUNT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Code\*
	!	.b
	!	.lm +5
	!	The ^*From Code\* field enters a discount
	!	code with which the report will begin printing.
	!	.b
	!	A blank field causes report to start with the first discount
	!	code in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(01) To Code\*
	!	.b
	!	.lm +5
	!	The ^*To Code\* field enters a discount
	!	code with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the
	!	last discount code in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Code Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Code Wildcard\* field selects
	!	designated discount codes using wildcard characters.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Discount file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.OPN"
	USE
		FILENAME$ = "PP_DISCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DISCOUNT LIST"
	TITLE$(2%) = "Pacific Pride System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "DisCode Description                              " + &
		"Method"

	TITLE$(5%) = "."
	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset Category file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PP_DISCOUNT.CH%
		ELSE
			FIND #PP_DISCOUNT.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PP_DISCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from Category file
	!
	WHEN ERROR IN
		GET #PP_DISCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PP_DISCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO ExitProgram IF PP_DISCOUNT::CODE > TO_ITEM$ AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
		PP_DISCOUNT::CODE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	SELECT PP_DISCOUNT::METHOD

	CASE "V"
		METHOD$ = "Volume   "

	CASE "N"
		METHOD$ = "NonVolume"

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = PP_DISCOUNT::CODE + "    " + &
		PP_DISCOUNT::DESCRIPTION + " " + &
		METHOD$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(25%) + "If Over     Discount Rate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	J% = 0%

	J% = I% IF PP_DISCOUNT::OVER(I%) <> 0.0 FOR I% = 0% TO 9%

	FOR I% = 0% TO J%

		TEXT$ = SPACE$(19%) + &
			FORMAT$(PP_DISCOUNT::OVER(I%), "##,###,###.##") + "     " + &
			FORMAT$(PP_DISCOUNT::RATE(I%), "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Try for next record
	!
	GOTO GetNextRec

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	!
	! Exit from the program after showing error message
	!
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
