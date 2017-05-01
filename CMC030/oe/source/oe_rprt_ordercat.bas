1	%TITLE "Sale Category List"
	%SBTTL "OE_RPRT_ORDERCAT"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:OE002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sale Category\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Sale Category
	!	.te
	!	Category Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Sale Category
	!	.x Sale Category>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_ORDERCAT/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_ORDERCAT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_ORDERCAT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification History:
	!
	!	09/25/91 - Deborah K. Fries
	!		Cleaned source code
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

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
	!	^*(01) From Category\*
	!	.b
	!	.lm +5
	!	The ^*From Category\* field enters a category with
	!	which the report will begin printing.
	!	.b
	!	A blank field causes report to start with the first category code in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x Sale Category
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Category\*
	!	.b
	!	.lm +5
	!	The ^*To Category\* field enters a Sale Category
	!	with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to end with the
	!	last Category code in the file.
	!	.lm -5
	!
	! Index:
	!	.x Sale Category
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated categories using wildcard characters.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Order Category file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.OPN"
	USE
		FILENAME$ = "OE_CATEGORY"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ORDER CATEGORY LIST"
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(4%) = "Category       Description"
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
			RESET #OE_CATEGORY.CH%
		ELSE
			FIND #OE_CATEGORY.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_CATEGORY"
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
		GET #OE_CATEGORY.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_CATEGORY"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO ExitProgram IF (OE_CATEGORY::ORDCAT > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(OE_CATEGORY::ORDCAT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = OE_CATEGORY::ORDCAT + "           " + &
		OE_CATEGORY::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
