1	%TITLE "Sale Type Report"
	%SBTTL "OE_RPRT_ORDERTYPE"
	%IDENT "V3.6a Calico"

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
	! ID:OE001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sale Type\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Sale Type
	!	.te
	!	Sale Type Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Entry
	!	.x Order Entry>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_ORDERTYPE/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_ORDERTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_ORDERTYPE.OBJ;*
	!
	! AUTHOR:
	!
	!	06/14/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	09/19/91 - Deborah K. Fries
	!		Clean source code.
	!
	!	11/03/91 - Frank F. Starman
	!		Change Order type to sales type.
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
	!	12/05/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

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
	!	^*(01) From Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*From Sale Type\* field allows entry of a sale type from
	!	which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	sale type in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Sale Type
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*To Sale Type\* field enters a sale type with which the
	!	report is to begin printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	sale type in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Order Type
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Order Type file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.OPN"
	USE
		FILENAME$ = "OE_ORDERTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SALE TYPE REPORT"
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(4%) = "SaleType     Description"
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
			RESET #OE_ORDERTYPE.CH%
		ELSE
			FIND #OE_ORDERTYPE.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_ORDERTYPE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Type record
	!
	WHEN ERROR IN
		GET #OE_ORDERTYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_ORDERTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current Order Type record
	!
	GOTO ExitProgram &
		IF (OE_ORDERTYPE::ORDTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(OE_ORDERTYPE::ORDTYPE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!

	TEXT$ = OE_ORDERTYPE::ORDTYPE + "           " + &
		OE_ORDERTYPE::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next Order Type record
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
