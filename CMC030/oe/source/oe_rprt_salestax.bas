1	%TITLE "Sales Tax Report"
	%SBTTL "OE_RPRT_SALESTAX"
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
	! ID:OE004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Tax\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Tax Code
	!	.te
	!	Tax Description
	!	.te
	!	State Tax Percentage
	!	.te
	!	GL State Account
	!	.te
	!	City Tax Percentage
	!	.te
	!	City Account
	!	.te
	!	County Tax Percentage
	!	.te
	!	County Account
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Sales Tax
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SALESTAX/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SALESTAX, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SALESTAX.OBJ;*
	!
	! AUTHOR:
	!
	!	06/24/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	09/20/91 - Deborah K. Fries
	!		Cleaned source code
	!		Improved error trapping
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!		Reformat source closer to 80 columns.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/14/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX

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
	!	^*(01) From Sales Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*From Sales Tax Code\* field enters the
	!	sales tax code with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	sales tax code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Sales Tax Code
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Sales Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*To Sales Tax Code\* field enters the
	!	sales tax code with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	sales tax code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Sales Tax Code
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
	!	using the Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Order Salestax file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.OPN"
	USE
		FILENAME$ = "OE_SALESTAX"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SALES TAX REPORT"
	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "TaxCode Description             StateTax%   " + &
		"GLStateAcc             CityTax% CityAcc               " + &
		"CountyTax% CountyAcc"
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
			RESET #OE_SALESTAX.CH%
		ELSE
			FIND #OE_SALESTAX.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_SALESTAX"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Salestax record
	!
	WHEN ERROR IN
		GET #OE_SALESTAX.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_SALESTAX"
		CONTINUE HelpError
	END WHEN

	!
	! Check current Order Salestax record
	!
	GOTO ExitProgram IF (OE_SALESTAX::TAXCODE > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$(OE_SALESTAX::TAXCODE, -1%), &
		WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = OE_SALESTAX::TAXCODE + "      " + &
		OE_SALESTAX::JURISDICTION + " " + &
		FORMAT$(OE_SALESTAX::STATETAX, "#,###,###.##") + "   " + &
		OE_SALESTAX::STATEACC + " " + &
		FORMAT$(OE_SALESTAX::CITYTAX, "#,###,###.##") + " " + &
		OE_SALESTAX::CITYACC + "  " + &
		FORMAT$(OE_SALESTAX::COUNTYTAX, "#,###,###.##") + " " + &
		OE_SALESTAX::COUNTYACC

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next Order Salestax record
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
