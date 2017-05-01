1	%TITLE "Pacific Pride Site List"
	%SBTTL "PP_RPRT_SITE"
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
	! ID:PP005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Pacific Pride Site List\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Host Number
	!	.te
	!	Site Code
	!	.te
	!	Site Type
	!	.te
	!	Site Name
	!	.te
	!	Address
	!	.te
	!	City
	!	.te
	!	State
	!	.te
	!	Zip
	!	.te
	!	Local Sale Location
	!	.te
	!	Foreign Sale Location
	!	.te
	!	Foreign Purchase Location
	!	.te
	!	Product Number
	!	.te
	!	Federal, State, County, City, Salestax INTP
	!	.te
	!	Federal, State, County, City, Salestax Rates
	!	.te
	!	Federal, State, County, City, Salestax GL Account Numbers
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_RPRT_SITE/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_SITE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_SITE.OBJ;*
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
	!	02/02/93 - Kevin Handy
	!		Fix error trapping for 310 (was 300).
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
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

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
	!	^*(01) From Host\*
	!	.b
	!	.lm +5
	!	The ^*From Host\* field enters the host with
	!	which the report will begin printing.
	!	.b
	!	A blank field causes report to start with the first host
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(01) To Host\*
	!	.b
	!	.lm +5
	!	The ^*To Host\* field enters a host
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the
	!	last host in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Host Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Host Wildcard\* field selects
	!	designated hosts using wildcard characters.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.OPN"
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Site Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SITE LIST"
	TITLE$(2%) = "Pacific Pride System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Host Site Styp SiteName                       " + &
		"City            St LocSalLoc ForSalLoc ForPurLoc"

	TITLE$(5%) = "             Product        " + &
		"FI FRate FedAccount SI SRate StaAccount " + &
		"CI CRate CouAccount CI CRate CtyAccount " + &
		"SI SRate StxAccount "

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	PRINT_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PP_SITE.CH%
		ELSE
			FIND #PP_SITE.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from Site file
	!
	WHEN ERROR IN
		GET #PP_SITE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO ExitProgram IF PP_SITE::HOST > TO_ITEM$ AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(PP_SITE::HOST, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = PP_SITE::HOST + "  " + &
		PP_SITE::SITE + "   " + &
		PP_SITE::STYPE + "    " + &
		PP_SITE::SNAME + " " + &
		PP_SITE::CITY + " " + &
		PP_SITE::STATE + " " + &
		PP_SITE::LOCSALE + "       " + &
		PP_SITE::FORSALE + "       " + &
		PP_SITE::FORPUR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	WHEN ERROR IN
		FIND #PP_SITE_PRODUCT.CH%, KEY #0% EQ PP_SITE::HOST + &
			PP_SITE::SITE + &
			PP_SITE::STYPE, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetExemptRec:
17120	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 11%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO OutaHere IF PP_SITE_PRODUCT::HOST <> PP_SITE::HOST OR &
		PP_SITE_PRODUCT::SITE <> PP_SITE::SITE OR &
		PP_SITE_PRODUCT::STYPE <> PP_SITE::STYPE

	TEXT$ = PP_SITE::HOST + SPACE$(10%) + &
		PP_SITE_PRODUCT::PRODUCT + " " + &
		PP_SITE_PRODUCT::FED_INTP + "  " + &
		FORMAT$(PP_SITE_PRODUCT::FED_RATE, "##.##") + " " + &
		LEFT(PP_SITE_PRODUCT::FED_ACCOUNT, 10%) + " " + &
		PP_SITE_PRODUCT::STA_INTP + "  " + &
		FORMAT$(PP_SITE_PRODUCT::STA_RATE, "##.##") + " " + &
		LEFT(PP_SITE_PRODUCT::STA_ACCOUNT, 10%) + " " + &
		PP_SITE_PRODUCT::COU_INTP + "  " + &
		FORMAT$(PP_SITE_PRODUCT::COU_RATE, "##.##") + " " + &
		LEFT(PP_SITE_PRODUCT::COU_ACCOUNT, 10%) + " " + &
		PP_SITE_PRODUCT::CTY_INTP + "  " + &
		FORMAT$(PP_SITE_PRODUCT::CTY_RATE, "##.##") + " " + &
		LEFT(PP_SITE_PRODUCT::CTY_ACCOUNT, 10%) + " " + &
		PP_SITE_PRODUCT::STX_INTP + "  " + &
		FORMAT$(PP_SITE_PRODUCT::STX_RATE, "##.##") + " " + &
		LEFT(PP_SITE_PRODUCT::STX_ACCOUNT, 10%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_FLAG% = -1%

	GOTO GetExemptRec

 OutaHere:
	!
	! Try for next record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF PRINT_FLAG%
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
