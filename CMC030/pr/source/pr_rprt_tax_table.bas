1	%TITLE "PR Tax Table Report"
	%SBTTL "PR_RPRT_TAX_TABLE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:PR062
	!
	! Abstract:HELP
	!	.P
	!	The ^*Tax Table\* option
	!	lists the Tax Table file.
	!
	! Index:
	!	.x Tax Table>Report
	!	.x Report>Tax Table
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TAX_TABLE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_TAX_TABLE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TAX_TABLE.OBJ;*
	!
	! Modification history:
	!
	!	12/22/89 - Kevin Handy
	!		Fixed bug where would only do the current
	!		year.
	!
	!	12/22/89 - Kevin Handy
	!		Added the year being printed to the title.
	!
	!	12/26/90 - Kevin Handy
	!		Modified to display the HI portion of the FICA
	!		seperately from the OASDI.
	!
	!	12/27/90 - Kevin Handy
	!		Modified to display additional personal exemptions.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)	PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field enters a
	!	particular item from which the report will start printing.
	!	.p
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!
	! Index:
	!	.x From Item>Tax Table Report
	!	.x Tax Table>Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field enters a particular
	!	item at which the report will end printing.
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>Tax Table Report
	!	.x Tax Table>Report>To Item
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) File Year\*
	!	.p
	!	The ^*File Year\* enters the year for which this report is to
	!	print. Format for entry in YYYY.
	!
	! Index:
	!	.x File Year>Tax Table
	!	.x Tax Table>File Year
	!
	!--
	YYYY$ = LEFT(DATE_TODAY, 4%) IF YYYY$ = ""

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
	USE
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "TAX TABLE REPORT"
	TITLE$(2%) = "FOR " + YYYY$
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = "A Co S  FICA R  FICA E  FICA Limit  CB  Basic %  " + &
		"SUI Min L  SUI Max  SUI Max L"

	TITLE2$ = "                                      OST DLine  " + &
		"OST Min L  OST   %  OST Max L"

	TITLE3$ = "     Std WH All  Adj Gross %  Min Std Adj  " + &
		"Max Std Adj  Pers Exempt  Add Exempt"

	TITLE3A$ = "       Rounding  Thr. Rate    Low Income Fed Credit"

	TITLE4$ = "                   Over               Base             " + &
		" Percent"

	TITLE5$ = STRING$(80%, 61%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TAX_TABLE.CH%, KEY #0%
		ELSE
			FIND #PR_TAX_TABLE.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
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
		GET #PR_TAX_TABLE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TAX_TABLE::AUTH + PR_TAX_TABLE::CODE + &
		PR_TAX_TABLE::TSTATUS > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 20%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF (PR_TAX_TABLE::FICA_EMPR_PCT / 10000.0 > 0.100)
	THEN
		DIVISOR = 1000.0
	ELSE
		DIVISOR = 100.0
	END IF

	TEXT$ = PR_TAX_TABLE::AUTH + " " + &
		PR_TAX_TABLE::CODE + " " + &
		PR_TAX_TABLE::TSTATUS + "  " + &
		FORMAT$(PR_TAX_TABLE::FICA_EMPR_PCT / DIVISOR, "##.##%") + "  " + &
		FORMAT$(PR_TAX_TABLE::FICA_EMPE_PCT / DIVISOR, "##.##%") + "  " + &
		FORMAT$(PR_TAX_TABLE::FICA_LIMIT, "###,###.##") + "  " + &
		PR_TAX_TABLE::CALC_BASIS + "   " + &
		FORMAT$(PR_TAX_TABLE::BASIS_PCT, "##.###%")    + "  " + &
		FORMAT$(PR_TAX_TABLE::SUI_MIN, "######.##") + "  " + &
		FORMAT$(PR_TAX_TABLE::SUI_PCT, "##.###%")   + "  " + &
		FORMAT$(PR_TAX_TABLE::SUI_MAX, "######.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "  " + &
		"   " + &
		"   " + &
		FORMAT$(PR_TAX_TABLE::FICA_EMPR_PCT_HI / DIVISOR, "##.##%") + "  " + &
		FORMAT$(PR_TAX_TABLE::FICA_EMPE_PCT_HI / DIVISOR, "##.##%") + "  " + &
		FORMAT$(PR_TAX_TABLE::FICA_LIMIT_HI, "###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(38%) + &
		FORMAT$(PR_TAX_TABLE::OT_DED_MAX, "######.##") + "  " + &
		FORMAT$(PR_TAX_TABLE::OT_ANL_MIN, "######.##") + "  " + &
		FORMAT$(PR_TAX_TABLE::OT_ANL_PCT, "##.###%")   + "  " + &
		FORMAT$(PR_TAX_TABLE::OT_ANL_MAX, "######.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE3$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "     " + FORMAT$(PR_TAX_TABLE::STD_WH, "###,###.##") + "  " + &
		FORMAT$(PR_TAX_TABLE::ADJ_GRS_PCT, "##,###.###%") + "  " + &
		FORMAT$(PR_TAX_TABLE::MIN_STD_ADJ, "###,###.##") + "   " + &
		FORMAT$(PR_TAX_TABLE::MAX_STD_ADJ, "###,###.##") + "   " + &
		FORMAT$(PR_TAX_TABLE::PR_EX, "###,###.##") + "  " + &
		FORMAT$(PR_TAX_TABLE::PR_EX_ADD, "###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE3A$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = FORMAT$(PR_TAX_TABLE::DROUNDING, "     '         ") + &
		FORMAT$(PR_TAX_TABLE::MINTAX, "########.##") + &
		FORMAT$(PR_TAX_TABLE::LOW_INCOME, "    ###,###.##") + &
		FORMAT$(PR_TAX_TABLE::FED_CREDIT_PCT, "    ###.###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE4$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(1%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(1%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(1%),   "###.###%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(2%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(2%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(2%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(3%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(3%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(3%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(4%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(4%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(4%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(5%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(5%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(5%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(6%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(6%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(6%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(7%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(7%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(7%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(8%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(8%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(8%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(9%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(9%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(9%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(10%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(10%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(10%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(16%) + &
		FORMAT$(PR_TAX_TABLE::OVER(11%),   "#,###,###") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::TAXAMT(11%), "###,###.##") + SPACE$(10%) + &
		FORMAT$(PR_TAX_TABLE::PLUS(11%),   "###.##%")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE5$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
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
