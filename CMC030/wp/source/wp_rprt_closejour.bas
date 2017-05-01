1	%TITLE "Print Close Journal"
	%SBTTL "WP_RPRT_CLOSEJOUR"
	%IDENT "V3.6a Calico"

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
	! ID:WP0022
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Print Close Journal\* option prints the journal
	!	relative to jobs being closed.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Print
	!	.x Print>Close Journal
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_CLOSEJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_CLOSEJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_CLOSEJOUR.OBJ;*
	!
	! AUTHOR:
	!
	!	09/18/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	12/04/92 - Frank F. Starman
	!		Added BALFLAG$ variable.
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/08/2002 - Kevin Handy
	!		Make TO_ITEM work.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	DECLARE			JC_JOB_CDD		JC_JOB_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.lm +5
	!	.b
	!	The ^*Batch Number\* field
	!	enters a two (2) digit batch number which has been assigned to the
	!	batch which is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Print>Batch Number
	!	.x Batch Number>>Print>Close Journal
	!	.x Journal>Close>Print
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Job\*
	!	.lm +5
	!	.b
	!	The ^*From Job\* field
	!	enters a job number which will be the first job to print.
	!	.b
	!	If this field is left blank, the report will begin printing with the first job
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Print>From Job
	!	.x Print>Close Journal>From Job
	!	.x From Job>Print>Close Journal
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Job\*
	!	.lm +5
	!	.b
	!	The ^*To Job\* field
	!	enters a job number which will be the last job to print.
	!	.b
	!	If this field is left blank, the report will end printing with the last job
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Print>To Job
	!	.x Print>Close Journal>To Job
	!	.x To Job>Print>Close Journal
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Job Wildcard\*
	!	.lm +5
	!	.b
	!	The ^*Job Wildcard\* field in
	!	enters a wildcard value which will cause the report to print a
	!	selected job or series of jobs depending upon how the wildcard is expressed.
	!	.b
	!	For more information about wildcard techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Print>Close Journal>Wildcard
	!	.x Close Journal>Print>Wildcard
	!	.x Wildcard>Print>Close Journal
	!
	!--

	BALFLAG$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Only Out of Balance\*
	!	.lm +5
	!	.b
	!	The ^*Only Out of Balance\* field
	!	enters a Yes if only out of balance jobs should be printed.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Print>Out of Balance
	!
	!--

300	!
	! Open CLOSE Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.OPN"
	USE
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open CLOSE Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.OPN"
	USE
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CLOSED JOB JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber   Status  Description                  " + &
		"             Type  Class  CloseDate   Operator"
	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	PRINTED_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_CLOSEJOUR.CH%
		ELSE
			FIND #WP_CLOSEJOUR.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #WP_CLOSEJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal &
		IF TO_ITEM$ <> "" AND WP_CLOSEJOUR::JOB > TO_ITEM$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
		IF PRINTED_FLAG%

	PRINTED_FLAG% = 0%

	!
	! Check current record if should be printed
	!
	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(WP_CLOSEJOUR::JOB, -1%), WLDCRD$) = 0% AND &
		WLDCRD$ <> ""

	!
	! Get the job description
	!
	V% = SB_EXAM_SUBACCOUNT("J", WP_CLOSEJOUR::JOB, JC_JOB_EXAM)

	!
	! Print out one line
	!
	TEXT1$ = WP_CLOSEJOUR::JOB + "  " + &
		JC_JOB_EXAM::SSTATUS + "       " + &
		JC_JOB_EXAM::DESCR + "  " + &
		JC_JOB_EXAM::TTYPE + "    " + &
		JC_JOB_EXAM::CLASS + "   " + &
		PRNT_DATE(WP_CLOSEJOUR::CLOSEDATE, 8%) + "  " + &
		WP_CLOSEJOUR::OPERATOR

	IF BALFLAG$ = "N"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		PRINTED_FLAG% = -1%
	END IF

	TEXT2$ = SPACE$(30%) + &
		"                Burden         Labor         Parts" + &
		"        RawMat         Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%) &
		IF BALFLAG$ = "N"

	STD_TOTAL = FUNC_ROUND(WP_CLOSEJOUR::STDBURDEN + &
		WP_CLOSEJOUR::STDLABOR + &
		WP_CLOSEJOUR::STDPARTS + &
		WP_CLOSEJOUR::STDRAWMAT, 2%)

	TEXT3$ = SPACE$(30%) + "Standard  " + &
		FORMAT$(WP_CLOSEJOUR::STDBURDEN, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::STDLABOR, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::STDPARTS, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::STDRAWMAT, "#,###,###.##  ") + &
		FORMAT$(STD_TOTAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT3$, 0%) &
		IF BALFLAG$ = "N"

	ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + &
		WP_CLOSEJOUR::ACTLABOR + &
		WP_CLOSEJOUR::ACTPARTS + &
		WP_CLOSEJOUR::ACTRAWMAT

	TEXT4$ = SPACE$(30%) + "Acutal    " + &
		FORMAT$(WP_CLOSEJOUR::ACTBURDEN, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::ACTLABOR, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::ACTPARTS, "#,###,###.##  ") + &
		FORMAT$(WP_CLOSEJOUR::ACTRAWMAT, "#,###,###.##  ") + &
		FORMAT$(ACT_TOTAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT4$, 0%) &
		IF BALFLAG$ = "N"

	VAR_BURDEN = WP_CLOSEJOUR::STDBURDEN - WP_CLOSEJOUR::ACTBURDEN
	VAR_LABOR  = WP_CLOSEJOUR::STDLABOR  - WP_CLOSEJOUR::ACTLABOR
	VAR_PARTS  = WP_CLOSEJOUR::STDPARTS  - WP_CLOSEJOUR::ACTPARTS
	VAR_RAWMAT = WP_CLOSEJOUR::STDRAWMAT - WP_CLOSEJOUR::ACTRAWMAT

	VAR_TOTAL  = VAR_BURDEN + VAR_LABOR + VAR_PARTS + VAR_RAWMAT

	TEXT5$ = SPACE$(30%) + "Variance  " + &
		FORMAT$(VAR_BURDEN, "#,###,###.##  ") + &
		FORMAT$(VAR_LABOR, "#,###,###.##  ") + &
		FORMAT$(VAR_PARTS, "#,###,###.##  ") + &
		FORMAT$(VAR_RAWMAT, "#,###,###.##  ") + &
		FORMAT$(VAR_TOTAL, "#,###,###.##")

	IF BALFLAG$ = "N"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT5$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram &
			IF UTL_REPORTX::STAT
	END IF

	PASS% = 0%

17100	WHEN ERROR IN
		FIND #WP_CLOSELINE.CH%, &
			KEY #0% EQ WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	PASS% = PASS% + 1%

	IF PASS% = 2%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT3$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT4$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT5$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		PRINTED_FLAG% = -1%
	ELSE
		TOTAL = 0.0
	END IF

	TEXT$ = SPACE$(10%) + &
		"Var Class   Var Account         Description         " + &
		"                            Amount"

	IF BALFLAG$ = "N" OR PASS% = 2%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF


 GetCloseLine:
17120	WHEN ERROR IN
		GET #WP_CLOSELINE.CH%, REGARDLESS
	USE
		CONTINUE PrintTotal IF ERR = 11%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintTotal &
		IF WP_CLOSELINE::JOB <> WP_CLOSEJOUR::JOB

	IF BALFLAG$ = "N" OR PASS% = 2%
	THEN
		!
		! Get the account description
		!
		V% = GL_EXAM_CHART(WP_CLOSELINE::VACCT, GL_CHART_EXAM)

		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM (OPT_ADDREC, WP_CLOSELINE::VACCT, &
			0.0, WP_CLOSELINE::VAMOUNT, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL

		!
		! Print out one line
		!
		TEXT$ = SPACE$(10%) + &
			WP_CLOSELINE::VCLASS + "        " + &
			WP_CLOSELINE::VACCT + "  " + &
			GL_CHART_EXAM::DESCR + "  " + &
			FORMAT$(WP_CLOSELINE::VAMOUNT, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram &
			IF UTL_REPORTX::STAT
	END IF

	TOTAL = FUNC_ROUND(TOTAL + WP_CLOSELINE::VAMOUNT, 2%) &
		IF PASS% = 1%

	GOTO GetCloseLine

 PrintTotal:
	IF PASS% = 1% AND BALFLAG$ = "Y"
	THEN
		GOTO 17100 &
			IF FUNC_ROUND(TOTAL, 2%) <> 0.0
	ELSE
		!
		! Print out one line
		!
		TEXT$ = "     Total" + &
			SPACE$(LEN(WP_CLOSELINE::VCLASS + "        " + &
			WP_CLOSELINE::VACCT + "  " + &
			GL_CHART_EXAM::DESCR + "  ")) + &
			FORMAT$(TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO GetNextRec

 ExitTotal:
	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

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
