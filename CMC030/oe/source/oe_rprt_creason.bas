1	%TITLE "Credit Codes Report"
	%SBTTL "OE_RPRT_CREASON"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center
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
	! ID:OE009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Credit Codes Report\* contains
	!	codes used in issuing a Credit Memo.
	!	.b
	!	The report includes the following information:
	!	.table 3,25
	!	.te
	!	Credit Reason
	!	.te
	!	Credit Description
	!	.te
	!	General Ledger Account Number
	!	.te
	!	General Ledger Account Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Credit Codes Report
	!	.x Credit Codes Report>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_CREASON/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_RPRT_CREASON, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_CREASON.OBJ;*
	!
	!
	! Author:
	!
	!	08/29/91 - JEFF BEARD
	!
	! Modification history:
	!
	!	09/18/91 - Dan Perkins
	!		Realigned report output.
	!		Use GL_EXAM_CHART function to get GL description.
	!		Cleaned up error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/02/2003 - Kevin Handy
	!		Update for changes in OE_CREASON file layout.
	!		Drop GL*, display taxable flag.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

 !	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
 !	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! External functions
	!
 !	EXTERNAL LONG		FUNCTION GL_EXAM_CHART

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a particular customer in the
	!	credit memo file with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first customer
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To item\* field enters a particular customer
	!	with which the report is to end printing.
	!	.b
	!	A blank field will cause the report to end with the last customer
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Credit Reason Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Credit Reason Wildcard\*
	!	selects records matching the wildcard. If left blank,
	!	all records will be printed.
	!	.b
	!	For information on "Wildcarding" techniques, refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Custom file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.OPN"
	USE
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CREDIT MEMO CODES"
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Reason   Description                   " + &
		"              Taxable"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_CREASON.CH%, &
				KEY #0%
		ELSE
			FIND #OE_CREASON.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "OE_CREASON"
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
17030	WHEN ERROR IN
		GET #OE_CREASON.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

	!
	! See if the record should be printed
	!
	GOTO ExitProgram IF OE_CREASON::CREASON > TO_ITEM$ AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(OE_CREASON::CREASON, -1%), WLDCRD$) = 0%

	!
	! GET THE GL DESCRIPTION
	!
 !	V% = GL_EXAM_CHART(OE_CREASON::CR_ACCT, GL_CHART_EXAM)

17050	!
	! Print a line
	!
	TEXT$ = OE_CREASON::CREASON + "       " + &
		OE_CREASON::DESCR + "    " + &
		OE_CREASON::TAXABLE

 !		OE_CREASON::CR_ACCT + "   " + &
 !		GL_CHART_EXAM::DESCR


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

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
