1	%TITLE "Certificate of Min Wage Table Report"
	%SBTTL "PR_RPRT_CERT_MWAGE"
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
	! ID:PR068
	!
	! Abstract:HELP
	!	.p
	!	The ^*Certificate of Minimum Wage Table\* option
	!	prints the
	!	contents of the Certificate of Minimum Wage Table. This list contains the
	!	following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Effective Date
	!	.le
	!	Rate
	!	.els
	!
	! Index:
	!	.x Report>Certificate Minimum Wage Table
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_CERT_MWAGE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_CERT_MWAGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_CERT_MWAGE.OBJ;*
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report
	!		could be set to either a spreadsheet or a DIF file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
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
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.HB"
	MAP	(PR_CERT_MIN_WAGE)	PR_CERT_MIN_WAGE_CDD	PR_CERT_MIN_WAGE

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
	!	The ^*From Item\* field causes the printing
	!	to begin from this particular
	!	item.
	!	.p
	!	A blank in this field causes the report to begin with
	!	the first item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with this particular item.
	!	.p
	!	A blank field causes the report to end with the last item
	!	in the file.
	!
	! Index:
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.OPN"
	USE
		FILENAME$ = "PR_CERT_MIN_WAGE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Certificate of Min Wage Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Effective Date               Rate"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "DEffectiveDate:012,$Rate:033"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_CERT_MIN_WAGE.CH%, KEY #0%
		ELSE
			FIND #PR_CERT_MIN_WAGE.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_CERT_MIN_WAGE"
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
		GET #PR_CERT_MIN_WAGE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_CERT_MIN_WAGE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_CERT_MIN_WAGE::EFF_DATE > &
		TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print out one line
	!
	TEXT$ = "  " + &
		PRNT_DATE(PR_CERT_MIN_WAGE::EFF_DATE, 8%) + "              " + &
		FORMAT$(PR_CERT_MIN_WAGE::RATE, "####.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
