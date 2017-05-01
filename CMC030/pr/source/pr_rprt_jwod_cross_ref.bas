1	%TITLE "PR JWOD Cross Reference TableReport"
	%SBTTL "PR_RPRT_JWOD_CROSS_REF"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402.
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
	! ID:PR109
	!
	! Abstract:HELP
	!	.p
	!	This report will print the JWOD
	!	cross reference table, which defines which jobs
	!	are JWOD and which ones are not.
	!
	! Index:
	!	.x JWOD Table>Report
	!	.x Report>JWOD Table
	!
	! Option:
	!
	! Author:
	!
	!	05/26/93 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_JWOD_CROSS_REF
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_JWOD_CROSS_REF, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_JWOD_CROSS_REF.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/14/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.HB"
	MAP	(PR_JWOD_CROSS_REF)	PR_JWOD_CROSS_REF_CDD	PR_JWOD_CROSS_REF

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
	!	to begin from a particular
	!	item.
	!	.p
	!	A blank in this field will cause the report to begin with
	!	the first item in the file.
	!
	! Index:
	!	.x From Item>Overhead Table Report
	!	.x Overhead Table>Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with a particular item.
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Overhead Table Report
	!	.x Overhead Table>Report>To Item
	!
	!--


	K_NUM% = 0%


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.OPN"
	USE
		FILENAME$ = "PR_JWOD_CROSS_REF"
		CONTINUE HelpError
	END WHEN

310	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "JWOD CROSS REFERENCE TABLE REPORT"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Subaccount  Flag"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_JWOD_CROSS_REF.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_JWOD_CROSS_REF.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_JWOD_CROSS_REF"
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
		GET #PR_JWOD_CROSS_REF.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11
		FILENAME$ = "PR_JWOD_CROSS_REF"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_JWOD_CROSS_REF::SUBACCT > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	!
	! Print out one line
	!
	TEXT$ = PR_JWOD_CROSS_REF::SUBACCT + "   " + &
		PR_JWOD_CROSS_REF::FLAG

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
