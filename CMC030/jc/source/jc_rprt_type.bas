1	%TITLE "Job Type List"
	%SBTTL "JC_RPRT_TYPE"
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
	! ID:SA0003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Type List\* option prints a list of the Job
	!	Type file. The report contains the following fields:
	!	.table 3,15
	!	.te
	!	Type
	!	.TE
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Job Type
	!	.x Job Type>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_TYPE/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_RPRT_TYPE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_TYPE.OBJ;*
	!
	! Author:
	!
	!	07/03/90 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP	(JC_TYPE)	JC_TYPE_CDD	JC_TYPE

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) From Type\*
	!	.b
	!	.lm +5
	!	The ^*From Type\* field
	!	enters a job type with which the report will begin printing.
	!	.b
	!	If this field is left blank, the report to begin with the first type in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Type>Job Type>Report
	!	.x Report>From Type>Job Type
	!	.x Job Type>Report>From Type
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.b
	!	.lm +5
	!	The ^*To Type\*
	!	enters a job type with which the report will end.
	!	.b
	!	If this field is left blank, the report will end with the last type in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Type>Job Type>Report
	!	.x Report>Job Type>To Type
	!	.x Job Type>Report>To Type
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	prints a report including selected types by entering a wildcard value.
	!	.b
	!	For more information on using the wildcard technique, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Type>Report
	!	.x Report>Job Type>Wildcard
	!	.x Job Type>Report>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
	USE
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Job Type Description"
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Type   Description"
	TITLE$(5%) = "."

	!
	! Layouts for printed line
	!
	LYT_LINE$ = "$TTYPE:006,$DESCR:038"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #JC_TYPE.CH%
		ELSE
			FIND #JC_TYPE.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #JC_TYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (JC_TYPE::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(JC_TYPE::TTYPE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ =  JC_TYPE::TTYPE	+ "     " + JC_TYPE::DESCR

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report JC_RPRT_TYPE
	!******************************************************************
	END
