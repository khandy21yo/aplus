1	%TITLE "Job Class Report"
	%SBTTL "JC_RPRT_CLASS"
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
	! ID:JC0004
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Job Class Report\* option prints a list of the
	!	Job Class File.  The list contains the following fields:
	!	.table 3,15
	!	.te
	!	Class
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Job Class>Report
	!	.x Report>Job Class
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_CLASS/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_RPRT_CLASS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_CLASS.OBJ;*
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
	!	11/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP	(JC_CLASS)	JC_CLASS_CDD	JC_CLASS

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
	!	^*(01) From Class\*
	!	.lm +5
	!	.b
	!	The ^*From Class\* field
	!	enters a class code with which the report will begin printing.
	!	.b
	!	If this field is left blank, the report will begin with the first record in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Class>Job Class>Report
	!	.x Job Class>Report>From Class
	!	.x Report>Job Class>From Class
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Class\*
	!	.lm +5
	!	.b
	!	The ^*To Class\* field
	!	enters a job class code with which the report will end.
	!	.b
	!	If this field is left blank, the report will end with the last
	!	class in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Class>Job Class>Report
	!	.x Report>Job Class>To Class
	!	.x Job Class>Report>To Class
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.lm +5
	!	.b
	!	The ^*Wildcard\* field
	!	prints a report including selected classes by entering a wildcard
	!	value.
	!	.b
	!	For more information in reference to wildcard technique, see Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Class>Report
	!	.x Report>Job Class>Wildcard
	!	.x Job Class>Report>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
	USE
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Job Class Description"
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Class   Description"
	TITLE$(5%) = "."

	!
	! Layouts for printed line
	!
	LYT_LINE$ = "$CLASS:007,$DESCR:039"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #JC_CLASS.CH%
		ELSE
			FIND #JC_CLASS.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "JC_CLASS"
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
		GET #JC_CLASS.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (JC_CLASS::CLASS > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(JC_CLASS::CLASS, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = JC_CLASS::CLASS	+ "    " + JC_CLASS::DESCR

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
	! End of report JC_RPRT_CLASS
	!******************************************************************
	END
