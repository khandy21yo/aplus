1	%TITLE "Financial Statements Layout Report"
	%SBTTL "GL_RPRT_FINR01"
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
	! ID:FINR01
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Financial Statements Layout Report\* option prints
	!	a layout records listing. The following information is included:
	!	.table 30
	!	.te
	!	Prompt
	!	.te
	!	Description
	!	.te
	!	Report title fields
	!	.te
	!	Command file
	!	.te
	!	Type
	!	.te
	!	Input fields
	!	.end table
	!	.LM -5
	!	.lm -5
	!
	! Index:
	!	.x Report>Financial Layout File
	!	.x Report>Financial Layout File
	!	.X Print>Financial Layout File
	!	.x Report>Financial Layout File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_FINR01/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_FINR01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_FINR01.OBJ;*
	!
	! Author:
	!
	!	11/26/86 - Kevin Handy
	!
	! Modification history:
	!
	!	12/14/90 - Kevin Handy
	!		Fixed bug in OUP_LINE calls, where it was missing
	!		array reference.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
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
	!	09/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP	(GL_FINSTA)	GL_FINSTA_CDD	GL_FINSTA

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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Prompt\*
	!	.b
	!	.lm +5
	!	The purpose of the ^*From Prompt\* field is to provide a means
	!	to select a specific Financial Statement Layout record with which
	!	to begin the Financial Statement Layout Report.
	!	.b
	!	If the report is to begin with the first record in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(2) To Prompt\*
	!	.b
	!	.lm +5
	!	The purpose of the ^*To Prompt\* field is to provide a means to
	!	select a specific Financial Statement Layout record with which the
	!	Financial Statement Layout Report will end.
	!	.b
	!	If the report is to end with the last record in file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.OPN"
	USE
		FILENAME$ = "GL_FINSTA"
		CONTINUE HelpError
	END WHEN


	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Financial File Report"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "Prompt Description                    ReportTitle " + &
		"                             CommandFile          Type Input"
	TITLE$(5%) = ""

	!
	! Layout for printed lines
	!
	LYT_LINE$ = "$Prompt:006,$Descr:037,$RepTitle:078," + &
		"$CommFile:099,$FinType:101,$FinComm:125"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_FINSTA.CH%
		ELSE
			FIND #GL_FINSTA.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_FINSTA"
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
		GET #GL_FINSTA.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "GL_FINSTA"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (GL_FINSTA::PROMPT > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print out one line
	!
	TEXT$ = GL_FINSTA::PROMPT + " " + &
		GL_FINSTA::DESCR + " " + &
		LEFT(GL_FINSTA::REPTITLE, 40%) + " " + &
		LEFT(GL_FINSTA::CMDFIL, 20%) + " " + &
		GL_FINSTA::FINTYPE + "    " + &
		TRM$(GL_FINSTA::FINCMD(1%))

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 2% to 8%

		TEMP$ = EDIT$(GL_FINSTA::FINCMD(LOOP%), 140%)

		IF TEMP$ <> ""
		THEN
			TEXT$ = TRM$(SPACE$(105%) + TEMP$)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
	!
	! Finish up the report
	!
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
	! End of report GL_RPRT_FINR01
	!******************************************************************
	END
