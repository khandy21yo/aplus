1	%TITLE "Terms"
	%SBTTL "UT_RPRT_TERMS"
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
	! ID:UT091
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Terms List\* option provides a report
	!	which contains the following:
	!	.table 3,25
	!	.te
	!	Terms Code
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Terms
	!	.x Terms>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_RPRT_TERMS/LINE
	!	$ LINK/EXE=UTL_EXE: UT_RPRT_TERMS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_RPRT_TERMS.OBJ;*
	!
	! Author:
	!
	!	03/29/90 - Kevin Handy
	!
	! Modification History:
	!
	!	06/17/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose unnecessary LOOP variable
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*TERMS/LIST\*
	!	.b
	!	.lm +5
	!	The Terms Report option prints
	!	the terms codes and the
	!	description table.
	!	.lm -5
	!
	! Index:
	!	.x TERMS/LIST
	!	.x Report>Terms
	!	.x Terms>Report
	!
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS) UTL_TERMS_CDD UTL_TERMS

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	A ^*From Item\* value causes the
	!	printing to begin with a
	!	selected code.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	terms code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Terms
	!	.x Terms>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* value causes the
	!	printing to end with a selected terms
	!	code.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Terms code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Terms
	!	.x Terms>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	Terms codes to be printed by entering a
	!	"wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Terms>Wildcard
	!	.x Wildcard>Terms
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Terms Code List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Terms         Description"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_TERMS.CH%
		ELSE
			FIND #UTL_TERMS.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_TERMS"
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
		GET #UTL_TERMS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_TERMS::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_TERMS::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = UTL_TERMS::CODE + SPACE$(11%) + " " + &
		UTL_TERMS::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
