1	%TITLE "Set List Description"
	%SBTTL "UTL_RPRT_SET"
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
	! ID:UT010
	!
	! Abstract:HELP
	!	.p
	!	The ^*Defaults and Allow Entries\* Report contains
	!	the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Program
	!	.le
	!	Item
	!	.le
	!	System
	!	.le
	!	Hard/Soft
	!	.le
	!	Data
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Defaults and Allow Entries
	!	.x Defaults and Allow Entries>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_SET/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_SET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_SET.OBJ;*
	!
	! AUTHOR:
	!
	!	01/18/88 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Ue integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/05/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	11/10/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET) UTL_SET_CDD UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%

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
	!	^*(01) From Program\*
	!	.p
	!	The ^*From Program\* field enters the
	!	program name of the inital program.
	!	.p
	!	A blank field causes the report to begin with the first
	!	program in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Program\*
	!	.p
	!	The ^*To Program\* field enters a program
	!	with which the report is to end.
	!	.p
	!	A blank setting causes the report to end with the
	!	last program in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated programs to be printed by entering a "wildcard"
	!	(for Wildcarding
	!	Technique.)
	!
	! Index:
	!
	!--


300	IF (UTL_SET.CH% = 0%)
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.OPN"
		USE
			FILENAME$ = "UTL_SET"
			CONTINUE HelpError
		END WHEN
	END IF

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "UTILITY  SET  REPORT"
	TITLE$(2%) = " Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(4%) = "Program           Item    System  HardSoft Data"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_SET.CH%
		ELSE
			FIND #UTL_SET.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
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
		GET #UTL_SET.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_SET"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_SET::PROGRAMNAME > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_SET::PROGRAMNAME, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17300	!
	! Print out one line
	!
	TEXT$ = left(UTL_SET::PROGRAMNAME, 16%) + "  " + &
		UTL_SET::ITEM + "  " + &
		UTL_SET::SYSTEM + "  " + &
		UTL_SET::HARD + "        " + &
		TRM$(UTL_SET::SDATA)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
