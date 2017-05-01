1	%TITLE "Print By Batch Number"
	%SBTTL "AR_RPRT_BYBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:AR024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print By Batch Number\* report prints a listing
	!	of the customer accounts by batch number. The batch numbers are system assigned
	!	during the posting process and are used when searching for a specific posting
	!	group.
	!	.lm -5
	!
	! Index:
	!	.x Print>By Batch Number
	!	.x Report>Print By Batch Number
	!
	! Option:
	!
	! Author:
	!
	!	05/08/89 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_BYBATCH/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_BYBATCH, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_BYBATCH.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Open AR_CONTROL as .OPN instead of .MOD.
	!
	!	06/19/96 - Kevin Handy
	!		Add new transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) From Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*From Batch _#\* field causes the printing
	!	to begin with the selected Batch _#.
	!	.b
	!	A blank setting will cause the report to begin with the first Batch _# in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Batch _#>Report by Batch _#
	!	.x Report by Batch _#>From Batch _#
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*To Batch _#\* field causes the printing
	!	to end with a selected Batch _#.
	!	.b
	!	A blank field will cause the report to end with the last Batch _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Batch _#>Report by Batch _#
	!	.x Report by Batch _#>To Batch _#
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

330	!
	! Open AR Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Receivable by Batch"
	TITLE$(2%) = ""
	TITLE% = 3%

	!
	! Display Heading
	!
	TITLE$(TITLE%) = LEFT(AR_CONTROL::CTITLE, 10%) + "  Name"

	TITLE$(TITLE% + 1%) = "    " + &
		"CUSNUM     " + &
		"INVNUM   " + &
		"TY " + &
		"TRADAT     " + &
		"   SALAMT " + &
		"RECNUM   " + &
		"CHKNUM " + &
		"ARACCT             " + &
		"DESCR                     " + &
		"UPDATED"


	TITLE$(TITLE% + 2%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	THIS_BATCH$ = "12345678901234567890"	! Impossible

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_OPEN.CH%, KEY #1%
		ELSE
			FIND #AR_OPEN.CH%, &
				KEY #1% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
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
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_OPEN::BATCH > TO_ITEM$) AND &
		TO_ITEM$ <> ""

17030	!
	! Dump out one line
	!
	IF (THIS_BATCH$ <> AR_OPEN::BATCH)
	THEN
		TEXT$ = "Batch #" + AR_OPEN::BATCH
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		THIS_BATCH$ = AR_OPEN::BATCH
	END IF

	TEXT$ = "    " + &
		AR_OPEN::CUSNUM + " " + &
		AR_OPEN::INVNUM + " " + &
		AR_OPEN::TRATYP + " " + &
		PRNT_DATE(AR_OPEN::TRADAT, 8%) + " " + &
		FORMAT$(AR_OPEN::SALAMT, "######.##") + " " + &
		AR_OPEN::RECNUM + " " + &
		AR_OPEN::CHKNUM + " " + &
		AR_OPEN::ARACCT + " " + &
		AR_OPEN::DESCR + " " + &
		AR_OPEN::UPDATED

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17020

 ExitTotal:
	GOTO ExitProgram

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
