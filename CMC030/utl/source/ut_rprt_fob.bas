1	%TITLE "FOB Codes List"
	%SBTTL "UT_RPRT_FOB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:UT068
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*FOB Code List\* option provides a report
	!	which contains the following:
	!	.TABLE 3,25
	!	.TE
	!	FOB Code
	!	.Te
	!	Description
	!	.eND TABLE
	!	.LM -5
	!
	! Index:
	!	.x Report>FOB Code
	!	.x FOB Code>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_RPRT_FOB/LINE
	!	$ LINK/EXE=UTL_EXE: UT_RPRT_FOB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_RPRT_FOB.OBJ;*
	!
	! Author:
	!
	!	05/31/90 - Aaron Redd
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
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.HB"
	MAP	(UTL_FOB)	UTL_FOB_CDD	UTL_FOB

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD
	DECLARE LONG LOOP

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
	!	FOB code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>FOB Code
	!	.x FOB Code>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	A ^*To Item\* value causes the
	!	printing to end with a selected FOB code.
	!	.b
	!	A blank field will cause the report to end with the last
	!	FOB code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>FOB Code
	!	.x FOB Code>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	FOB codes to be printed by entering a
	!	"wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x FOB Code>Wildcard
	!	.x Wildcard>FOB Code
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.OPN"
	USE
		FILENAME$ = "UTL_FOB"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "FOB CODE LIST"
	TITLE$(2%) = "Utility System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "FOB Code      Description"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	LOOP = 0

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_FOB.CH%
		ELSE
			FIND #UTL_FOB.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_FOB"
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
		GET #UTL_FOB.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_FOB"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_FOB::FOBCODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_FOB::FOBCODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = UTL_FOB::FOBCODE + SPACE$(12%) + UTL_FOB::DESCR

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
