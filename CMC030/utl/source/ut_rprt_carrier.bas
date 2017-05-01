1	%TITLE "Carrier List"
	%SBTTL "UT_RPRT_CARRIER"
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
	! ID:UT090
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Carrier List\* option provides a report
	!	which contains the following:
	!	.table 3,25
	!	.te
	!	Carrier Code
	!	.te
	!	Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Carrier
	!	.x Carrier>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_RPRT_CARRIER/LINE
	!	$ LINK/EXE=UTL_EXE: UT_RPRT_CARRIER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_RPRT_CARRIER.OBJ;*
	!
	! Author:
	!
	!	03/29/90 - Kevin Handy
	!
	! Modification History:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message
	!
	!	12/04/91 - Kevin Handy
	!		Removed garbage characters so program could compile.
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
	!	06/06/97 -  Kevin Handy
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

	!++
	! Abstract:COMMAND
	!	^*CARRIER/LIST\*
	!	.b
	!	.lm +5
	!	Allows printing of the Carrier code definition file.
	!	.b
	!	^*Format: CARRIER/LIST\*
	!	.lm -5
	!
	! Index:
	!	.x CARRIER/LIST
	!	.Y CARRIER
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER) UTL_CARRIER_CDD UTL_CARRIER

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
	!	selected carrier.
	!	.b
	!	A blank field begin printing with the first
	!	carrier code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Carrier
	!	.x Carrier>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	A ^*To Item\* value causes the
	!	printing to end with a selected carrier.
	!	.b
	!	A blank field will end printing with the last
	!	carrier code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Carrier
	!	.x Carrier>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	carrier codes to be printed by entering a
	!	"wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Carrier>Wildcard
	!	.x Wildcard>Carrier
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.OPN"
	USE
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Carrier List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Carrier       Description"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #UTL_CARRIER.CH%
		ELSE
			FIND #UTL_CARRIER.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_CARRIER"
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
		GET #UTL_CARRIER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (UTL_CARRIER::CODE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(UTL_CARRIER::CODE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = UTL_CARRIER::CODE + SPACE$(11%) + " " + &
		UTL_CARRIER::DESCR

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
