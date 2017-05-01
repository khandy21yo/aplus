1	%TITLE "Reason Code Account Report"
	%SBTTL "OE_RPRT_REASONACCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:OE027
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Reason Code Account\* Report contains columns for
	!	the following information:
	!	.table 3,25
	!	.te
	!	Reason Code
	!	.te
	!	Reason Description
	!	.te
	!	Location
	!	.te
	!	Location Description
	!	.te
	!	GL Account
	!	.te
	!	Account Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Reason Code Account Report
	!	.x Reason Code Account Report>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REASONACCT/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REASONACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REASONACCT.OBJ;*
	!
	! Author:
	!
	!	04/15/92 - Dan Perkins
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	MAP (OE_REASONACCT)	OE_REASONACCT_CDD	OE_REASONACCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART

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
	!	^*(01) From Reason Code\*
	!	.b
	!	.lm +5
	!	The ^*From Reason Code\* field enters a reason
	!	code from which the report is to begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	reason code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Reason Code\*
	!	.b
	!	.lm +5
	!	The ^*To Reason Code\* field enters a reason code
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	reason code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field selects
	!	designated locations to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.OPN"
	USE
		FILENAME$ = "OE_REASONACCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "REASON CODE ACCOUNT REPORT"
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Reason Description                    " + &
		"Location Description                    " + &
		"GL Account         Description "

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_REASONACCT.CH%
		ELSE
			FIND #OE_REASONACCT.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REASONACCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Product record
	!
17020	WHEN ERROR IN
		GET #OE_REASONACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_REASONACCT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF (OE_REASONACCT::CREASON > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REASONACCT::LOCATION, -1%), &
		WLDCRD$) = 0% AND WLDCRD$ <> ""

	!
	! Get REASON description
	!
17100	IF INSTR(1%, OE_REASONACCT::CREASON, "?") > 0%
	THEN
		OE_CREASON::DESCR = "Reason Overlay Mask"
	ELSE
		OE_CREASON::DESCR = ""

		WHEN ERROR IN
			GET #OE_CREASON.CH%, &
				KEY #0% EQ OE_REASONACCT::CREASON, &
				REGARDLESS
		USE
			CONTINUE L17020 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_CREASON"
			CONTINUE HelpError
		END WHEN
	END IF

 L17020:
	!
	! Get LOCATION description
	!
	V% = UTL_EXAM_LOCATION(OE_REASONACCT::LOCATION, UTL_LOCATION_EXAM)

	!
	! Get ACCOUNT description
	!
	V% = GL_EXAM_CHART(OE_REASONACCT::ACCOUNT, GL_CHART_EXAM)

	TEXT$ = OE_REASONACCT::CREASON + "     " + &
		LEFT(OE_CREASON::DESCR, 30%) + " "     + &
		OE_REASONACCT::LOCATION + "     " + &
		LEFT(UTL_LOCATION_EXAM::LOCNAME, 30%) + " "     + &
		OE_REASONACCT::ACCOUNT + " "     + &
		LEFT(GL_CHART_EXAM::DESCR, 30%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

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
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
