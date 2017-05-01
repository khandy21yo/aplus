1	%TITLE "Print Transaction Account Table"
	%SBTTL "UTL_RPRT_TRANSACCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:UT017
	!
	! Abstract:HELP
	!	.p
	!	The ^*Transaction Accounts\* option provides a report
	!	which contains the following:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Location
	!	.le
	!	Name
	!	.le
	!	Type
	!	.le
	!	Description
	!	.le
	!	Account
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Transaction Accounts
	!	.x Transaction Accounts>Report
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_TRANSACCT/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_TRANSACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_TRANSACCT.OBJ;*
	!
	! AUTHOR:
	!
	!	06/09/2000 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	07/13/2000 - Kevin Handy
	!		Lose UNSOLICITED checks (OUTP_LINE handles this
	!		stuff already doesn't it?)
	!		Do a better job on the wildcard checks.
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"
	MAP (UTL_TRANSACCT) UTL_TRANSACCT_CDD UTL_TRANSACCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)	GL_CHART_CDD	GL_CHART

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
	!	^*From Location\*
	!	.p
	!	A ^*From Location\* value causes
	!	the printing to begin with the selected transaction
	!	account.
	!	.p
	!	A blank setting will cause the report to begin with the frst
	!	transaction account in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Location\*
	!	.p
	!	A ^*To Location\* value causes the
	!	printing to end with the selected transaction
	!	account.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	transaction account in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	transaction accounts to be printed by entering a
	!	"wildcard" value.
	!
	! Index:
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.OPN"
	USE
		FILENAME$ = "UTL_TRANSACCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "TRANSACTION  ACCOUNTS  TABLE"
	TITLE$(2%) = "Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		'1234567890123456789012345678901234567890
	TITLE$(4%) = "Loc  Name                              " + &
		"       Type Description          Acco" + &
		"unt            Description"
	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_TRANSACCT.CH%
		ELSE
			FIND #UTL_TRANSACCT.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
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
		GET #UTL_TRANSACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_TRANSACCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	IF TO_ITEM$ <> ""
	THEN
		GOTO ExitTotal IF (UTL_TRANSACCT::LOCATION > TO_ITEM$)
	END IF

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(UTL_TRANSACCT::LOCATION, -1%), &
			WLDCRD$) = 0%
	END IF

	GOSUB 18500

17300	!
	! Print out one line
	!
	ACCOUNT$ = UTL_TRANSACCT::ACCOUNT
	GOSUB 18550

	TEXT$ = UTL_TRANSACCT::LOCATION + " " + &
		UTL_LOCATION::LOCNAME + " " + &
		UTL_TRANSACCT::TRANSTYPE + "   " + &
		UTL_TRANSTYPE::DESCRIPTION + " " + &
		UTL_TRANSACCT::ACCOUNT + " " + &
		GL_CHART::DESCR

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

18500	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_TRANSACCT::LOCATION, &
			REGARDLESS
	USE
		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
		CONTINUE 18510 IF ERR = 155%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

18510	WHEN ERROR IN
		GET #UTL_TRANSTYPE.CH%, &
			KEY #0% EQ UTL_TRANSACCT::TRANSTYPE, &
			REGARDLESS
	USE
		UTL_TRANSTYPE::DESCRIPTION = &
			STRING$(LEN(UTL_TRANSTYPE::DESCRIPTION), A"?"B)
		CONTINUE EndType IF ERR = 155%
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

 EndType:
	RETURN

18550	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ ACCOUNT$, REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)
		CONTINUE Account IF ERR = 155%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 Account:
	RETURN

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
