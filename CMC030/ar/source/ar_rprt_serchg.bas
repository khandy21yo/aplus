1	%TITLE "Accounts Receivable Service Charge Report"
	%SBTTL "AR_RPRT_SERCHG"
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
	! ID:AR052
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Service Charge\* report provides a report which contains
	!	the following information from the Service Charge Table:
	!	.te
	!	.table 3,25
	!	Country
	!	.te
	!	State
	!	.te
	!	Account
	!	.te
	!	Revenue Account
	!	.te
	!	% Service
	!	.te
	!	Min. Charge
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Print Table
	!	.x Report>Service Charge Table
	!	.x Print>Service Charge Table
	!
	! Option:
	!
	! Author:
	!
	!	02/29/88 - Aaron Redd
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_SERCHG.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_SERCHG, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_SERCHG.OBJ;*
	!
	! Modification history:
	!
	!	03/08/88 - Kevin Handy
	!		Played around with file layout.
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	07/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.HB"
	MAP	(AR_SERCHG)	AR_SERCHG_CDD	AR_SERCHG

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.OPN"
	USE
		FILENAME$ = "AR_SERCHG"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Service Charge File List"
	TITLE$(2%) = "Accounts Receivable System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Country  State  Account            " + &
		"Rev Account         %Service    Min. Chg.  Dollar Amt."
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$CountryCode:002,$StateCode:011,$AccountNum:034," + &
		"$RevAccount:054,VServiceCharge:063,VMinimum:076,VDollar:089"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_SERCHG.CH%
		ELSE
			FIND #AR_SERCHG.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal
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
		GET #AR_SERCHG.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_SERCHG::COUNTRY > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Print the cash receipt line
	!
	TEXT$ = AR_SERCHG::COUNTRY + "       " + &
		AR_SERCHG::STATE + "     " + &
		AR_SERCHG::ACCT + "  " + &
		AR_SERCHG::SCREV + "  " + &
		FORMAT$(AR_SERCHG::SERCHG, "###.###") + "  " + &
		FORMAT$(AR_SERCHG::MINIMUM, "########.##") + "  " + &
		FORMAT$(AR_SERCHG::DOLLAR, "########.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try for next record
	!
	GOTO GetNextRec

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
