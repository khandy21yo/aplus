1	%TITLE "Accounts Receivable Sales Tax Report"
	%SBTTL "AR_RPRT_SALTAX"
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
	! ID:AR053
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Tax\* report option prints
	!	a report which contains the following information:
	!	.te
	!	.table 3,25
	!	Country
	!	.te
	!	State
	!	.te
	!	County
	!	.te
	!	Sales Tax Percentage
	!	.te
	!	Account
	!	.te
	!	Description
	!	.end table
	!
	! Index:
	!	.x Report>Sales Tax Table
	!	.x Sales Tax>Print Table
	!	.x Print>Sales Tax Table
	!
	! Option:
	!
	! Author:
	!
	!	03/10/88 - Aaron Redd
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_SALTAX.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_SALTAX, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_SALTAX.OBJ;*
	!
	! Modification history:
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAX.HB"
	MAP	(AR_SALTAX)	AR_SALTAX_CDD	AR_SALTAX

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

300	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAX.OPN"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Sales Tax File List"
	TITLE$(2%) = "Accounts Receivable System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Country   State   County   Sales Tax Percentage" + &
		"   Account              Description"
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$CountryCode:002,$StateCode:012,$CountyCode:020," + &
		"VPercentSalesTax:036,$Account:068,$AcctDescr:111"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_SALTAX.CH%
		ELSE
			FIND #AR_SALTAX.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
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
		GET #AR_SALTAX.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram &
		IF ((AR_SALTAX::COUNTRY + AR_SALTAX::STATE + &
		AR_SALTAX::COUNTY) > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Get description for GL_CHART account
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ AR_SALTAX::ACCOUNT, REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

	!
	! Print the Sales Tax line
	!
	TEXT$ = AR_SALTAX::COUNTRY + "        " + &
		AR_SALTAX::STATE + "      " + &
		AR_SALTAX::COUNTY + "       " + &
		FORMAT$(AR_SALTAX::PERCENT, "#####.###") + "              " + &
		AR_SALTAX::ACCOUNT + "   " + &
		GL_CHART::DESCR

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try for next record
	!
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
