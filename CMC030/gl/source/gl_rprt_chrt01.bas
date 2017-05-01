1	%TITLE "Print Financial Codes"
	%SBTTL "GL_RPRT_CHRT01"
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
	! ID:CHR001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Financial Codes\* option prints the Chart
	!	of Accounts, including the values contained in the various financial code
	!	fields.
	!	.b
	!	The report contains the following:
	!	.table 30
	!	.te
	!	Account [Number]
	!	.te
	!	Description
	!	.te
	!	Financial [Code]
	!	.te
	!	Type [Code]
	!	.te
	!	Cash Flow [Code]
	!	.te
	!	Work[ing Capital Code]
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x Reports>Chart of Accounts>Financial Codes
	!	.x Print>Chart of Accounts>Financial Codes
	!	.x Chart of Accounts>Print Financial Codes
	!	.x Financial Codes>Print
	!	.x Print>Financial Codes
	!	.x Financial Codes>Print
	!	.x Chart of Account>Financial Codes Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_CHRT01/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_CHRT01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_CHRT01.OBJ;*
	!
	! Author:
	!
	!	11/07/86 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/21/91 - Kevin Handy
	!		Modified so that header will print.
	!
	!	05/21/91 - Kevin Handy
	!		Added Sortby field/capability.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Account\*
	!	.b
	!	.lm +5
	!	The ^*From Account\* field causes
	!	printing to begin with a specified
	!	account number.
	!	.b
	!	If the report is to start with the first account number in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.b
	!	.lm +5
	!	The ^*To Account\* field causes
	!	printing to end with a specified
	!	account number.
	!	.b
	!	If the report is to end with the last account number in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a
	!	report including selected accounts only, using the wildcard
	!	technique.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99 and
	!	the two rightmost numbers represented a department number and a
	!	report were to be printed to include department "02" only, the
	!	Wildcard setting of "?????-02" (or "_*-02") would cause the report to print
	!	all accounts with the suffix 02.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Chart Codes
	!	.x Chart Codes>Wildcard
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Sort by>Chart Codes
	!	^*(04) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines if the
	!	report is to be printed by Account Number,
	!	Balance/Income, Cash Flow, or Work Capital.
	!	.b
	!	An entry is required in this field.  Valid entries are:
	!	.table 3,25
	!	.te
	!	^*A\*	- Account Number
	!	.te
	!	^*B\*	- Balance/Income
	!	.te
	!	^*C\*	- Cash Flow
	!	.te
	!	^*W\*	- Work Capital
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Chart Codes>Sort by
	!
	!--

	SELECT SORTBY$
	CASE "B"
		K_NUM% = 3%
		K_NAM$ = "Balance/Income"
	CASE "C"
		K_NUM% = 1%
		K_NAM$ = "Cash Flow"
	CASE "W"
		K_NUM% = 2%
		K_NAM$ = "Work Capitol"
	CASE ELSE
		K_NUM% = 0%
		K_NAM$ = "Account"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Chart Financial Codes"
	TITLE$(2%) = "Printed in " + K_NAM$ + " Order"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Account            Description             " + &
		"                 Bal/Inc    Cash Work"
	TITLE$(5%) = ""

	!
	! Layouts for printed line
	!
	LYT_LINE$ = "$Account:018,$Descr:059,$FinType:070," + &
		"$CashFlow:075,$WorkCap:080"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_CHART.CH%, KEY #K_NUM%
		ELSE
			FIND #GL_CHART.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_CHART"
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
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	IF TO_ITEM$ <> ""
	THEN
		SELECT K_NUM%

		CASE 0%
			GOTO ExitTotal IF (GL_CHART::ACCT > TO_ITEM$)
		CASE 1%
			GOTO ExitTotal IF (GL_CHART::FLOW > TO_ITEM$)
		CASE 2%
			GOTO ExitTotal IF (GL_CHART::WORK > TO_ITEM$)
		CASE 3%
			GOTO ExitTotal IF (GL_CHART::FINTYPE > TO_ITEM$)
		END SELECT
	END IF

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = GL_CHART::ACCT + " " + &
		GL_CHART::DESCR + " " + &
		GL_CHART::FINTYPE + " " + &
		GL_CHART::FLOW + " " + &
		GL_CHART::WORK

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

	!******************************************************************
	! End of report GL_RPRT_CHRT01
	!******************************************************************
	END
