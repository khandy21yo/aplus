1	%TITLE "Print Chart of Accounts"
	%SBTTL "GL_RPRT_CHRT02"
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
	! ID:CHR002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Chart of Accounts\* option
	!	lists the Account Number and Account Description of the records in
	!	the Chart of Accounts file. At the user's option, year-to-date balances
	!	or period transaction amounts related to each account, as of, or for
	!	any specified period, may also be printed on a Chart of Accounts
	!	report. The following fields are included in this report:
	!	.table 30
	!	.te
	!	Account
	!	.te
	!	Description
	!	.te
	!	Type
	!	.te
	!	Cash Flow
	!	.te
	!	Work Capital
	!	.te
	!	Balance/Income Code
	!	.te
	!	Summary Flag
	!	.te
	!	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Chart of Accounts>Print
	!	.x Chart of Accounts>Report
	!	.x Print>Chart of Accounts
	!	.x Report>Chart of Accounts
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_CHRT02/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_CHRT02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_CHRT02.OBJ;*
	!
	! Author:
	!
	!	05/24/86 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/05/2001 - Kevin Handy
	!		Sort by description (kbj)
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Declare some variables
	!
	DECLARE	REAL	THIS_AMOUNT, GRAND_TOTAL
	DECLARE	STRING	PRNT_USING

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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines
	!	whether the report will be sorted by Account Number,
	!	Cash Code, Work Capital Code or by Balance/Income Code.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*A\* - Account Number
	!	.te
	!	^*C\* - Cash Code
	!	.te
	!	^*W\* - Work Capital
	!	.te
	!	^*B\* - Balance/Income Code
	!	.te
	!	*D - Description
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes
	!	printing to begin with a specified
	!	item.  The value entered must be in agreement with the value in field
	!	(01) Sort by.
	!	.b
	!	If the report is to start with the first item in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	printing to end with a specified
	!	item.  The value entered must be in agreement with the value in field
	!	(01) Sort by.
	!	.b
	!	If the report is to end with the last item in the file, this field
	!	should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected accounts only, using the wildcard
	!	technique.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99 and
	!	the two rightmost numbers represented a department number and a
	!	report were to be printed to include department "02" only, the
	!	Wildcard setting of "?????-02" would cause the report to print
	!	all accounts with the suffix 02.
	!	.lm -5
	!
	! Index:
	!
	!--

	PBN$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^*(06) Year/Period\*
	!	.b
	!	.lm +5
	!	The ^*Year/Period\* field must contain valid year and period information
	!	if field (05), Period/Balance/None, is set to ^*P\* or ^*B\*. If field
	!	(05) is set to ^*N\*, the ^*Year/Period\* field may be left blank.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Year/Period
	!
	!--

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Period/Balance/None\*
	!	.b
	!	.lm +5
	!	The ^*Period/Balance/None\* field selects information
	!	other than the Account Number and Account Description.
	!	.b
	!	Valid settings are ^*P\*, ^*B\*, or ^*N\* in either upper or lower case.
	!	The meaning and function of each setting is as follows:
	!	.b
	!	.lm +5
	!	^*P\*#-#Period
	!	.break
	!	This setting will cause the sum of the transactions for each account for the
	!	year and period specified in field (04) Year/Period to be printed.
	!	.b
	!	^*B\*#-#Balance
	!	.break
	!	This setting will cause the balance in each account as of the end of the period
	!	specified in field (04) Year/Period to be printed.
	!	.b
	!	^*N\*#-#None
	!	.break
	!	This setting will cause the report to include the Account Number and Account
	!	Description only. No dollar information will be displayed.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRNT_USING = SPACE$(67%) + "  Grand Total:  ###,###,###,###.##"

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Check status of CLOSEFLAG
	!
	SELECT GL_PERIOD::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "GL Close in process", &
			"ERR", "GL_CLOSE", &
			"ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "GL Reset in process", &
			"ERR", "GL_RESET", &
			"ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE ELSE
		IF PBN$ <> "N"
		THEN
			!
			! Calculate array item containing correct period
			! to be displayed.
			!
			IF YYYYPP$ = ""
			THEN
				!
				! Default MMYY if not given
				!
				YYYYPP$ = GL_PERIOD::YEAR + &
					FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")
			END IF

			FIRST_PERIOD% = VAL%(GL_PERIOD::YEAR) * &
				GL_PERIOD::FPFY + GL_PERIOD::LASTPERCLO

			ARRAY_ITEM% = FIRST_PERIOD% - &
				(VAL%(LEFT(YYYYPP$, 4%)) * GL_PERIOD::FPFY + &
				VAL%(MID(YYYYPP$, 5%, 2%)))

			!
			! Force the period to exist within the file
			!
			IF (ARRAY_ITEM% < 0%) OR (ARRAY_ITEM% > 20%)
			THEN
				YYYYPP1$ = GL_PERIOD::YEAR + &
					FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")
				CALL ENTR_3MESSAGE(SCOPE, "Period " + &
					YYYYPP$ + &
					" is not in file! Assuming " + &
					YYYYPP1$ + "!", 0%)

				YYYYPP$ = YYYYPP1$
				ARRAY_ITEM% = 0%
			END IF

			CURRENT_PERIOD% = VAL%( MID(YYYYPP$, 5%, 2%))
		END IF

	END SELECT

	!
	! Open GL Chart of Accounts file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Select which method to sort by.
	!
	SELECT SORTBY$
	CASE "C"
		K_NUM% = 1%
		TITLE1$ = "Chart of Accounts by Cash Code"
	CASE "W"
		K_NUM% = 2%
		TITLE1$ = "Chart of  Accounts by Work Capital"
	CASE "B"
		K_NUM% = 3%
		TITLE1$ = "Chart of  Accounts by Bal/Inc Code"
	CASE "D"
		TITLE1$ = "Chart of  Accounts by Description"
		K_NUM% = 0%
		CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

		CALL ASSG_CHANNEL(GL_TEMP.CH%, STAT%)
		CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

		WHEN ERROR IN
			OPEN UTL_WORK.DEV$ + "GL_TEMP.TMP" &
				FOR OUTPUT AS FILE GL_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 64%, &
				MAP GL_CHART, &
				PRIMARY KEY &
				( &
					GL_CHART::DESCR, &
					GL_CHART::ACCT &
				), &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

330		WHEN ERROR IN
			RESET #GL_CHART.CH%
		USE
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

 GetChartRec:
		WHEN ERROR IN
			GET #GL_CHART.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CLOSE GL_CHART.CH%
				GL_CHART.CH% = GL_TEMP.CH%
				CONTINUE ReportTitle
			END IF
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		PUT #GL_TEMP.CH%

		GOTO GetChartRec
	CASE ELSE	! "A"
		K_NUM% = 0%
		TITLE1$ = "Chart of Accounts by Account Number"
	END SELECT

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = TITLE1$
	TITLE$(2%) = "For Period " + &
		TRM$(GL_PERIOD::PERIOD(VAL%(MID(YYYYPP$, 5%, 2%)))) + &
		" in Year " + LEFT(YYYYPP$, 4%)
	TITLE$(2%) = PRNT_FANCYDATE(DATE_TODAY) IF INSTR(1%, " PB", PBN$) <= 1%
	TITLE$(3%) = ""

	!
	! Headings
	!
	TITLE$(4%) = "Account            Description               " + &
		"             Type  Flow Work Bal-Code  "
	TITLE$(4%) = TITLE$(4%) + "  Period-Amount" IF PBN$ = "P"
	TITLE$(4%) = TITLE$(4%) + "        Balance" IF PBN$ = "B"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$Account:018,$Descr:059,$AccType:061," + &
		"$Summary:065,VAmount:080"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_CHART.CH%, &
				KEY #K_NUM%
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
		CONTINUE 17900 IF ERR = 11%
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
	SELECT SORTBY$
	CASE "A"
		GOTO 17900 IF (GL_CHART::ACCT > TO_ITEM$) AND (TO_ITEM$ <> "")

		GOTO GetNextRec &
			IF (COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), &
			WLDCRD$) = 0%) AND (WLDCRD$ <> "")

	CASE "C"
		GOTO 17900 IF (GL_CHART::FLOW > TO_ITEM$) AND (TO_ITEM$ <> "")

		GOTO GetNextRec &
			IF (COMP_STRING(EDIT$(GL_CHART::FLOW, -1%), &
			WLDCRD$) = 0%) AND (WLDCRD$ <> "")

	CASE "W"
		GOTO 17900 IF (GL_CHART::WORK > TO_ITEM$) AND (TO_ITEM$ <> "")

		GOTO GetNextRec IF (COMP_STRING(EDIT$(GL_CHART::WORK, -1%), &
			WLDCRD$) = 0%) AND (WLDCRD$ <> "")

	CASE "B"
		GOTO 17900 &
			IF (GL_CHART::FINTYPE > TO_ITEM$) AND (TO_ITEM$ <> "")

		GOTO GetNextRec &
			IF (COMP_STRING(EDIT$(GL_CHART::FINTYPE, -1%), &
			WLDCRD$) = 0%) AND (WLDCRD$ <> "")

	END SELECT


	!
	! Calculate current information
	!
	THIS_AMOUNT = FUNC_ROUND(GL_CHART::DOLLAR(ARRAY_ITEM%), 2%)

	IF CURRENT_PERIOD% <> 1% OR INSTR(1%, "RE", GL_CHART::ACCTYPE) = 0%
	THEN
		THIS_AMOUNT = THIS_AMOUNT - &
			GL_CHART::DOLLAR(ARRAY_ITEM% + 1%) &
			IF PBN$ = "P"
	END IF

	GRAND_TOTAL = GRAND_TOTAL + THIS_AMOUNT

	!
	! Print out one line
	!
	TEXT$ = GL_CHART::ACCT + " " + &
		GL_CHART::DESCR	+ " " + &
		GL_CHART::ACCTYPE + "   " + &
		GL_CHART::FLOW + " " + &
		GL_CHART::WORK + " " + &
		GL_CHART::FINTYPE + " " + &
		GL_CHART::SUMMARY

	TEXT$ = TEXT$ + " " + &
		FORMAT$(THIS_AMOUNT, "###,###,###.##") &
		IF INSTR(1%, " PB", PBN$) > 1%

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Was this period closed correctly?
	!
	IF (GL_CHART::CPERIOD <> GL_PERIOD::LASTPERCLO) AND &
		(GL_CHART::CPERIOD <> 0%)
	THEN
		TEXT$ = "     The above account was not properly closed!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17900	!******************************************************************
	! Handle end of report
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	IF INSTR(1%, " PB", PBN$) > 1%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = FORMAT$(GRAND_TOTAL, PRNT_USING)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

 ExitProgram:
	!
	! Finish up the report
	!
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

32767	!******************************************************************
	! End of report GL_RPRT_CHRT02
	!******************************************************************
	END
