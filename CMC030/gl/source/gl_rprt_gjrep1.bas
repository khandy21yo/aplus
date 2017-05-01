1	%TITLE "General Journal Report (Alternate)"
	%SBTTL "GL_RPRT_GJREP1"
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
	! ID:GJREP1
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Alternate Print Journal\* routine provides an alternate means to
	!	print a report containing only a specific batch of data in journal
	!	number sequence. A final version of a journal file, in regular or
	!	alternate form, must be printed before the system will allow the
	!	journal file to be posted.
	!	.b
	!	The following fields are included:
	!	.table 30
	!	.te
	!	Item
	!	.te
	!	Account Number
	!	.te
	!	Account Name
	!	.te
	!	Source
	!	.te
	!	Description
	!	.te
	!	Transaction Date
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.te
	!	Units
	!	.te
	!	Hours
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.X Print>Journal>Alternate
	!	.X Journal>Alternate>Print
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_GJREP1/LINE
	!	$ LINK/EXE=GL_EXE: GL_RPRT_GJREP1, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_GJREP1.OBJ;*
	!
	! Author:
	!
	!	11/21/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/05/90 - Kevin Handy
	!		Modified for Recurring/Reversing Journal (4).
	!
	!	06/10/91 - Craig Tanner
	!		Changed so that Debit/Credit page done by
	!		GL_OUTP_ACCTSUM.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
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

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD	GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP	(GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD UTL_REPORTX

	!
	! Declare some variables
	!
	DECLARE	REAL	REVERSING
	DECLARE	STRING	UNDEF_ACCT_FLAG, CURRENT_JRL

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.FIELD
	!	^*(01) From Journal _#\*
	!	.b
	!	The ^*From Journal _#\* field causes printing
	!	to begin with a specified journal number.
	!	.b
	!	If the report is to begin with the first journal number in the
	!	file, the field should be left blank.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	.FIELD
	!	^*(02) To Journal _#\*
	!	.b
	!	The ^*To Journal _#\* field causes the report to
	!	end printing with a specified journal number.
	!	.b
	!	If the report is to end with the last journal number in the file,
	!	the field should be left blank.
	!
	! Index:
	!
	! Required:
	!--

	JRL_TYPE$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), 132), 1%)

	!++
	! Abstract:FLD03
	!	.FIELD
	!	^*(03) Journal Type\*
	!	.b
	!	The ^*Journal Type\* field is used to identify which type of
	!	general journal is to be printed -- Regular, Recurring, Reversing,
	!	Recurring/Reversing Journals. The codes used to identify the types are:
	!	.table 3,25
	!	.te
	!	^*1\* - Regular
	!	.te
	!	^*2\* - Recurring
	!	.te
	!	^*3\* - Reversing
	!	.te
	!	^*4\* - Recurring/Reversing
	!	.end table
	!
	! Index:
	!	.x General Journals>Print
	!
	!--

	PRINT_ITEMS$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	.FIELD
	!	^*(04) Wildcard\*
	!	.b
	!	The ^*Wildcard\* field enables the user to print a report including selected
	!	accounts only, using the wildcard techniques.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99
	!	and the two right most numbers represented a department
	!	number and a report were to be printed to include department
	!	"02" only, the ^*Wildcard\* setting of "?????-02"
	!	would cause the report to print all accounts with the suffix of 02.
	!
	! Index:
	!
	!--


	SELECT JRL_TYPE$

	CASE "2"
		GL_GJ_LINE.JOURNAL$ = "Recurring"

	CASE "3"
		GL_GJ_LINE.JOURNAL$ = "Reversing"

	CASE "4"
		GL_GJ_LINE.JOURNAL$ = "Recurring/Reversing"

	CASE ELSE
		GL_GJ_LINE.JOURNAL$ = "General"
		JRL_TYPE$ = "1"

	END SELECT

	REVERSING = 1.0
	CURRENT_JRL = "~~~~~~~"

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.OPN"
	USE
		FILENAME$ = "GL_GJ_LINE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = GL_GJ_LINE.JOURNAL$ + " Journal"
	TITLE$(2%) = "GL System"
	TITLE$(3%) = ""

	!
	! Headings
	!
	TITLE$(4%) = "Item Acc #              Account Name        " + &
		"Source Description       Transaction    " + &
		"      -------Amount-------      Units      Hours"
	TITLE$(5%) = "                                            " + &
		"                                Date    " + &
		"      Debit         Credit"
	TITLE$(6%) = "."

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$ItemNum:004,$UndefAcctFlag:006,$Account:025," + &
		"$AcctDescr:046,$Source:051,$Descr:072,DTranDate:80," + &
		"VDebit:094,VCredit:109,VUnits:120,VHours:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_GJ_LINE.CH%
		ELSE
			FIND #GL_GJ_LINE.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_GJ_LINE"
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
		GET #GL_GJ_LINE.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "GL_GJ_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17400 IF (GL_GJ_LINE::JOURNAL > TO_ITEM$) AND (TO_ITEM$ <> "")

	!
	! Skip if recurring journal, and item not in list
	!
	GOTO GetNextRec &
		IF ((JRL_TYPE$ = "2") OR (JRL_TYPE$ = "4")) AND &
			(PRINT_ITEMS$ <> "") AND &
			(COMP_STRING(GL_GJ_LINE::JOURNAL, PRINT_ITEMS$) = 0%)

	!
	! Test account number against Chart of Accounts
	!
	UNDEF_ACCT_FLAG = " "	! Defined

	STAT% = GL_EXAM_CHART(GL_GJ_LINE::ACCT, GL_CHART_EXAM)
	GOTO ExitProgram IF (STAT% = CMC$_UNTERROR)

	IF (STAT% = CMC$_UNDEFINED)
	THEN
		UNDEF_ACCT_FLAG = "*"
		GL_CHART_EXAM::DESCR = STRING$(40%, A"?"B)
	END IF

17300	!
	! Print out one line
	!
	IF CURRENT_JRL <> GL_GJ_LINE::JOURNAL
	THEN
		GOSUB JournalTotal

		!
		! Display the new journal
		!
		TEXT$ = "|" + STRING$(130%, 45%) + "|"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "  Journal #  " + GL_GJ_LINE::JOURNAL + SPACE$(42%) + &
			"Source:  " + GL_GJ_LINE::SOURCE

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	AMOUNT = GL_GJ_LINE::AMOUNT * REVERSING

	TEXT$ = GL_GJ_LINE::ITEMNUM + " " + &
		UNDEF_ACCT_FLAG + &
		GL_GJ_LINE::ACCT + " " + &
		left(GL_CHART_EXAM::DESCR, 20%) + " " + &
		GL_GJ_LINE::SOURCE + " " + &
		left(GL_GJ_LINE::DESCR, 20%) + " " + &
		PRNT_DATE(GL_GJ_LINE::TRANDAT, 6%)

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, GL_GJ_LINE::ACCT, &
		0.0, AMOUNT, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	IF AMOUNT >= 0.
	THEN
		TEXT$ = TEXT$ + FORMAT$(AMOUNT, " ###,###,###.##") + &
			SPACE$(15%)
		J_DEBIT_TOT = J_DEBIT_TOT + AMOUNT
	ELSE
		TEXT$ = TEXT$ + SPACE$(15%) + &
			FORMAT$(-AMOUNT, " ###,###,###.##")
		J_CREDIT_TOT = J_CREDIT_TOT + AMOUNT
	END IF

	TEXT$ = TEXT$ + &
		FORMAT$(GL_GJ_LINE::UNITS * REVERSING, " <%>##,###.##") + &
		FORMAT$(GL_GJ_LINE::HOURS * REVERSING, " <%>##,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17400	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out last Journal total
	!
	GOSUB JournalTotal

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out grand totals
	!
	TEXT$ = SPACE$(43%) + &
		"GRAND TOTALS                         " + &
		FORMAT$(DEBIT_GRAND_TOT, "###,###,###.## ") + &
		FORMAT$(-CREDIT_GRAND_TOT, "###,###,###.##")

	TEXT$ = TEXT$ + "     Out of Balance  **" &
		IF FUNC_ROUND((DEBIT_GRAND_TOT + CREDIT_GRAND_TOT), 2%) <> 0.

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Force a second pass if closing journal
	!
	IF ((JRL_TYPE$ = "3") OR (JRL_TYPE$ = "4")) AND (REVERSING = 1.0)
	THEN
		TITLE$(2%) = "** Reversing **"
		REVERSING = -1.0
		DEBIT_GRAND_TOT, CREDIT_GRAND_TOT = 0.0
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
		CURRENT_JRL = "~~~~~~~"
		GOTO 17000
	END IF

	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

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

 JournalTotal:
	!******************************************************************
	! Subroutine to print the total for one journal item
	!******************************************************************
	IF CURRENT_JRL <> "~~~~~~~"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = "*=Undefined Account No.      JOURNAL # " + &
			CURRENT_JRL + &
			"   TOTALS                          " + &
			FORMAT$(J_DEBIT_TOT, "###,###,###.## ") + &
			FORMAT$(-J_CREDIT_TOT, "###,###,###.##")

		IF FUNC_ROUND((J_DEBIT_TOT+J_CREDIT_TOT), 2%) <> 0.0
		THEN
			TEXT$ = TEXT$ + "     Out of Balance  **"
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		DEBIT_GRAND_TOT = DEBIT_GRAND_TOT + J_DEBIT_TOT
		CREDIT_GRAND_TOT = CREDIT_GRAND_TOT + J_CREDIT_TOT
		J_DEBIT_TOT, J_CREDIT_TOT = 0.0

	END IF

	CURRENT_JRL = GL_GJ_LINE::JOURNAL + ""

	RETURN

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
	! End of report GL_RPRT_GJREP1
	!******************************************************************
	END
