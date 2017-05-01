1	%TITLE "Transmittal by Batch Report"
	%SBTTL "GL_RPRT_AUDT01"
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
	! ID:GLBTCH
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Transmittal by Batch Report\* lists the transmittal information for
	!	each batch, which has been posted to a specified General
	!	Ledger file.
	!	.b
	!	The following columns are included:
	!	.table 30
	!	.te
	!	Account Number
	!	.te
	!	Description
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.te
	!	Units Amount
	!	.te
	!	Hours Amount
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x Transmittal by Batch>Print
	!	.x Print>Transaction by Batch
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_AUDT01/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_AUDT01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_AUDT01.OBJ;*
	!
	! Author:
	!
	!	12/29/86 - Kevin Handy
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/15/91 - Frank F. Starman
	!		Print message "OUT OF BALANCE" if batch is not
	!		in balance.
	!
	!	02/22/92 - Kevin Handy
	!		Added "Total Only" option to help search out
	!		batches that don't balance.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/10/93 - Kevin Handy
	!		Through in a partial binary search on the account
	!		summary in an attempt to gain a slight bit of more
	!		speed.  Any little bit will help.
	!
	!	03/29/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/12/94 - Kevin Handy
	!		Added TOTAL_ONLY type "O".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/23/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/13/96 - Kevin Handy
	!		Clean up.
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	04/17/98 - Kevin Handy
	!		Lose a lot of %PAGE directives
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/22/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Define record for the array(s)
	!
	RECORD TOTAL_RECORD
		STRING	ACCT = 18
		DOUBLE	DEBIT
		DOUBLE	CREDIT
		DOUBLE	UNITS
		DOUBLE	HOURS
	END RECORD

	!
	! Declare constants
	!
	DECLARE	LONG	CONSTANT MAX_BATCH = 20000
	DECLARE LONG	CONSTANT MAX_GRAND = 20000

	!
	! Dimension arrays
	!
	DIM TOTAL_RECORD BATCH_TOTAL(MAX_BATCH)
	DIM TOTAL_RECORD GRAND_TOTAL(MAX_GRAND)

	!
	! Declare some variables
	!
	DECLARE STRING CURRENT_YEAR, CURRENT_PERIOD, BATCH_NUM
	DECLARE LONG TOTAL_BATCH, TOTAL_GRAND

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

	FROM_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 6%)

	!++
	! Abstract:FLD01
	!	^*(01) From Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*From Batch _#\* setting selects a batch
	!	number from which the report is to begin printing.
	!	.b
	!	If the report is to begin with the first batch _# in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%), 6%)

	!++
	! Abstract:FLD02
	!	^*(02) To Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*To Batch _#\* setting selects a batch
	!	number with which the report is to end printing.
	!	.b
	!	If the report is to end with the last batch _# in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(2%), 5%)
	!++
	! Abstract:FLD03
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	^*Period\* refers to the accounting period that will be considered when
	!	running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a Transmittal
	!	(Batch) Audit Report including only selected accounts, using
	!	wildcard techniques. A blank or an _* in this field will
	!	cause a Transmittal Audit Report to list ^&all\& batches.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOTAL_ONLY$ = LEFT(UTL_REPORTX::OPTDEF(4%), 1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Balances Only	Yes, No, Only Nonzero\*
	!	.b
	!	.lm +5
	!	Determines how much detail is shown on the report.
	!	Valid entries are:
	!	.lm +5
	!	.b
	!	*Y Yes. Print only totals.
	!	.br
	!	*N No. Print Totals and full detail.
	!	.br
	!	*O Only nonzero.
	!	Only print non-zero batches.
	!	.lm -5
	!	.lm -5
	!
	! Index:
	!
	!--

	CURRENT_YEAR = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD = RIGHT(YYYY_PP$, 6%)
	BATCH_NUM = "12345678901234567890"
	TOTAL_BATCH = 0%
	TOTAL_GRAND = 0%

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Get the current period file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "GL Audit Batch Listing"
	TITLE$(2%) = "Period " + CURRENT_PERIOD + " of Year " + CURRENT_YEAR
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "Account #           Description               " + &
		"                      Debit                    Credit " + &
		"         Units         Hours"
	TITLE$(5%) = ""

	!
	! Layouts for some of the printed lines
	!
	LYT_LINE$ = "$Account:018,$Descr:050,VDebit:072,VCredit:098," + &
		"VUnits:113,VHours:127"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%, KEY #4%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #4% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_" + YYYY_PP$
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
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17900 IF (GL_YYYY_PP::BTHNUM > TO_ITEM$) AND TO_ITEM$ <> ""

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(GL_YYYY_PP::ACCT, -1%), &
			WLDCRD$) = 0%
	END IF

	!
	! Total up this batch item if new batch number
	!
	GOSUB BatchTotal IF GL_YYYY_PP::BTHNUM <> BATCH_NUM

	!
	! Search BATCH_TOTAL balance list for currently existing account
	!

	!
	! First a binary search to speed up the sequential search.
	! Will cut the search range down to a maximum of 4 items to look at.
	! Then is followed by a sequential search of the localized range.
	!
	SFROM% = 1%
	STO% = TOTAL_BATCH

	WHILE STO% - SFROM% > 4%
		SMID% = (SFROM% + STO%) / 2%
		IF (BATCH_TOTAL(SMID%)::ACCT <= GL_YYYY_PP::ACCT)
		THEN
			SFROM% = SMID%
		ELSE
			STO% = SMID%
		END IF
	NEXT

	!
	! Now the sequential search on the localized range.
	!
	GOTO GotAccount &
		IF (BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT) &
			FOR I% = SFROM% TO STO%

	!
	! Item not found, create it
	!
	I%, TOTAL_BATCH = TOTAL_BATCH + 1%

	WHILE (I% > 1%) AND (BATCH_TOTAL(I% - 1%)::ACCT > GL_YYYY_PP::ACCT)
		BATCH_TOTAL(I%) = BATCH_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT
	BATCH_TOTAL(I%)::CREDIT = 0.0
	BATCH_TOTAL(I%)::DEBIT = 0.0
	BATCH_TOTAL(I%)::UNITS = 0.0
	BATCH_TOTAL(I%)::HOURS = 0.0

 GotAccount:
	!
	! Add credit/debit amounts
	!
	IF GL_YYYY_PP::AMOUNT > 0.0
	THEN
		BATCH_TOTAL(I%)::DEBIT = BATCH_TOTAL(I%)::DEBIT + &
			GL_YYYY_PP::AMOUNT
	ELSE
		BATCH_TOTAL(I%)::CREDIT = BATCH_TOTAL(I%)::CREDIT + &
			GL_YYYY_PP::AMOUNT
	END IF

	BATCH_TOTAL(I%)::UNITS = BATCH_TOTAL(I%)::UNITS + GL_YYYY_PP::UNITS
	BATCH_TOTAL(I%)::HOURS = BATCH_TOTAL(I%)::HOURS + GL_YYYY_PP::HOURS

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Total up last batch number
	!
	GOSUB BatchTotal

	!
	! Print out grand total title
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Grand Totals", 0%)

	!
	! Print all totals for this batch
	!
	TOTAL_DEBIT = 0.0
	TOTAL_CREDIT = 0.0
	TOTAL_UNITS = 0.0
	TOTAL_HOURS = 0.0

	FOR I% = 1% TO TOTAL_GRAND

		GOTO 17930 IF GRAND_TOTAL(I%)::DEBIT = 0.0 AND &
			GRAND_TOTAL(I%)::CREDIT = 0.0 AND &
			GRAND_TOTAL(I%)::UNITS = 0.0 AND &
			GRAND_TOTAL(I%)::HOURS = 0.0

17910		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ GRAND_TOTAL(I%)::ACCT, &
				REGARDLESS
		USE
			GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)
			CONTINUE 17920 IF ERR = 155%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

17920		TEXT$ = LEFT(GRAND_TOTAL(I%)::ACCT + SPACE$(18%), 18%) + &
			"  " + &
			LEFT(GL_CHART::DESCR, 30%) + "     " + &
			FORMAT$(GRAND_TOTAL(I%)::DEBIT, &
				"<%>##,###,###,###.##        ") + &
			FORMAT$(-GRAND_TOTAL(I%)::CREDIT, &
				"<%>##,###,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(I%)::UNITS, &
				"###,###,###.##") + &
			FORMAT$(GRAND_TOTAL(I%)::HOURS, &
				"###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TOTAL_DEBIT = TOTAL_DEBIT + GRAND_TOTAL(I%)::DEBIT
		TOTAL_CREDIT = TOTAL_CREDIT + GRAND_TOTAL(I%)::CREDIT
		TOTAL_UNITS = TOTAL_UNITS + GRAND_TOTAL(I%)::UNITS
		TOTAL_HOURS = TOTAL_HOURS + GRAND_TOTAL(I%)::HOURS

17930	NEXT I%

	TEXT$ = "                    " + &
		"Grand Totals                       " + &
		FORMAT$(TOTAL_DEBIT, "<%>##,###,###,###.##        ") + &
		FORMAT$(-TOTAL_CREDIT, "<%>##,###,###,###.## ") + &
		FORMAT$(TOTAL_UNITS, "###,###,###.##") + &
		FORMAT$(TOTAL_HOURS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)



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

 BatchTotal:
18000	!******************************************************************
	! Subroutine to print out totals for one batch
	!******************************************************************

	GOTO 18040 IF TOTAL_BATCH = 0%

	!
	! Calculate totals
	!
	TOTAL_DEBIT = 0.0
	TOTAL_CREDIT = 0.0
	TOTAL_UNITS = 0.0
	TOTAL_HOURS = 0.0

	FOR I% = 1% TO TOTAL_BATCH

		TOTAL_DEBIT = TOTAL_DEBIT + BATCH_TOTAL(I%)::DEBIT
		TOTAL_CREDIT = TOTAL_CREDIT + BATCH_TOTAL(I%)::CREDIT
		TOTAL_UNITS = TOTAL_UNITS + BATCH_TOTAL(I%)::UNITS
		TOTAL_HOURS = TOTAL_HOURS + BATCH_TOTAL(I%)::HOURS

	NEXT I%

	!
	! Skip over this batch if only want non-zero totals
	!
	IF TOTAL_ONLY$ = "O"
	THEN
		GOTO 18040 IF ABS(TOTAL_DEBIT + TOTAL_CREDIT) < .001
	END IF

	!
	! Print title for batch
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"BATCH NUMBER:  " + BATCH_NUM, 3%)

	!
	! Print all totals for this batch
	!
	FOR I% = 1% TO TOTAL_BATCH

18010		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ BATCH_TOTAL(I%)::ACCT, &
				REGARDLESS
		USE
			CONTINUE 18020 IF ERR = 155%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

18020		IF TOTAL_ONLY$ = "N"
		THEN
			TEXT$ = BATCH_TOTAL(I%)::ACCT + "  " + &
				LEFT(GL_CHART::DESCR, 30%) + "     " + &
				FORMAT$(BATCH_TOTAL(I%)::DEBIT, &
					"<%>##,###,###,###.##        ") + &
				FORMAT$(-BATCH_TOTAL(I%)::CREDIT, &
					"<%>##,###,###,###.## ") + &
				FORMAT$(BATCH_TOTAL(I%)::UNITS, &
					"###,###,###.##") + &
				FORMAT$(BATCH_TOTAL(I%)::HOURS, &
					"###,###,###.##")

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 1%)
		END IF

	NEXT I%

	IF FUNC_ROUND(TOTAL_DEBIT + TOTAL_CREDIT, 2%) <> 0.0
	THEN
		OUTFLAG$ = "***************"
	ELSE
		OUTFLAG$ = "               "
	END IF

	TEXT$ = "                    " + &
		"Batch Totals        " + &
		OUTFLAG$ + &
		FORMAT$(TOTAL_DEBIT, &
			"<%>##,###,###,###.##        ") + &
		FORMAT$(-TOTAL_CREDIT, &
			"<%>##,###,###,###.## ") + &
		FORMAT$(TOTAL_UNITS, &
			"###,###,###.##") + &
		FORMAT$(TOTAL_HOURS, &
			"###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

18030	!
	! Insert batch totals into grand totals
	!
	GRAND_POINTER% = 1%

	FOR I% = 1% TO TOTAL_BATCH

		!
		! Search for place to add it to in the grand totals
		!
		GRAND_POINTER% = GRAND_POINTER% + 1% &
			UNTIL (GRAND_TOTAL(GRAND_POINTER%)::ACCT >= &
			BATCH_TOTAL(I%)::ACCT) OR &
			GRAND_POINTER% > TOTAL_GRAND

		!
		! Create new one if necessary
		!
		IF (GRAND_POINTER% > TOTAL_GRAND) OR &
			(GRAND_TOTAL(GRAND_POINTER%)::ACCT <> &
			BATCH_TOTAL(I%)::ACCT)
		THEN
			!
			! Make room for new one
			!
			TOTAL_GRAND = TOTAL_GRAND + 1%
			GRAND_TOTAL(J%) = GRAND_TOTAL(J% - 1%) &
				FOR J% = TOTAL_GRAND TO GRAND_POINTER% + 1% &
					STEP -1%

			!
			! And insert it
			!
			GRAND_TOTAL(GRAND_POINTER%) = BATCH_TOTAL(I%)
		ELSE
			!
			! Add to the totals
			!
			J% = GRAND_POINTER%
			GRAND_TOTAL(J%)::DEBIT = GRAND_TOTAL(J%)::DEBIT + &
				BATCH_TOTAL(I%)::DEBIT
			GRAND_TOTAL(J%)::CREDIT = GRAND_TOTAL(J%)::CREDIT + &
				BATCH_TOTAL(I%)::CREDIT
			GRAND_TOTAL(J%)::UNITS = GRAND_TOTAL(J%)::UNITS + &
				BATCH_TOTAL(I%)::UNITS
			GRAND_TOTAL(J%)::HOURS = GRAND_TOTAL(J%)::HOURS + &
				BATCH_TOTAL(I%)::HOURS
		END IF

	NEXT I%

18040	!
	! Initialize for new batch group
	!
	BATCH_NUM = GL_YYYY_PP::BTHNUM + ""
	TOTAL_BATCH = 0%

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
	! End of report GL_RPRT_AUDT01
	!******************************************************************
	END
