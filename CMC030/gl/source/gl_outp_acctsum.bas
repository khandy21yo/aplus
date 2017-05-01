1	%TITLE "Store and Print Account Summary"
	%SBTTL "GL_OUTP_ACCTSUM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_OUTP_ACCTSUM(LONG OPT, &
		STRING ACCT_NUM, GFLOAT BEG_AMT, GFLOAT ACCT_AMT, &
		GFLOAT ACCT_UNITS, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX)

	!
	! COPYRIGHT (C) 1991 BY
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	Store and print Summary of GL account.
	!	.LM -5
	!
	! Index:
	!
	! Parameters:
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_OUTP_ACCTSUM
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_OUTP_ACCTSUM
	!	$ DELETE GL_OUTP_ACCTSUM.OBJ;*
	!
	! Author:
	!
	!	06/03/91 - Craig Tanner
	!
	! Modification history:
	!
	!	06/21/91 - Frank F. Starman
	!		Set account number to blank if the string doesn't
	!		have "good" characters.
	!		Add message if there is no balance.
	!
	!	10/22/92 - Frank F. Starman
	!		Added new argument for beginning balance.
	!
	!	03/18/93 - Kevin Handy
	!		Added new arguement for units.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE		GL_CHART_CDD	GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]POST_TO_GL.HB"
	MAP	(GL_TOTAL)	POST_TO_GL_CDD	GL_TOTAL

	COM (GL_OUTP_ACCTSUM.COM) &
		LONG GL_TOTAL.IDX

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	DECLARE STRING ACCT_NUMBER
	DECLARE LONG EXIT_STATUS

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

100	!
	! Open file
	!
	IF GL_TOTAL.IDX = 0%
	THEN
		CALL ASSG_CHANNEL(GL_TOTAL.IDX, STAT%)
		WHEN ERROR IN
			OPEN "GL_TOTAL.IDX" FOR OUTPUT AS FILE GL_TOTAL.IDX, &
				ORGANIZATION INDEXED FIXED, &
				MAP GL_TOTAL, &
				BUFFER 32%, &
				TEMPORARY, &
				PRIMARY KEY GL_TOTAL::ACCT, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "GL_TOTAL"
			CONTINUE HelpError
		END WHEN

	END IF

	SELECT OPT

	!
	! Create file
	!
	CASE OPT_ADDREC

		!GOTO ExitFunction IF ACCT_AMT = 0.0

		!
		! Put/update record in the GL temporary total file
		!

		! Credit/debit amounts first
		DEBIT, CREDIT = 0.0

		IF (ACCT_AMT > 0.0)
		THEN
			DEBIT = ACCT_AMT
		ELSE
			CREDIT = ACCT_AMT
		END IF

		!
		! Find the record in the file that has this account
		!
		ACCT_NUMBER = ACCT_NUM
		ACCT_NUMBER = SPACE$(LEN(GL_TOTAL::ACCT)) &
			IF EDIT$(ACCT_NUM, -1%) = ""

500		WHEN ERROR IN
			GET #GL_TOTAL.IDX, KEY #0% EQ ACCT_NUMBER
		USE
			CONTINUE 550 IF ERR = 155%
			FILENAME$ = "GL_TOTAL"
			CONTINUE HelpError
		END WHEN

		GL_TOTAL::DEBIT		= GL_TOTAL::DEBIT + DEBIT
		GL_TOTAL::CREDIT	= GL_TOTAL::CREDIT + CREDIT
		GL_TOTAL::BEGBAL	= GL_TOTAL::BEGBAL + BEG_AMT
		GL_TOTAL::UNITS		= GL_TOTAL::UNITS + ACCT_UNITS

		WHEN ERROR IN
			UPDATE #GL_TOTAL.IDX
		USE
			CONTINUE 550 IF ERR = 155%
			FILENAME$ = "GL_TOTAL"
			CONTINUE HelpError
		END WHEN

		GOTO ExitFunction

550		!
		! Record not found; create it
		!
		GL_TOTAL::ACCT		= ACCT_NUMBER
		GL_TOTAL::DESCR		= ""
		GL_TOTAL::ACCTYPE	= ""
		GL_TOTAL::BEGBAL	= BEG_AMT
		GL_TOTAL::UNITS		= ACCT_UNITS
		GL_TOTAL::HOURS		= 0.0
		GL_TOTAL::DEBIT		= DEBIT
		GL_TOTAL::CREDIT	= CREDIT
		PUT #GL_TOTAL.IDX

	!
	! Print out totals for credit/debit
	!
	CASE OPT_SUMMARY, OPT_SUMMARY + SUBOPT_DETAIL

		I% = 1%
		WHILE TITLE(I%) <> ""
			I% = I% + 1%
		NEXT

		J% = I% + 1%
		WHILE TITLE(J%) <> ""
			TITLE(J%) = ""
			J% = J% + 1%
		NEXT

		!
		! Get ready to print Debit/Credit (New page, reset titles, etc.)
		!
		IF OPT = OPT_SUMMARY
		THEN
			TITLE(I% + 1%) = "Account            Description" + &
				"                             Debit         Credit"
		ELSE
			TITLE(I% + 1%) = "Account            Description" + &
				"                            BegBal          Debit         Credit         EndBal"
		END IF

		TITLE(I%+2%) = "."

		LIN% = 999%
		TOTAL.DB, TOTAL.CR = 0.0
		TOTAL.BB, TOTAL.EB = 0.0
		TOTAL.UN = 0.0

600		RESET #GL_TOTAL.IDX

		!
		! Print out the Debit/Credit information
		!
610		WHEN ERROR IN
			GET #GL_TOTAL.IDX
		USE
			CONTINUE 620 IF ERR = 11%
			FILENAME$ = "GL_TOTAL"
			CONTINUE HelpError
		END WHEN

		ST% = GL_EXAM_CHART(GL_TOTAL::ACCT, GL_CHART_EXAM)

		IF OPT = OPT_SUMMARY
		THEN
			TEXT$ = GL_TOTAL::ACCT + " " + &
				LEFT(GL_CHART_EXAM::DESCR, 30%) + " " + &
				FORMAT$(GL_TOTAL::DEBIT, "<%>##,###,###.## ") + &
				FORMAT$(-GL_TOTAL::CREDIT, "<%>##,###,###.## ") + &
				FORMAT$(GL_TOTAL::UNITS, "<%>##,###,###.## ")
		ELSE
			TEXT$ = GL_TOTAL::ACCT + " " + &
				LEFT(GL_CHART_EXAM::DESCR, 30%) + " " + &
				FORMAT$(GL_TOTAL::BEGBAL, "###,###,###.## ") + &
				FORMAT$(GL_TOTAL::DEBIT, "<%>##,###,###.## ") + &
				FORMAT$(-GL_TOTAL::CREDIT, "<%>##,###,###.## ") + &
				FORMAT$(GL_TOTAL::UNITS, "<%>##,###,###.## ") + &
				FORMAT$(GL_TOTAL::BEGBAL + &
					GL_TOTAL::DEBIT + &
					GL_TOTAL::CREDIT, "###,###,###.## ")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, LIN%)
		GOTO ExitFunction IF UTL_REPORTX::STAT

		!
		! Add credit/debit amounts
		!
		TOTAL.BB = FUNC_ROUND(TOTAL.BB + GL_TOTAL::BEGBAL, 2%)
		TOTAL.DB = FUNC_ROUND(TOTAL.DB + GL_TOTAL::DEBIT, 2%)
		TOTAL.CR = FUNC_ROUND(TOTAL.CR + GL_TOTAL::CREDIT, 2%)
		TOTAL.UN = FUNC_ROUND(TOTAL.UN + GL_TOTAL::UNITS, 2%)

		LIN% = 2%

		GOTO 610

		!
		! Print out a blank, then Debit/Credit totals
		!
620		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", LIN%)
		GOTO ExitFunction IF UTL_REPORTX::STAT

		IF OPT = OPT_SUMMARY
		THEN
			IF TOTAL.DB + TOTAL.CR <> 0.0
			THEN
				TEXT$ = "Totals are out of balance " + &
					FORMAT$(TOTAL.DB + TOTAL.CR, "##,###,###.##           ")
			ELSE
				TEXT$ = "Totals" + SPACE$(44%)
			END IF
			TEXT$ = TEXT$ + &
				FORMAT$(TOTAL.DB, "<%>##,###,###.## ") + &
				FORMAT$(-TOTAL.CR, "<%>##,###,###.## ") + &
				FORMAT$(TOTAL.UN, "<%>##,###,###.## ")
		ELSE
			TEXT$ = "Totals" + SPACE$(44%) + &
				FORMAT$(TOTAL.BB, "###,###,###.## ") + &
				FORMAT$(TOTAL.DB, "<%>##,###,###.## ") + &
				FORMAT$(-TOTAL.CR, "<%>##,###,###.## ") + &
				FORMAT$(TOTAL.UN, "<%>##,###,###.## ") + &
				FORMAT$(TOTAL.BB + &
					TOTAL.DB + &
					TOTAL.CR, "###,###,###.## ")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	END SELECT

 ExitFunction:
	GL_OUTP_ACCTSUM = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)
	EXIT_STATUS = CMC$_UNTERROR

	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
