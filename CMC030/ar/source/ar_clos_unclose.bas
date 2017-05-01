1	%TITLE "Accounts Receivable UnClosing Program"
	%SBTTL "AR_CLOS_UNCLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program is used to reverse a close in the
	!	AR system.
	!	.B
	!	^*Note:\*  This procedure will only work if ALL customers are open
	!	item.  Does not work with balance forward customers.
	!	Does not handle being interrupted.  Do not abort
	!	in the middle.  It is also a good idea to have
	!	good backups of AR_OPEN, AR_CLOSED and AR_CONTROL
	!	in case of failure.
	!	Does not recover distribution, since it isn't kept
	!	with the closed file.
	!	Does nothing with the AR_CUSBAL file.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	!	AR_CLOS_UNCLOSE$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CLOS_UNCLOSE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_CLOS_UNCLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CLOS_UNCLOSE.OBJ;*
	!
	! Author:
	!
	!	10/27/88 - Kevin Handy
	!
	! Modification history:
	!
	!	11/29/88 - Kevin Handy
	!		Fixed bug where it was not updating the control
	!		file correctly.
	!
	!	02/05/93 - Kevin Handy
	!		Added AR_OPEN::DUEDATE and AR_OPEN::DISCOUNTDATE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	01/08/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	03/05/98 - Kevin Handy
	!		Allow others to be in the OPEN file at the
	!		same time (MOD vs UPD)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Use WHEN ERROR
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.MOD"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AR close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.CRE"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

340	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

360	!
	! Open AR Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.MOD"

		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Close in process", &
			"ERR", "AR_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "3"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Purge in process", &
			"ERR", "AR_PURGE", "ERROR_PURGE")
		GOTO ExitProgram
	END IF


	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE
	YEAR$ = AR_CONTROL::YEAR

	!
	! Note: YYYY_PP has underscore stored in it, YYYYPP doesn't.
	!
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")
	YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	!
	! Calculate the date of the last day of the period, if
	! possible.
	!
	AGE.MONTH% = VAL%(RIGHT(YYYYPP$, 5%)) - GL_PERIOD::NEWYEAR + 1%
	AGE.MONTH% = AGE.MONTH% + GL_PERIOD::FPFY &
		IF AGE.MONTH% <= 0%
	AGE.MONTH% = AGE.MONTH% * 12% / GL_PERIOD::FPFY

	!
	! Wiz-bang method of calculating last date of month.
	! Take some date in the month (say the 20'th), add enough to force
	! it into the next month, but not any farther, change the
	! date to the first of that month, then subtract one day.
	!
	! (Don't laugh, it's all I could come up with on the spur of
	! the moment, and it does work)
	!
	AGE.DATE$ = LEFT(YYYYPP$, 4%) + FORMAT$(AGE.MONTH%, "<0>#") + "20"
	AGE.DATE$ = DATE_INVDCODE(DATE_DAYCODE( &
		LEFT(DATE_INVDCODE(DATE_DAYCODE(AGE.DATE$) + 20%), 6%) &
		+ "01") - 1%)

500	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Accounts Receivable UnClose for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"UNCLOSING " + YYYY_PP$ + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 2%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Age Date         " + PRNT_DATE(AGE.DATE$, 8%), 4%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Customer # ", 6%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm unclosing - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
1100	! Close Accounts receivable
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "UnClosing", 1%)

	WHEN ERROR IN
		RESET #AR_CLOSED.CH%
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

1115	!
	! Set close flag in control file
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%

		AR_CONTROL::CLOSEFLAG = "2"

		UPDATE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

1125	!
	! Loop through closed file, copying everything that was
	! closed in the specified period.
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%

		IF AR_CLOSED::CLOSEDATE = YYYYPP$
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				AR_OPEN::CUSNUM, 6%, 23%)

			AR_OPEN::CUSNUM	= AR_CLOSED::CUSNUM
			AR_OPEN::INVNUM	= AR_CLOSED::INVNUM
			AR_OPEN::TRATYP	= AR_CLOSED::TRATYP
			AR_OPEN::TRADAT	= AR_CLOSED::TRADAT
			AR_OPEN::SALAMT	= AR_CLOSED::SALAMT
			AR_OPEN::DISAMT	= AR_CLOSED::DISAMT
			AR_OPEN::OTHCHG	= AR_CLOSED::OTHCHG
			AR_OPEN::RECNUM	= AR_CLOSED::RECNUM
			AR_OPEN::CHKNUM	= AR_CLOSED::CHKNUM
			AR_OPEN::ARACCT	= AR_CLOSED::ARACCT
			AR_OPEN::SUBACC	= AR_CLOSED::SUBACC
			AR_OPEN::SALNUM	= AR_CLOSED::SALNUM
			AR_OPEN::DESCR	= AR_CLOSED::DESCR
			AR_OPEN::BATCH	= AR_CLOSED::BATCH
			AR_OPEN::UPDATED	= AR_CLOSED::UPDATED
			AR_OPEN::CLOSEDATE	= YYYYPP$
			AR_OPEN::DUEDATE = AR_CLOSED::DUEDATE
			AR_OPEN::DISCOUNTDATE = AR_CLOSED::DISCOUNTDATE

			!
			! Move across
			!
			PUT #AR_OPEN.CH%

			DELETE #AR_CLOSED.CH%
		END IF
	USE
		CONTINUE 2400
	END WHEN

1130	!
	! Get the next record
	!
	GOTO 1125

	%PAGE

2200	!*******************************************************************
	! Complete Initail process
	!*******************************************************************

2400	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%

		CUR_PERIOD% = CUR_PERIOD% - 1%
		IF CUR_PERIOD% = 0%
		THEN
			CUR_PERIOD% = GL_PERIOD::FPFY
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		END IF

		AR_CONTROL::CLOSEFLAG = "0"
		AR_CONTROL::LASTPERCLOSE = CUR_PERIOD%
		AR_CONTROL::YEAR = YEAR$

		UPDATE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "UnClosing completed", 0%)

	SCOPE::PRG_ITEM = "HELP"
	GOTO ExitProgram

	%Page

	!*******************************************************************
	! Exit from the program
	!*******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE


 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

32767	END
