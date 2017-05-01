1	%TITLE "Print By Transaction Type"
	%SBTTL "AR_RPRT_TRANS"
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
	! ID:AR022
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print by Transaction Type\* report prints
	!	a listing of all customer accounts in order by their
	!	transaction type. The type is previously assigned to each account.
	!	.lm -5
	!
	! Index:
	!	.x Print>By Transaction Type
	!	.x Report>Print By Transaction Type
	!
	! Option:
	!
	!
	! Author:
	!
	!	07/15/88 - Robert Peterson
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_TRANS/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_TRANS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_TRANS.OBJ;*
	!
	! Modification history:
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11", adjustment.
	!
	!	10/11/96 - Kevin Handy
	!		Clean up source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	07/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	MAP (AR_TEMP) &
		STRING	AR_TEMP.TYPE = 1%, &
		STRING	AR_TEMP.TRANS = 8%, &
		STRING	AR_TEMP.CUSNUM = 10%, &
		STRING	AR_TEMP.ARACCT = 18%, &
		STRING	AR_TEMP.TRADAT = 8%, &
		STRING	AR_TEMP.SUBACC = 10%, &
		STRING	AR_TEMP.APPLY = 8%, &
		STRING	AR_TEMP.DESCR = 20%, &
		REAL	AR_TEMP.AMOUNT

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	DIM TEXT$(1000%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

315	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

320	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AR_CONTROL::AR_ACCT = ""
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR account not defined", &
			"ERR", "AR_ACCT", "ERROR_NOARACCT")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR close in process", &
			"ERR", "AR_CLOSE", "ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Reset in process", &
			"ERR", "AR_RESET", "ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "3"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Purge in process", &
			"ERR", "AR_PURGE", "ERROR_PURGE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AR_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	TEST_YYYY_PP$, YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	YYYY_PP$ = LEFT(YYYY_PP$, 4%) + "_" + RIGHT(YYYY_PP$, 5%)

330	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AR_TEMP, &
			PRIMARY KEY (AR_TEMP.TYPE, AR_TEMP.TRANS) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "AR_TEMP"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		RESET #AR_OPEN.CH%, KEY #0%
	USE
		CONTINUE ReportTitle
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Open file", 1%)

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

410	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	SELECT RRR_FLAG%

	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

420	IF LEFT(AR_OPEN::UPDATED, 6%) = TEST_YYYY_PP$
	THEN

		IF INSTR(1%, "01!02!03!04!08!", AR_OPEN::TRATYP)
		THEN
			AR_TEMP.TYPE = "A"
			AR_TEMP.TRANS = AR_OPEN::INVNUM
			AR_TEMP.CUSNUM = AR_OPEN::CUSNUM
			AR_TEMP.ARACCT = AR_OPEN::ARACCT
			AR_TEMP.TRADAT = AR_OPEN::TRADAT
			AR_TEMP.SUBACC = AR_OPEN::SUBACC
			AR_TEMP.APPLY = AR_OPEN::RECNUM
			AR_TEMP.DESCR = AR_OPEN::DESCR
			AR_TEMP.AMOUNT = AR_OPEN::SALAMT

			WHEN ERROR IN
				PUT #AR_TEMP.CH%
			USE
				FILENAME$ = "AR_TEMP"
				CONTINUE HelpError
			END WHEN
		END IF

		IF INSTR(1%, "02!09!10!", AR_OPEN::TRATYP)
		THEN
			AR_TEMP.TYPE = "B"
			AR_TEMP.TRANS = AR_OPEN::RECNUM
			AR_TEMP.CUSNUM = AR_OPEN::CUSNUM
			AR_TEMP.ARACCT = AR_OPEN::ARACCT
			AR_TEMP.TRADAT = AR_OPEN::TRADAT
			AR_TEMP.SUBACC = AR_OPEN::SUBACC
			AR_TEMP.APPLY = AR_OPEN::INVNUM
			AR_TEMP.DESCR = AR_OPEN::DESCR
			AR_TEMP.AMOUNT = AR_OPEN::SALAMT

			WHEN ERROR IN
				PUT #AR_TEMP.CH%
			USE
				FILENAME$ = "AR_TEMP"
				CONTINUE HelpError
			END WHEN
		END IF
	END IF

	GOTO 410

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	TITLE$(1%) = "AR Transaction Report"
	TITLE$(2%) = "Accounting Period Ended " + &
		RIGHT(YYYY_PP$, 6%) + " " + &
		LEFT(YYYY_PP$, 4%)
	TITLE$(3%) = ""

	TITLE$(4%) = "Invoice # " + LEFT(AR_CONTROL::CTITLE, 10%) + &
		"  Name                            AR Account   " + &
		"         Date             Amount  Apply To        Balance"

	TITLE$(5%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FIRST_RECORD% = 0%

	WHEN ERROR IN
		RESET #AR_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "AR_TEMP"
		CONTINUE HelpError
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
		GET #AR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_TEMP"
		CONTINUE HelpError
	END WHEN

	IF AR_TEMP.TYPE = "B" AND TEST% = 0%
	THEN
		IF FIRST_RECORD%
		THEN
			GOSUB PrintTrans

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TEXT$ = SPACE$(101%) + &
			"Total    " + &
			FORMAT$(TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TITLE$(4%) = &
			"Receipt # " + LEFT(AR_CONTROL::CTITLE, 10%) + &
			"  Name                            AR Account   " + &
			"         Date             Amount  Apply To        Balance"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 999%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL = 0.0
		TEST_TRANS$ = ""
		FIRST_RECORD% = 0%

		TEST% = -1%
	END IF

17030	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ AR_TEMP.CUSNUM, REGARDLESS
	USE
		AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)

		CONTINUE 17040 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17040	IF TEST_TRANS$ <> AR_TEMP.TRANS AND FIRST_RECORD%
	THEN
		GOSUB PrintTrans

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	TEXT_LOOP% = TEXT_LOOP% + 1%

	IF TEST_TRANS$ <> AR_TEMP.TRANS OR FIRST_RECORD% = 0%
	THEN
		TEXT$(TEXT_LOOP%) = AR_TEMP.TRANS + "  " + &
			AR_TEMP.CUSNUM + "  " + &
			LEFT(AR_35CUSTOM::CUSNAM, 30%) + "  "
	ELSE
		TEXT$(TEXT_LOOP%) = SPACE$(54%)
	END IF

	TEXT$(TEXT_LOOP%) = TEXT$(TEXT_LOOP%) + &
		AR_TEMP.ARACCT + "  " + &
		PRNT_DATE(AR_TEMP.TRADAT, 8%) + "  " + &
		FORMAT$(AR_TEMP.AMOUNT, "##,###,###.##  ") + &
		AR_TEMP.APPLY + "  "

	TEST_TRANS$ = AR_TEMP.TRANS

	BALANCE = BALANCE + AR_TEMP.AMOUNT
	TOTAL = TOTAL + AR_TEMP.AMOUNT

	FIRST_RECORD% = -1%

	!
	! Try for another record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	IF FIRST_RECORD%
	THEN
		GOSUB PrintTrans

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	TEXT$ = SPACE$(101%) + &
		"Total    " + &
		FORMAT$(TOTAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE AR_TEMP.CH%

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 PrintTrans:
	!***************************************************************
	! Print transaction loop
	!**************************************************************
	FOR LOOP% = 1% TO TEXT_LOOP%
		IF LOOP% = TEXT_LOOP%
		THEN
			TEXT$(LOOP%) = TEXT$(LOOP%) + &
				FORMAT$(BALANCE, "##,###,###.##  ")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$(LOOP%), 0%)

		GOTO PrintTransExit IF UTL_REPORTX::STAT
	NEXT LOOP%

 PrintTransExit:
	BALANCE = 0.0
	TEXT_LOOP% = 0%

	RETURN

	%Page

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
