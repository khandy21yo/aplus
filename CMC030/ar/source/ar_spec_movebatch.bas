1	%TITLE "Move Posted Batch to Another Period"
	%SBTTL "AR_SPEC_MOVEBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:ARMVBT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program is used to move a batch in the AR OPEN
	!	file when posted to the wrong period.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXE=AR_EXE: AR_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	07/11/89 - Kevin Handy
	!
	! Modification history:
	!
	!	07/20/89 - Lance Williams
	!		Added in movement of GK period files.
	!
	!	11/21/90 - Kevin Handy
	!		Fixed yet again in a vain attempt to try to get the
	!		parameters to match the report settings, until
	!		Frank decides to change them again so that it will
	!		not work again.
	!
	!	03/01/94 - Kevin Handy
	!		Fixed bug where it reported AR_OPEN error as
	!		being from GL_YYYY_PP.
	!
	!	03/01/94 - Kevin Handy
	!		Trapped error at 300 (error 155) to give up and
	!		try the GL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/05/98 - Kevin Handy
	!		Scan through the AR_CLOSED file also
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	07/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	!
	! Define record for the array(s)
	!
	RECORD TOTAL_RECORD
		STRING	ACCT = 18%
		STRING	SOURCE = 4%
		STRING	TRANDAT = 8%
		DOUBLE	AMOUNT
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
	DIM	TOTAL_RECORD	BATCH_TOTAL(MAX_BATCH)

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

	GL_BATCH$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 6%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* enters the number of the batch which
	!	will be moved.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!	.x Move>Batch
	!	.x Batch>Move
	!	.x Number>Batch
	!
	!--
	FROM_YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(1%), 4%) + "_" + &
		TRM$(RIGHT(UTL_REPORTX::OPTDEF(1%), 5%))

	!++
	! Abstract:FLD02
	!	^*(02) From General Ledger Period\*
	!	.b
	!	.lm +5
	!	The ^*From General Ledger Period\* field enters the
	!	accounting period from which the batch will be removed.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--
	TO_YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		TRM$(RIGHT(UTL_REPORTX::OPTDEF(2%), 5%))

	!++
	! Abstract:FLD03
	!	.ts 55
	!	^*(03) To General Ledger Period	YYYYPP\*
	!	.b
	!	.lm +5
	!	The ^*To General Ledger Period\* enters the period to
	!	which the batch will be moved.
	!	.lm -5
	!
	! Index:
	!
	!--
	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 6%)

	%PAGE

300	!******************************************************************
	! Do the AR_OPEN file
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.MOD"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #1% EQ GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 320 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		GET #AR_OPEN.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 320 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 320 IF AR_OPEN::BATCH <> GL_BATCH$

	AR_OPEN::UPDATED = YYYY_PP$ + "00"

	UPDATE #AR_OPEN.CH%

	PRINT ".";

	GOTO 310

320	CLOSE #AR_OPEN.CH%


	!*******************************************************************
	! To the General Ledger File
	!*******************************************************************

	!
	! Get the current period file
	!
330	YYYY_PP$ = FROM_YYYY_PP$
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	FROM_GL_YYYY_PP.CH% = GL_YYYY_PP.CH%
	GL_YYYY_PP.CH% = 0%

340	!
	! Open up GL_YYYY_PP file to transfer to
	!
	YYYY_PP$ = TO_YYYY_PP$
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"

400	!******************************************************************
	! Do the AR_OPEN file
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.MOD"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #1% EQ GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 420 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		GET #AR_CLOSED.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 420 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 420 IF AR_CLOSED::BATCH <> GL_BATCH$

	AR_CLOSED::UPDATED = YYYY_PP$ + "00"

	UPDATE #AR_CLOSED.CH%

	PRINT ".";

	GOTO 410

420	CLOSE #AR_CLOSED.CH%

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "GL Transfer Transmittal"
	TITLE$(2%) = "From Period " + FROM_YYYY_PP$ + " To Period " + TO_YYYY_PP$
	TITLE$(3%) = "Batch #" + GL_BATCH$
	TITLE$(4%) = ""
	TITLE$(5%) = "Account                                           " + &
			"       Amount           Unit          Hours"

	!
	! Headers
	!
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	TOTAL_BATCH% = 0%
	TOTAL_TRAN% = 0%

	!
	! Get start of next batch
	!
17120	WHEN ERROR IN
		GET #FROM_GL_YYYY_PP.CH%, KEY #4% GE GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 17200
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17200 IF (GL_YYYY_PP::BTHNUM <> GL_BATCH$)

	!
	! Search BATCH_TOTAL balance list for currently existing account
	!
	GOTO GotAccount &
		IF (BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT) &
			FOR I% = 1% TO TOTAL_BATCH%

	!
	! Item not found, create it
	!
	I%, TOTAL_BATCH% = TOTAL_BATCH% + 1%

	WHILE (I% > 1%) AND (BATCH_TOTAL(I% - 1%)::ACCT > GL_YYYY_PP::ACCT)
		BATCH_TOTAL(I%) = BATCH_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT
	BATCH_TOTAL(I%)::SOURCE = GL_YYYY_PP::SOURCE
	BATCH_TOTAL(I%)::TRANDAT = GL_YYYY_PP::TRANDAT
	BATCH_TOTAL(I%)::AMOUNT = 0.0
	BATCH_TOTAL(I%)::UNITS = 0.0
	BATCH_TOTAL(I%)::HOURS = 0.0

 GotAccount:
	!
	! Add credit/debit amounts
	!
	BATCH_TOTAL(I%)::AMOUNT = BATCH_TOTAL(I%)::AMOUNT + &
		GL_YYYY_PP::AMOUNT
	BATCH_TOTAL(I%)::UNITS = BATCH_TOTAL(I%)::UNITS + GL_YYYY_PP::UNITS
	BATCH_TOTAL(I%)::HOURS = BATCH_TOTAL(I%)::HOURS + GL_YYYY_PP::HOURS

17150	!
	! Copy over this record
	!
	PUT #GL_YYYY_PP.CH%

17160	DELETE #FROM_GL_YYYY_PP.CH%

	TOTAL_TRAN% = TOTAL_TRAN% + 1%

17180	!
	! Try for next record
	!
	GOTO 17120

	%PAGE

17200	!*******************************************************************
	! Read entire batch, now add data to transfer file
	!*******************************************************************

	FOR I% = 1% TO TOTAL_BATCH%

		TEXT$ = BATCH_TOTAL(I%)::ACCT + &
			SPACE$(30%) + " " + &
			FORMAT$(BATCH_TOTAL(I%)::AMOUNT, "###,###,###.##") + " " + &
			FORMAT$(BATCH_TOTAL(I%)::UNITS, "###,###,###.##") + " " + &
			FORMAT$(BATCH_TOTAL(I%)::HOURS, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	NEXT I%

	TEXT$ = GL_BATCH$ + "   Transfered, " + &
		NUM1$(TOTAL_TRAN%) + " records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitTotal

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:


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
	SELECT ERR
	!
	! Locked block
	!
	CASE 154%
		SLEEP 1%
		RESUME
	END SELECT

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report GL_SPEC_MOVEBATCH
	!******************************************************************
	END
