1	%TITLE "Erase a Posted Batch"
	%SBTTL "AP_SPEC_ERASEBATCH"
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program is used to move a batch in the AP OPEN
	!	file when posted to the wrong period.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_ERASEBATCH/LINE
	!	$ LINK/EXE=AP_EXE: AP_SPEC_ERASEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_ERASEBATCH.OBJ;*
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
	!	05/07/92 - Kevin Handy
	!		Reduced rediculous max of 20,000 to 5,000.
	!
	!	05/07/92 - Kevin Handy
	!		Removed printing of "."s.
	!
	!	06/17/93 - Kevin Handy
	!		Added removal of batches from AR_OPEN_DIST
	!		also.
	!
	!	06/17/93 - Kevin Handy
	!		Modified so won't give up if cannot find record
	!		in first fil it tries.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/12/98 - Kevin Handy
	!		Fix so that it skips to 420 on error `55 line 400
	!		instead of expecting an error 11.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/16/2000 - Kevin Handy
	!		Handle error 11 on line 400.
	!		Use WHEN ERROR IN
	!
	!	06/10/2003 - Kevin Handy
	!		Remove entries from PO register at same time
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
	DECLARE UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP	(PO_REG_LINE)	PO_REG_LINE_CDD	PO_REG_LINE

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
	DECLARE	LONG	CONSTANT MAX_BATCH = 5000
	DECLARE LONG	CONSTANT MAX_GRAND = 5000

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
	!	^*(01) Process Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Process Batch Number\* enters the number of the
	!	batch which the user desires to remove.
	!	.lm -5
	!
	! Index:
	!
	!--
	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 6%)
	FROM_YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		TRM$(RIGHT(UTL_REPORTX::OPTDEF(2%), 5%))

	!++
	! Abstract:FLD03
	!	^*(03) Accounting Period\*
	!	.b
	!	.lm +5
	!	The ^*Account Period\* enters the period from which
	!	the identified batch will be removed.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Accounting Period
	!	.x Period>Accounting
	!
	!--

	%PAGE

300	!******************************************************************
	! Open all files
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		CONTINUE 320
	END WHEN

	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #2% EQ GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 320
	END WHEN

310	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 320
	END WHEN

	GOTO 320 IF AP_OPEN::BATCH <> GL_BATCH$

	DELETE #AP_OPEN.CH%

 !	PRINT ".";

	GOTO 310

320	CLOSE #AP_OPEN.CH%


400	!******************************************************************
	! Process distributation
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.MOD"
	USE
		CONTINUE 420 IF ERR = 155%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #1% EQ GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 420 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 420 IF ERR = 11%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	IF AP_OPEN_DIST::BTHNUM = GL_BATCH$
	THEN
		DELETE #AP_OPEN_DIST.CH%
		GOTO 410
	END IF

420	CLOSE #AP_OPEN_DIST.CH%


	%PAGE

500	!*******************************************************************
	! PO Register
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.MOD"
	USE
		CONTINUE 500 IF ERR = 1%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		FIND #PO_REG_LINE.CH%, KEY #3% EQ GL_BATCH$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 520 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		GET #PO_REG_LINE%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 420 IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	IF PO_REG_LINE::BATCH = GL_BATCH$
	THEN
		DELETE #PO_REG_LINE.CH%
		GOTO 510
	END IF

520	CLOSE #PO_REG_LINE.CH%

600	!*******************************************************************
	! Get the current period file
	!*******************************************************************

630	YYYY_PP$ = FROM_YYYY_PP$

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		CONTINUE 690
	END WHEN

	FROM_GL_YYYY_PP.CH% = GL_YYYY_PP.CH%
	GL_YYYY_PP.CH% = 0%

690	!

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "GL Transfer Erase Transmittal"
	TITLE$(2%) = "From Period " + FROM_YYYY_PP$
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
		FIND #FROM_GL_YYYY_PP.CH%, KEY #4% GE GL_BATCH$
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

	WHEN ERROR IN
		GET #FROM_GL_YYYY_PP.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 17200
	END WHEN

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
