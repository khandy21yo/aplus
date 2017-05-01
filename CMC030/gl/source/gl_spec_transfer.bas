1	%TITLE "Copy GL in Summary to Another GL File"
	%SBTTL "GL_SPEC_TRANSFER"
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
	! ID:GLTRAN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Copy General Ledger
	!	transfers data from one
	!	General Ledger file into another General Ledger file.  Detail records are not
	!	transferred, only the net changes or summary amounts for each system generated
	!	batch are transferred.
	!	.b
	!	Several objectives may be met by the utilization of this option.  One concern
	!	may be having detail payroll information in the "master" General Ledger".  By
	!	posting detail payroll data into a "payroll" General Ledger and transferring
	!	the data in summary into the "master" General Ledger, personnel authorized
	!	access to "master" General Ledger data are denied access to any payroll detail
	!	data.  Another objective may be met by maintaining "sub" General Ledgers for
	!	cost centers.  "Sub" General Ledgers could be posted in summary into a "master"
	!	or "consolidated" General Ledger.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_TRANSFER/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_TRANSFER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_TRANSFER.OBJ;*
	!
	! Author:
	!
	!	06/29/89 - Kevin Handy
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	09/28/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

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
	DIM TOTAL_RECORD BATCH_TOTAL(MAX_BATCH)

	!
	! Declare some variables
	!
	DECLARE	STRING	CURRENT_YEAR, CURRENT_PERIOD

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

	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(2%), 5%)

	!++
	! Abstract:FLD03
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	The accounting period that is to be considered in the running of the
	!	desired program is entered in this field.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	CURRENT_YEAR = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD = RIGHT(YYYY_PP$, 6%)
	TOTAL_BATCH% = 0%

	%PAGE

	!******************************************************************
	! Open all files
	!******************************************************************

	!
	! Get the current period file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

320	!
	! Open up GL_YYYY_PP file to transfer to
	!
	!	(There should be a better way to do this)
	!
	!======================================================================
	! GL_YYYY_PP file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_YYYY_PP_TRANSFER.CH%, STAT%)
	CALL READ_DEVICE("GL_YYYY_PP_TRANSFER", GL_YYYY_PP_TRANSFER.DEV$, STAT%)
	CALL READ_PROTECTION("GL_YYYY_PP", GL_YYYY_PP.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(GL_YYYY_PP.PRO$, STAT%)

	GL_YYYY_PP_TRANSFER.NAME$ = GL_YYYY_PP_TRANSFER.DEV$ + &
		"GL_" + YYYY_PP$ + ".LED"

	OPEN GL_YYYY_PP_TRANSFER.NAME$ AS FILE GL_YYYY_PP_TRANSFER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_YYYY_PP, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			GL_YYYY_PP::ACCT, &
			GL_YYYY_PP::TRANDAT &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::SUBACC, &
			GL_YYYY_PP::OPERATION, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::XREFNO, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::CKNO, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_YYYY_PP::BTHNUM &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "GL Transfer Transmittal"
	TITLE$(2%) = "Period " + CURRENT_PERIOD + " of Year " + CURRENT_YEAR
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "Batch"
	TITLE$(5%) = ""

	!
	! Layouts for some of the printed lines
	!

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	THIS_BATCH$ = "      "

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get start of next batch
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, KEY #4% GT THIS_BATCH$, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	THIS_BATCH$ = GL_YYYY_PP::BTHNUM

17110	!
	! See if this one already exists in the transfer file.
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP_TRANSFER.CH%, KEY #4% EQ THIS_BATCH$, REGARDLESS
	USE
		CONTINUE 17120
	END WHEN

	GOTO GetNextRec

17120	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17200 IF (GL_YYYY_PP::BTHNUM <> THIS_BATCH$)

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

17180	!
	! Try for next record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	GOTO 17120

	%PAGE

17200	!*******************************************************************
	! Read entire batch, now add data to transfer file
	!*******************************************************************

	FOR I% = 1% TO TOTAL_BATCH%

		GL_YYYY_PP::ACCT	= BATCH_TOTAL(I%)::ACCT
		GL_YYYY_PP::SOURCE	= BATCH_TOTAL(I%)::SOURCE
		GL_YYYY_PP::REFNO	= ""
		GL_YYYY_PP::TRANDAT	= BATCH_TOTAL(I%)::TRANDAT
		GL_YYYY_PP::DESCR	= "Summary"
		GL_YYYY_PP::AMOUNT	= BATCH_TOTAL(I%)::AMOUNT
		GL_YYYY_PP::XREFNO	= ""
		GL_YYYY_PP::POSTIM	= ""
		GL_YYYY_PP::POSDAT	= ""
		GL_YYYY_PP::CKNO	= ""
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= ""
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= BATCH_TOTAL(I%)::UNITS
		GL_YYYY_PP::HOURS	= BATCH_TOTAL(I%)::HOURS
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= THIS_BATCH$

		PUT #GL_YYYY_PP_TRANSFER.CH%

	NEXT I%

	TEXT$ = THIS_BATCH$ + "   Transfered, " + &
		NUM1$(TOTAL_BATCH%) + " summary records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_BATCH% = 0%

	GOTO GetNextRec

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

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report GL_SPEC_TRANSFER
	!******************************************************************
	END
