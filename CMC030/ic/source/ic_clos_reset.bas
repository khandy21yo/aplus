1	%TITLE "Reset Inventory Ledger"
	%SBTTL "IC_CLOS_RESET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	The ^*Reset Ledger for Period\* routine re-opens an Inventory
	!	period file which has previously been closed. Resetting a period
	!	file would permit the posting and printing of additional journals.
	!	.b
	!	Successive Inventory period files may be re-opened, including the
	!	period files of the previous fiscal year, by repeatedly executing
	!	the ^*Reset\* routine.
	!	.b
	!	The system is returned to the current period file by closing
	!	each file which has been reset.
	!	.b
	!	^*Note:\* All reports affected by any changes made should be reprinted
	!	before closing.
	!	.lm -5
	!
	! Index:
	!	.x Reset>Ledger for Period
	!	.x Ledger for Period>Reset
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_CLOS_RESET/LINE
	!	$ LINK/EXE=IC_EXE: IC_CLOS_RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_CLOS_RESET.OBJ;*
	!
	! Author:
	!
	!	06/02/88 - Frank Starman
	!
	! Modification history:
	!
	!	01/09/91 - Frank F. Starman
	!		Completly change reset program.
	!
	!	01/12/92 - Frank F. Starman
	!		Remove IC_HISTORY.
	!
	!	07/30/92 - Frank F. Starman
	!		Do not create temporary file.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/24/93 - Dan Perkins
	!		Added "IC" to error text messages so user would
	!		know from what system they come.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/06/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG	EXIT_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	!
	! Title
	!
	TITLE(1%) = "INVENTORY  RESET  PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.UPD"
		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CONTROLFLAG$ = IC_CONTROL::CONTROLFLAG
	CUR.PERIOD$, YYYYPP$ = IC_CONTROL::PERIOD

	!
	! Grab the prior period, which becomes the current one
	!
	IF READ_PERIOD("FIND", IC_CONTROL::ERA, CUR.PERIOD$, &
		"", STAT$, "", "", -1%)
	THEN
		TEXT$ = "%Prior IC period before " + YYYYPP$ + &
			" doesn't exist."

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted
	END IF

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0", "2"
		!
		! Open up batch control file and check if interrupted
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, &
			BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "IC_RESET", "", "", "")

		!
		! Grab the new current period
		!
		IF READ_PERIOD("READ", IC_CONTROL::ERA, CUR.PERIOD$, &
			PERIOD_DESCR$, STAT$, START_DATE$, &
			END_DATE$, 0%)
		THEN
			TEXT$ = "%Undefined IC period " + CUR.PERIOD$
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted
		END IF

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted
	END SELECT

320	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_RESET", "RESET", &
		YYYYPP$, CUR.PERIOD$) <> CMC$_NORMAL

1000	!
	! Set close flag in control file
	!
	IC_CONTROL::CONTROLFLAG = "2"

	WHEN ERROR IN
		UPDATE #IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYYPP$ = CUR.PERIOD$

1100	!
	! Set beginning balances for the current period
	!
	WHEN ERROR IN
		RESET #IC_35BALANCE.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

 SetBegBal:
	WHEN ERROR IN
		GET #IC_35BALANCE.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	SELECT CONTROLFLAG$

	CASE "0"
		IC_35BALANCE::CBALANCE = 0.0
	CASE "2"
		IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + &
			IC_35BALANCE::CBALANCE
		IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE - &
			IC_35BALANCE::CBALANCE
		IC_35BALANCE::CBALANCE = 0.0
	END SELECT

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO SetBegBal

1200	!
	! Read posting quantities from the transaction file
	!
	EXIT_STATUS = CMC$_WARNING
	!
	! Open Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		CONTINUE 1290 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, &
		"IC_TRANSACTION_" + YYYYPP$ + ".LED", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", &
		"",  "")

	WHEN ERROR IN
		RESET #IC_TRANSACTION.CH%
	USE
		CONTINUE 1290 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 1290 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

1230	GOTO 1240 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.0

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A
	USE
		CONTINUE AddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE - &
		IC_TRANSACTION::QUANTITY_A
	IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE + &
		IC_TRANSACTION::QUANTITY_A
	IC_35BALANCE::CBALANCE = IC_35BALANCE::CBALANCE - &
		IC_TRANSACTION::QUANTITY_A

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 1240

 AddA:
	IC_35BALANCE::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35BALANCE::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_TRANSACTION::TYPE_A
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= IC_TRANSACTION::QUANTITY_A
	IC_35BALANCE::RBALANCE	= 0.0
	IC_35BALANCE::CBALANCE	= 0.0

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1240	GOTO 1250 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.0

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B
	USE
		CONTINUE AddB IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::BBALANCE	= IC_35BALANCE::BBALANCE - &
		IC_TRANSACTION::QUANTITY_B
	IC_35BALANCE::PBALANCE	= IC_35BALANCE::PBALANCE + &
		IC_TRANSACTION::QUANTITY_B
	IC_35BALANCE::CBALANCE	= IC_35BALANCE::CBALANCE - &
		IC_TRANSACTION::QUANTITY_B

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 1250

 AddB:
	IC_35BALANCE::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35BALANCE::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_TRANSACTION::TYPE_B
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= IC_TRANSACTION::QUANTITY_B
	IC_35BALANCE::RBALANCE	= 0.0
	IC_35BALANCE::CBALANCE	= 0.0

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1250	GOTO GetNextRec

1290	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "") &
		IF EXIT_STATUS = CMC$_NORMAL

1500	!
	! Update period file
	!
	GET #IC_CONTROL.CH%, RECORD 1%

	IC_CONTROL::PERIOD	= CUR.PERIOD$
	IC_CONTROL::CONTROLFLAG = "0"

	UPDATE #IC_CONTROL.CH%

 Completed:
	!
	! Complete closing process and remove batch control
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", &
		"", "")
	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)
	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:FLD01
	!	^*(01) Reset to Period\*
	!	.b
	!	.lm +5
	!	The ^*Reset to Period\* field resets
	!	to a prior accounting period.
	!	.lm -5
	!
	! Index:
	!	.x Reset to Period
	!
	!--
