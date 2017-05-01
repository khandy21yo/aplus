1	%TITLE "Inventory Ledger Closing Program"
	%SBTTL "IC_CLOS_CLOSE"
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
	!	The ^*Close Ledger for Period\* routine must be executed after
	!	the completion of each accounting period. The purpose of the
	!	routine is to advance the system to the next accounting period.
	!	.b
	!	The system will not allow any transactions to be posted to a
	!	closed period, nor can any ledger or balance reports be printed
	!	for a closed period. (See the ^*Reset\* routine for information
	!	on re-opening a closed ledger.)
	!	.lm -5
	!
	! Index:
	!	.x Close>Ledger for Period
	!	.x Ledger for Period>Close
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_CLOS_CLOSE/LINE
	!	$ LINK/EXE=IC_EXE: IC_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	09/23/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	12/26/90 - Frank F. Starman
	!		Remove history file. Change posting routine, because
	!		IC_35BALANCE file has been change to IC_35BALANCE.
	!
	!	01/18/91 - Frank F. Starman
	!		Modify for a new IC_HISTORY file layout.
	!
	!	01/10/92 - Frank F. Starman
	!		Modify for a new IC_35HISTORY file layout.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/23/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/30/92 - Frank F. Starman
	!		Modify for a new IC_HISTORY file layout.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/08/93 - Frank F. Starman
	!		Fixed bug while update to IC_35HISTORY.
	!
	!	02/24/93 - Dan Perkins
	!		Added "IC" to error text messages so user would
	!		know from what system they come.  Cleaned program code.
	!
	!	02/26/93 - Kevin Handy
	!		Modified to do a GET on the CONTROL file before
	!		doing an UPDATE, so that it would not crash
	!		with a "NO CURRENT RECORD" error EVERY time.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/06/95 - Kevin Handy
	!		Added trap for error 154 (Record locked).
	!		Modified several lines so resume from this error
	!		would work properly.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check).
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG		EXIT_STATUS

	DECLARE STRING		TITLE(10%)
	DECLARE STRING		BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION ASSG_POSTBATCH

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	!
	! Title
	!
	TITLE(1%) = "INVENTORY CLOSING PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""
	TITLE(4%) = "."

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.CRE"

		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CONTROLFLAG$ = IC_CONTROL::CONTROLFLAG

	!
	! Assign the current period
	!
	YYYYPP$ = IC_CONTROL::PERIOD

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0", "1", "5"

		V% = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "IC_CLOSE", "", "", "")

		!
		! Read the info about the current period
		!
		IF READ_PERIOD("READ", IC_CONTROL::ERA, YYYYPP$, &
			PERIOD_DESCR$, STAT$, START_DATE$, &
			END_DATE$, 0%)
		THEN
			TEXT$ = "%Can't read the current IC period " + &
				IC_CONTROL::PERIOD

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted
		END IF

		CUR_PERIOD$ = YYYYPP$

		!
		! Grab the next period, which becomes the current one
		!
		IF READ_PERIOD("FIND", IC_CONTROL::ERA, CUR_PERIOD$, &
			"", STAT$, "", "", 1%)
		THEN
			TEXT$ = "%Next IC period after " + YYYYPP$ + &
				" is not open."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted
		END IF

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

310	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open inventory history file
	!
	YYYY$ = LEFT(YYYYPP$, 4%)
	IND%  = VAL%(RIGHT(YYYYPP$, 5%))

	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"
	USE
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted &
		IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_CLOSE", "", YYYYPP$, CUR_PERIOD$) <> &
		CMC$_NORMAL

1000	!
	! Set close flag in control file
	!
	IC_CONTROL::CONTROLFLAG = "5"

	WHEN ERROR IN
		UPDATE #IC_CONTROL.CH%
		UNLOCK #IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

1010	!
	! Reset to beginning of current balance file
	!
	WHEN ERROR IN
		RESET #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "IC_35BALANCE", "", YYYYPP$, "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, "")

 SetBegBal:
1100	WHEN ERROR IN
		GET #IC_35BALANCE.CH%
	USE
		CONTINUE 1170 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	SELECT CONTROLFLAG$

	CASE "1"
		IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE - &
			IC_35BALANCE::CBALANCE

		IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE + &
			IC_35BALANCE::CBALANCE

		IC_35BALANCE::CBALANCE = 0.0

	CASE ELSE
		IC_35BALANCE::CBALANCE = 0.0

	END SELECT

1150	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO SetBegBal

1170	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, "")

	!
	! Set close flag in control file
	!
	WHEN ERROR IN
		GET #IC_CONTROL.CH%, RECORD 1%

		IC_CONTROL::CONTROLFLAG = "1"

		UPDATE #IC_CONTROL.CH%
		UNLOCK #IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

1180	WHEN ERROR IN
		RESET #IC_35HISTORY.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "IC_35BALANCE", "", YYYYPP$, "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, "")

 HistRec:
1190	WHEN ERROR IN
		GET #IC_35HISTORY.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = 0.0
	IC_35HISTORY::PRICEAMT(IND%) = 0.0
	IC_35HISTORY::COSTAMT(IND%) = 0.0

	WHEN ERROR IN
		UPDATE #IC_35HISTORY.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO HistRec

1200	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, "")

	!
	! Read posting quantities from the transaction file
	!
	EXIT_STATUS = CMC$_WARNING

	!
	! Open Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		CONTINUE 1390 IF ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "IC_TRANSACTION_" + YYYYPP$ + ".LED", &
		"", YYYYPP$, CUR_PERIOD$)

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, CUR_PERIOD$)

	WHEN ERROR IN
		RESET #IC_TRANSACTION.CH%
	USE
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
1210	!
	! Main loop starts here
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 1390 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN


1230	GOTO 1240 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.0

1235	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + &
		IC_TRANSACTION::QUANTITY_A

	IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE - &
		IC_TRANSACTION::QUANTITY_A

	IC_35BALANCE::CBALANCE = IC_35BALANCE::CBALANCE + &
		IC_TRANSACTION::QUANTITY_A

	UPDATE #IC_35BALANCE.CH%

	GOTO 1240

 AddA:
	IC_35BALANCE::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35BALANCE::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_TRANSACTION::TYPE_A
	IC_35BALANCE::RBALANCE	= 0.0

	IC_35BALANCE::BBALANCE	= IC_TRANSACTION::QUANTITY_A
	IC_35BALANCE::PBALANCE	= 0.0
	IC_35BALANCE::CBALANCE	= IC_TRANSACTION::QUANTITY_A

	PUT #IC_35BALANCE.CH%

1240	GOTO 1330 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.0

1250	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AddB IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + &
		IC_TRANSACTION::QUANTITY_B

	IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE - &
		IC_TRANSACTION::QUANTITY_B

	IC_35BALANCE::CBALANCE = IC_35BALANCE::CBALANCE + &
		IC_TRANSACTION::QUANTITY_B

	UPDATE #IC_35BALANCE.CH%

	GOTO 1330

 AddB:
	IC_35BALANCE::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35BALANCE::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_TRANSACTION::TYPE_B
	IC_35BALANCE::RBALANCE	= 0.0

	IC_35BALANCE::BBALANCE	= IC_TRANSACTION::QUANTITY_B
	IC_35BALANCE::PBALANCE	= 0.0
	IC_35BALANCE::CBALANCE	= IC_TRANSACTION::QUANTITY_B

	PUT #IC_35BALANCE.CH%

1330	GOTO 1340 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.0

1335	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AddAHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = IC_35HISTORY::PQUANTITY(IND%) + &
		IC_TRANSACTION::QUANTITY_A

	IC_35HISTORY::PRICEAMT(IND%)  = IC_35HISTORY::PRICEAMT(IND%) + &
		IC_TRANSACTION::PRICE

	IC_35HISTORY::COSTAMT(IND%)   = IC_35HISTORY::COSTAMT(IND%) + &
		IC_TRANSACTION::COST

	UPDATE #IC_35HISTORY.CH%

	GOTO 1340

 AddAHist:
	IC_35HISTORY::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35HISTORY::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35HISTORY::CROSSREF	= IC_TRANSACTION::CROSS_REF
	IC_35HISTORY::SUBACCT	= IC_TRANSACTION::SUBACCOUNT
	IC_35HISTORY::TRANSTYPE	= IC_TRANSACTION::TYPE_A

	FOR I% = 0% TO 12%

		IC_35HISTORY::PQUANTITY(I%)	= 0.0
		IC_35HISTORY::PRICEAMT(I%)	= 0.0
		IC_35HISTORY::COSTAMT(I%)	= 0.0

	NEXT I%

	IC_35HISTORY::PQUANTITY(IND%) = IC_TRANSACTION::QUANTITY_A
	IC_35HISTORY::PRICEAMT(IND%)  = IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = IC_TRANSACTION::COST

	PUT #IC_35HISTORY.CH%

1340	GOTO 1350 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.0

1345	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AddBHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = IC_35HISTORY::PQUANTITY(IND%) + &
		IC_TRANSACTION::QUANTITY_B

	IC_35HISTORY::PRICEAMT(IND%)  = IC_35HISTORY::PRICEAMT(IND%) + &
		IC_TRANSACTION::PRICE

	IC_35HISTORY::COSTAMT(IND%)   = IC_35HISTORY::COSTAMT(IND%) + &
		IC_TRANSACTION::COST

	UPDATE #IC_35HISTORY.CH%

	GOTO 1350

 AddBHist:
	IC_35HISTORY::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35HISTORY::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35HISTORY::CROSSREF	= IC_TRANSACTION::CROSS_REF
	IC_35HISTORY::SUBACCT	= IC_TRANSACTION::SUBACCOUNT
	IC_35HISTORY::TRANSTYPE	= IC_TRANSACTION::TYPE_B

	FOR I% = 0% TO 12%

		IC_35HISTORY::PQUANTITY(I%)	= 0.0
		IC_35HISTORY::PRICEAMT(I%)	= 0.0
		IC_35HISTORY::COSTAMT(I%)	= 0.0

	NEXT I%

	IC_35HISTORY::PQUANTITY(IND%) = IC_TRANSACTION::QUANTITY_B
	IC_35HISTORY::PRICEAMT(IND%)  = IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = IC_TRANSACTION::COST

	PUT #IC_35HISTORY.CH%

1350	GOTO GetNextRec

1390	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$, CUR_PERIOD$) &
		IF EXIT_STATUS = CMC$_NORMAL

	!
	! Update period file
	!
	GET #IC_CONTROL.CH%, RECORD 1%

	IC_CONTROL::PERIOD = CUR_PERIOD$

	IC_CONTROL::CONTROLFLAG = "0"

	UPDATE #IC_CONTROL.CH%

 Completed:
	!
	! Complete closing process and remove batch control
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "",YYYYPP$, CUR_PERIOD$)

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
		UTL_REPORTX, "", "", YYYYPP$, CUR_PERIOD$)

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
