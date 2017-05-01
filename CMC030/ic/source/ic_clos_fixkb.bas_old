1	%TITLE "Inventory Ledger Closing Program"
	%SBTTL "IC_CLOS_FIXKB"
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
	!	$ BAS IC_SOURCE:IC_CLOS_FIXKB/LINE
	!	$ LINK/EXE=IC_EXE: IC_CLOS_FIXKB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_CLOS_FIXKB.OBJ;*
	!
	! Author:
	!
	!	10/01/93 - Kevin Handy
	!		Taken from IC_CLOS_CLOSE.
	!
	! Modification history:
	!
	!	01/12/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG		EXIT_STATUS

	DECLARE STRING		TITLE(10%)
	DECLARE STRING		BATCH_NUMBER

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
	! Assign the current period
	!
	YYYYPP$ = "199308"

310	!
	! Open Balance file
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"

320	!
	! Open inventory history file
	!
	YYYY$ = LEFT(YYYYPP$, 4%)
	IND%  = VAL%(RIGHT(YYYYPP$, 5%))

1200	!
	! Read posting quantities from the transaction file
	!
	EXIT_STATUS = CMC$_WARNING

	!
	! Open Transaction file
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"

	RESET #IC_TRANSACTION.CH%

 GetNextRec:
	!
	! Main loop starts here
	!
	! Get next record
	!
	GET #IC_TRANSACTION.CH%, REGARDLESS

	!
	! Skip unless in location 101,105,104
	!
	GOTO GetNextRec UNLESS IC_TRANSACTION::LOCATION = "101 " OR &
		IC_TRANSACTION::LOCATION = "104 " OR &
		IC_TRANSACTION::LOCATION = "105 "

1230	GOTO 1240 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.

	GET #IC_35BALANCE.CH%, KEY#0% EQ IC_TRANSACTION::PRODUCT + &
		IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + &
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
		IC_TRANSACTION::QUANTITY_B = 0.

	GET #IC_35BALANCE.CH%, KEY#0% EQ IC_TRANSACTION::PRODUCT + &
		IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B

	IC_35BALANCE::BBALANCE = IC_35BALANCE::BBALANCE + &
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

1330	!

1350	GOTO GetNextRec

1390
 Completed:
	!
	! Complete closing process and remove batch control
	!

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

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	SELECT ERL

	CASE 310%
		FILENAME$ = "IC_35BALANCE"

	CASE 1200%
		RESUME 1390 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYYPP$

	CASE 1230% ! Record not found
		RESUME AddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"

	CASE 1240% ! Record not found
		RESUME AddB IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"

	END SELECT

	RESUME HelpError

32767	END
