1	%TITLE "Inventory Ledger Closing Program"
	%SBTTL "IC_SPEC_REGENBALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Recalculate the beginning balances in the IC_35BALANCE file.
	!	.lm -5
	!
	! Index:
	!	.x Recalculate Balances
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_REGENBALANCE/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_REGENBALANCE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_REGENBALANCE.OBJ;*
	!
	! Author:
	!
	!	10/26/2004 - Kevin Handy
	!
	! Modification history:
	!
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

	DECLARE LONG		EXIT_STATUS

	DECLARE STRING		TITLE(10%)

	!
	! Dimension statements
	!
	DIM IC_TRANSACTION_FILE$(1000)

	%PAGE

	ON ERROR GOTO 19000

	INPUT "DANGEROUS PROGRAM. DO YOU WANT TO CONTINUE (N/y)"; CONFIRM$
	CONFIRM$ = LEFT(EDIT$(CONFIRM$, -1%), 1%)
	IF CONFIRM$ <> "Y"
	THEN
		EXIT PROGRAM
	END IF

	CALL READ_INITIALIZE

	!
	! Get info required for main file
	!
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), 16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))

	IF IC_TRANSACTION_FILE%
	THEN
		IC_TRANSACTION_FILE$(LOOP%) = &
			MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%) &
			FOR LOOP% = 1% TO IC_TRANSACTION_FILE%
	ELSE
		GOTO ExitProgram
	END IF

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
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CONTROLFLAG$ = IC_CONTROL::CONTROLFLAG

	!
	! Assign the current period
	!
	CONTROL_YYYYPP$ = IC_CONTROL::PERIOD

310	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.MOD"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

320	!

1000	!

1010	!
	! Reset to beginning of current balance file
	!
	WHEN ERROR IN
		RESET #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

 SetBegBal:
1100	WHEN ERROR IN
		GET #IC_35BALANCE.CH%
	USE
		CONTINUE 1170 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::BBALANCE = 0.0

1150	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO SetBegBal

1170	!
	FOR FILE_LOOP% = 1% TO IC_TRANSACTION_FILE%

		YYYYPP$ = IC_TRANSACTION_FILE$(FILE_LOOP%)
		IF CONTROL_YYYYPP$ > YYYYPP$
		THEN
			PRINT "Starting: "; YYYYPP$
			GOSUB 1200
		END IF

	NEXT FILE_LOOP%

	GOTO Completed

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
		CONTINUE 1390 IF ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

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
		GET #IC_35BALANCE.CH%, &
			KEY #0% EQ IC_TRANSACTION::PRODUCT + &
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
		GET #IC_35BALANCE.CH%, &
			KEY #0% EQ IC_TRANSACTION::PRODUCT + &
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
	GOTO GetNextRec

1390	!
	CLOSE IC_TRANSACTION.CH%
	RETURN


 Completed:
	!

 ExitProgram:
	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************

	GOTO ExitProgram

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	PRINT "ERROR OCCURRED "; ERL; " "; ERT; " "; ERT$(ERL); " "; ERN$

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
