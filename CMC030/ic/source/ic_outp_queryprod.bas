1	%TITLE "Product Transaction Scan Report"
	%SBTTL "IC_OUTP_QUERYPROD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_OUTP_QUERYPROD(PD_PRODUCT_CDD PD_PRODUCT, &
		UTL_LOCATION_CDD UTL_LOCATION, STRING CURPERIOD)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints Inventory product scan report
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_OUTP_QUERYPROD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_OUTP_QUERYPROD
	!	$ DELETE IC_OUTP_QUERYPROD.OBJ;*
	!
	! Author:
	!
	!	08/10/88 - Frank Starman
	!
	! Modification History:
	!
	!	12/23/91 - Dan Perkins
	!		Removed function UTL_READ_TRANSTYPE.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/10/92 - Frank F. Starman
	!		Print running and posted balances.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/09/93 - Frank F. Starman
	!		Round balances before print warning message.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/29/93 - Dan Perkins
	!		Print tran type A and qty A to display
	!		returns from sales.
	!
	!	06/01/93 - Dan Perkins
	!		Declare FUNC_ROUND in external functions.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	11/28/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	08/30/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/29/2001 - Kevin Handy
	!		Increase dimension for files from 100 to 300
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	DIM IC_TRANSACTION_FILE$(300%)

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "IC_OUTP_QUERYPROD"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	IC_OUTP_QUERYPROD = 0%
	REPORT$ = "IC055"

400	!
 SetScreen:
	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

510	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
	USE
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Set up from user input
	!
	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field enters the period with which
	!	the report will end. A blank field causes the report to end with
	!	the last period in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Period
	!
	!--

 FindTrans:
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		IC_TRANSACTION_FILE$(), &
		16%, "", "")

	IC_TRANSACTION_FILE% = VAL%(IC_TRANSACTION_FILE$(0%))

	IF IC_TRANSACTION_FILE%
	THEN
		IC_TRANSACTION_FILE$(LOOP%) = &
			MID(IC_TRANSACTION_FILE$(LOOP%), 16%, 6%) &
				FOR LOOP% = 1% TO IC_TRANSACTION_FILE%
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find any inventory ledger file", 0%)
		GOTO ExitFunction
	END IF

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = TRM$(PD_PRODUCT::PRODUCT_NUM) + "  " + &
		TRM$(PD_PRODUCT::DESCRIPTION) + "  AT  LOCATION  " + &
		TRM$(UTL_LOCATION::LOCATION) + "  " + &
		TRM$(UTL_LOCATION::LOCNAME)

	TITLE$(2%) = "Inventory Control System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "TransDate  PostDate   Batch# PrimRef          " + &
		"CrossRef   Subacct         Onhand       Alloc" + &
		"     OnOrder    TranCode         Qty"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! Read balance file
	!
	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%)
	ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%)
	ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%)

	TEXT_TEXT$ = PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 8%) + " " + &
		PRNT_DATE(IC_TRANSACTION::POSTDATE, 8%) + " " + &
		IC_TRANSACTION::BATCH + " " + &
		IC_TRANSACTION::PRIMARY_REF + " " + &
		IC_TRANSACTION::CROSS_REF + " " + &
		IC_TRANSACTION::SUBACCOUNT

	TEXT$ = "           Current Running Balance" + &
		STRING$(LEN(TEXT_TEXT$) - 33%, A"."B) + &
		FORMAT$(ONHAND + BALANCE(1%, 3%), "########.##") + " " + &
		FORMAT$(ALLOC + BALANCE(2%, 3%), "########.##") + " " + &
		FORMAT$(ONORDER + BALANCE(3%, 3%), "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &

	TEXT$ = "           Current Posted Balance " + &
		STRING$(LEN(TEXT_TEXT$) - 33%, A"."B) + &
		FORMAT$(ONHAND, "########.##") + " " + &
		FORMAT$(ALLOC, "########.##") + " " + &
		FORMAT$(ONORDER, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &

	FOR LOOP% = IC_TRANSACTION_FILE% TO 1% STEP -1%

		YYYYPP$ = IC_TRANSACTION_FILE$(LOOP%)

		GOTO ExitFunction IF TO_PERIOD$ <> "" AND &
			TO_PERIOD$ > YYYYPP$

		CLOSE IC_TRANSACTION.CH%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
		USE
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

17010		WHEN ERROR IN
			FIND #IC_TRANSACTION.CH%, &
				KEY #0% GE PD_PRODUCT::PRODUCT_NUM + &
				UTL_LOCATION::LOCATION, &
				REGARDLESS

		USE
			CONTINUE 17330 IF ERR = 155%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

 GetNextRec:
17020		!
		! Main loop starts here
		!
		GOTO ExitFunction IF UTL_REPORTX::STAT

		!
		! Get next record
		!
		WHEN ERROR IN
			GET #IC_TRANSACTION.CH%, REGARDLESS
		USE
			CONTINUE 17330 IF ERR = 11%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		!
		! Check current record
		!
		GOTO NextPeriod IF IC_TRANSACTION::PRODUCT <> &
			PD_PRODUCT::PRODUCT_NUM OR &
			IC_TRANSACTION::LOCATION <> UTL_LOCATION::LOCATION

		QTY(I%) = 0.0 FOR I% = 1% TO 3%

17300		WHEN ERROR IN
			GET #UTL_TRANSTYPE.CH%, &
				KEY #0% EQ IC_TRANSACTION::TYPE_A, &
				REGARDLESS
		USE
			CONTINUE 17310 IF ERR = 155%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN

		I% = VAL%(UTL_TRANSTYPE::CLASS)
		QTY(I%) = IC_TRANSACTION::QUANTITY_A

17310		IF IC_TRANSACTION::TYPE_B <> ""
		THEN
			WHEN ERROR IN
				GET #UTL_TRANSTYPE.CH%, &
					KEY #0% EQ IC_TRANSACTION::TYPE_B, &
					REGARDLESS
			USE
				CONTINUE 17320 IF ERR = 155%
				FILENAME$ = "UTL_TRANSTYPE"
				CONTINUE HelpError
			END WHEN

			I% = VAL%(UTL_TRANSTYPE::CLASS)
			QTY(I%) = QTY(I%) + IC_TRANSACTION::QUANTITY_B
		END IF

17320		TEXT$ = PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 8%) + " " + &
			PRNT_DATE(IC_TRANSACTION::POSTDATE, 8%) + " " + &
			IC_TRANSACTION::BATCH + " " + &
			IC_TRANSACTION::PRIMARY_REF + " " + &
			IC_TRANSACTION::CROSS_REF + " " + &
			IC_TRANSACTION::SUBACCOUNT + " " + &
			FORMAT$(QTY(1%), "<%>#######.##") + " " + &
			FORMAT$(QTY(2%), "<%>#######.##") + " " + &
			FORMAT$(QTY(3%), "<%>#######.##")

		IF QTY(1%) = 0.0 AND QTY(2%) = 0.0 AND QTY(3%) = 0.0
		THEN
			TEXT$ = TEXT$ + "    " + &
				IC_TRANSACTION::TYPE_A + "       " + &
				FORMAT$(IC_TRANSACTION::QUANTITY_A, &
				"<%>#######.##")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		ONHAND = ONHAND - QTY(1%)
		ALLOC = ALLOC  - QTY(2%)
		ONORDER = ONORDER- QTY(3%)

		GOTO GetNextRec

 NextPeriod:
17330		TEXT$ = "           Beginning Balance For Period " + YYYYPP$ + &
			STRING$(LEN(TEXT_TEXT$) - 45%, A"."B) + &
			FORMAT$(ONHAND, "########.##") + " " + &
			FORMAT$(ALLOC, "########.##") + " " + &
			FORMAT$(ONORDER, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%) &

		IF YYYYPP$ = CURPERIOD OR &
			YYYYPP$ < CURPERIOD AND CHECK.BAL% = 0%
		THEN
			CHECK.BAL% = -1%

			IF FUNC_ROUND(ONHAND - BALANCE(1%, 1%), 2%) <> 0.0 OR &
				FUNC_ROUND(ALLOC  - BALANCE(2%, 1%), 2%) <> 0.0 OR &
				FUNC_ROUND(ONORDER - BALANCE(3%, 1%), 2%) <> 0.0
			THEN
				TEXT$ = "           This is correct Beg " + &
					"Balance. Resynchronize Balances!" + &
				STRING$(LEN(TEXT_TEXT$) - 62%, A":"B) + &
				FORMAT$(BALANCE(1%, 1%), "########.##") + " " + &
				FORMAT$(BALANCE(2%, 1%), "########.##") + " " + &
				FORMAT$(BALANCE(3%, 1%), "########.##")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

				ONHAND = BALANCE(1%, 1%)
				ALLOC = BALANCE(2%, 1%)
				ONORDER = BALANCE(3%, 1%)
			END IF
		END IF

	NEXT LOOP%

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE UTL_TRANSTYPE.CH%
	CALL ASSG_FREECHANNEL(UTL_TRANSTYPE.CH%)

	CLOSE IC_TRANSACTION.CH%
	CALL ASSG_FREECHANNEL(IC_TRANSACTION.CH%)

 Exit1:
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
