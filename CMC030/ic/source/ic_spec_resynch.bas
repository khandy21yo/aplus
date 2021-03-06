1	%TITLE "Resynch Inventory Balance File"
	%SBTTL "IC_SPEC_RESYNCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Resynch Inventory Balance File\* routine is used to correct
	!	any file count problems (premature ejection from system being one)
	!	in the inventory balance file.
	!	.b
	!	The routine re-establishes the correct posted balance to the
	!	balance file and automatically resets the running balance as
	!	zero.
	!	.b
	!	^*Note:\* There can be ^*NO\* outstanding journals
	!	in the order entry system when you process the resynch
	!	procedure. Orders/shipping/invoicing journals should be posted
	!	prior to executing this process.
	!	.lm -5
	!
	! Index:
	!	.x Resynch>Inventory Balance File
	!	.x Inventory Balance File>Resynch
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_RESYNCH/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_RESYNCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_RESYNCH.OBJ;*
	!
	! Author:
	!
	!	01/14/91 - Val James Allen
	!
	! Modification history:
	!
	!	01/05/92 - Frank F. Starman
	!		Add IC_35HISTORY file.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	06/06/92 - Frank F. Starman
	!		Allow to deal only with one location.
	!		Allow to update only history file.
	!
	!	06/15/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/09/92 - Frank F. Starman
	!		Allow to deal only with some transaction codes
	!		Do not create a new file.
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/24/93 - Dan Perkins
	!		Added "IC" to error text messages so user would
	!		know from what system they come.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/08/93 - Kevin Handy
	!		Changed error trap for 1180 from "IC_35BALANCE"
	!		to "IC_35HISTORY".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/24/95 - Kevin Handy
	!		Remove unecessary external function def's
	!
	!	12/08/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/30/97 - Kevin Handy
	!		Clean up
	!
	!	07/30/97 - Kevin Handy
	!		Added a lot of comments.
	!		Use VAL% instead of VAL.
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	10/20/97 - Kevin Handy
	!		Look into IC Transaction Journals to update
	!		running balances.
	!
	!	11/13/97 - Kevin Handy
	!		Start on PS Journals updating running balances.
	!
	!	11/18/97 - Kevin Handy
	!		Fix up some LOCATION$ bugs.
	!
	!	11/26/97 - Kevin Handy
	!		Skip on any error opening IC_JOURNAL_?? files.
	!
	!	12/01/97 - Kevin Handy
	!		Allow IC transaction journals to have other than
	!		two character names.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/04/99 - Kevin Handy
	!		Change IC_CONTROL from .UPD to .MOD
	!
	!	02/14/2000 - Kevin Handy
	!		Modify to be able to look in other directories
	!		for journal files
	!
	!	04/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN for error trapping.
	!
	!	02/22/2000 - Kevin Handy
	!		Trap error 162 (Cannot open file) on OE_ORDERJOUR
	!
	!	03/07/2000 - Kevin Handy
	!		Add year to IC_35HISTORY file name in error messages.
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.HB"
	MAP (IC_JOURNAL)	IC_JOURNAL_CDD		IC_JOURNAL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	DECLARE LONG	EXIT_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes that are to be included in the process.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Inventory Resynchronize
	!
	!--

	ONLY.HIST$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Only History (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Only History\* field resynchronizes
	!	only the inventory history file.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Only History>Inventory Resynchronize
	!
	!--

	TRANSTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Transaction Types\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Types\* field enters the transaction
	!	codes that are to be included in the process.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Types>Inventory Resynchronize
	!
	!--

	TITLE(1%) = "INVENTORY  RESYNCH  PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_RESYNCH", "", "", "")

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.MOD"
		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CUR.PERIOD$, YYYYPP$ = IC_CONTROL::PERIOD

	EXIT_STATUS = CMC$_NORMAL

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0", "1", "4"
		!
		! Check the current period
		!
		IF READ_PERIOD("READ", IC_CONTROL::ERA, CUR.PERIOD$, &
			PERIOD_DESCR$, STAT$, START_DATE$, &
			END_DATE$, 0%)
		THEN
			TEXT$ = "%Undefined IC period " + CUR.PERIOD$

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_WARNING
		END IF

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_WARNING

	END SELECT

	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_RESYNCH", "RESYNCH", &
		CUR.PERIOD$, CUR.PERIOD$) <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	!
	! Set close flag in control file
	!
	IC_CONTROL::CONTROLFLAG = "4"
	WHEN ERROR IN
		UPDATE #IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Don't worry about balance file if resynch only history
	!
	GOTO 1170 IF ONLY.HIST$ = "Y"

320	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1000	!
	! Reset IC_35BALANCE to zero posted balance
	!
	WHEN ERROR IN
		RESET #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

1100	!
 SetBegBal:
	WHEN ERROR IN
		GET #IC_35BALANCE.CH%
	USE
		CONTINUE 1170 IF ERR = 11% OR ERR = 131%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1150	IF LOCATION$ = "" OR &
		COMP_STRING(EDIT$(IC_35BALANCE::LOCATION, -1%), LOCATION$)
	THEN
		IF TRANSTYPE$ = "" OR &
			COMP_STRING(EDIT$(IC_35BALANCE::TRANSTYPE, -1%), &
			TRANSTYPE$)
		THEN
			IC_35BALANCE::PBALANCE = 0.0
			IC_35BALANCE::RBALANCE = 0.0

			WHEN ERROR IN
				UPDATE #IC_35BALANCE.CH%
			USE
				FILENAME$ = "IC_35BALANCE"
				CONTINUE HelpError
			END WHEN
		END IF
	END IF

	GOTO SetBegBal

1170	!
	! Open IC_35HISTORY History files for open periods
	!
	TEMP.YYYYPP$ = YYYYPP$

	YEARS% = 0%
	NY% = 0%

	WHILE READ_PERIOD("FIND", IC_CONTROL::ERA, YYYYPP$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, NY%) = 0%

		NY% = 1%
		YYYY$ = LEFT(YYYYPP$, 4%)

		IF YYYY$ <> ARR.YYYY$(YEARS%)
		THEN
			YEARS% = YEARS% + 1%

			WHEN ERROR IN
				%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"
			USE
				FILENAME$ = "IC_35HISTORY_" + YYYY$
				CONTINUE HelpError
			END WHEN

			IC_35HISTORY.CH%(YEARS%) = IC_35HISTORY.CH%
			IC_35HISTORY.CH% = 0.0
			ARR.YYYY$(YEARS%) = YYYY$
			S.INDEX%(YEARS%) = VAL%(RIGHT(YYYYPP$, 5%))
		END IF

		E.INDEX%(YEARS%) = VAL%(RIGHT(YYYYPP$, 5%))
	NEXT

	FOR Y% = 1% TO YEARS%

1180		WHEN ERROR IN
			RESET #IC_35HISTORY.CH%(Y%)
		USE
			CONTINUE 1185 IF ERR = 11%
			FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
			CONTINUE HelpError
		END WHEN

 HistRec:
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%)
		USE
			CONTINUE 1185 IF ERR = 11%
			FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
			CONTINUE HelpError
		END WHEN

		IF LOCATION$ = "" OR &
			COMP_STRING(EDIT$(IC_35HISTORY::LOCATION, -1%), &
			LOCATION$)
		THEN
			IF TRANSTYPE$ = "" OR &
				COMP_STRING(EDIT$(IC_35HISTORY::TRANSTYPE, &
				-1%), TRANSTYPE$)
			THEN
				FOR IND% = S.INDEX%(Y%) TO E.INDEX%(Y%)
					IC_35HISTORY::PQUANTITY(IND%) = 0.0
					IC_35HISTORY::PRICEAMT(IND%) = 0.0
					IC_35HISTORY::COSTAMT(IND%) = 0.0
				NEXT IND%

				UPDATE #IC_35HISTORY.CH%(Y%)
			END IF
		END IF

		GOTO HistRec

1185	NEXT Y%

	YYYYPP$ = TEMP.YYYYPP$

1200	!
	! Read posting quantities from the transaction file
	!
	EXIT_STATUS = CMC$_WARNING

	!
	! Open Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "IC_TRANSACTION_" + YYYYPP$ + &
			".LED", "", YYYYPP$, "")

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "")

		RESET #IC_TRANSACTION.CH%
	USE
		CONTINUE 2000 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	IND% = VAL%(RIGHT(YYYYPP$, 5%))

	FOR Y% = 1% TO YEARS%
		GOTO GetNextRec IF ARR.YYYY$(Y%) = LEFT(YYYYPP$, 4%)
	NEXT Y%

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
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	IF LOCATION$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(IC_TRANSACTION::LOCATION, -1%), &
			LOCATION$) = 0%
	END IF

1230	!
	! Update IC_35BALANCE for transaction A
	!
	GOTO 1330 IF ONLY.HIST$ = "Y"

	GOTO 1240 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.0

	GOTO 1240 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_A, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A
	USE
		CONTINUE AddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE + &
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

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		CONTINUE AddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1240	!
	! Update IC_35BALANCE for Transaction B
	!
	GOTO 1330 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.0

	GOTO 1330 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_B, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B
	USE
		CONTINUE AddB IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::PBALANCE = IC_35BALANCE::PBALANCE + &
		IC_TRANSACTION::QUANTITY_B

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 1330

 AddB:
	IC_35BALANCE::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35BALANCE::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_TRANSACTION::TYPE_B
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= IC_TRANSACTION::QUANTITY_B
	IC_35BALANCE::RBALANCE	= 0.0

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		CONTINUE AddB IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

1330	!
	! Update IC_35HISTORY for Transaction A
	!
	GOTO 1340 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.

	GOTO 1340 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_A, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(Y%), KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		CONTINUE AddAHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = &
		IC_35HISTORY::PQUANTITY(IND%) + IC_TRANSACTION::QUANTITY_A
	IC_35HISTORY::PRICEAMT(IND%)  = &
		IC_35HISTORY::PRICEAMT(IND%) + IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = &
		IC_35HISTORY::COSTAMT(IND%) + IC_TRANSACTION::COST

	WHEN ERROR IN
		UPDATE #IC_35HISTORY.CH%(Y%)
	USE
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

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

	WHEN ERROR IN
		PUT #IC_35HISTORY.CH%(Y%)
	USE
		CONTINUE AddAHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

1340	!
	! Update IC_35HISTORY for Transaction B
	!
	GOTO 1350 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.

	GOTO 1350 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_B, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(Y%), KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		CONTINUE AddBHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = IC_35HISTORY::PQUANTITY(IND%) + &
		IC_TRANSACTION::QUANTITY_B

	IC_35HISTORY::PRICEAMT(IND%) = IC_35HISTORY::PRICEAMT(IND%) + &
		IC_TRANSACTION::PRICE

	IC_35HISTORY::COSTAMT(IND%) = IC_35HISTORY::COSTAMT(IND%) + &
		IC_TRANSACTION::COST

	WHEN ERROR IN
		UPDATE #IC_35HISTORY.CH%(Y%)
	USE
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

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

	WHEN ERROR IN
		PUT #IC_35HISTORY.CH%(Y%)
	USE
		CONTINUE AddBHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY_" + ARR.YYYY$(Y%)
		CONTINUE HelpError
	END WHEN

1350	GOTO GetNextRec

2000	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "") &
		IF EXIT_STATUS = CMC$_NORMAL

	GOTO 1200 IF READ_PERIOD("FIND", IC_CONTROL::ERA, YYYYPP$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 1%) = 0%

2005	!*******************************************************************
	! Scan for any open journals (if we are doing running balances)
	!*******************************************************************
	GOTO 3000 IF ONLY.HIST$ = "Y"

	!
	! Get list of journals (assuming they are all on this account)
	!
	IC_JOURNAL.DEV$ = ""
	CALL READ_DEVICE("IC_JOURNAL", IC_JOURNAL.DEV$, STAT%)
	CALL FIND_FILE(IC_JOURNAL.DEV$ + "IC_JOURNAL_*.JRL", ICFILE$(), &
		16%, "", "")
	ICFILE% = VAL(ICFILE$(0%))

	!
	! Process each file
	!
	FOR LOOP% = 1% TO ICFILE%

		BATCH_NO$ = RIGHT(ICFILE$(LOOP%), 12%)
		I% = INSTR(1%, BATCH_NO$, ".")
		BATCH_NO$ = LEFT(BATCH_NO$, I% - 1%)
		GOSUB 2100

	NEXT LOOP%

	!
	! Get list of journals (assuming they are all on this account)
	!
	FOR DIRECT% = 1% TO 9%
		IC_JOURNAL.DEV$ = ""
		CALL READ_DEVICE("IC_JOURNAL_" + NUM1$(DIRECT%), &
			IC_JOURNAL.DEV$, STAT%)
		GOTO 2090 IF IC_JOURNAL.DEV$ = ""

		CALL FIND_FILE(IC_JOURNAL.DEV$ + "IC_JOURNAL_*.JRL", &
			ICFILE$(), 16%, "", "")
		ICFILE% = VAL(ICFILE$(0%))

		!
		! Process each file
		!
		FOR LOOP% = 1% TO ICFILE%

			BATCH_NO$ = RIGHT(ICFILE$(LOOP%), 12%)
			I% = INSTR(1%, BATCH_NO$, ".")
			BATCH_NO$ = LEFT(BATCH_NO$, I% - 1%)
			GOSUB 2100

		NEXT LOOP%
2090	NEXT DIRECT%

	GOTO 3000

2100	!
	! Open up this journal
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "IC_JOURNAL_" + BATCH_NO$ + &
		".JRL", "", YYYYPP$, "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.OPN"
		RESET #IC_JOURNAL.CH%
	USE
		CONTINUE 2900
	END WHEN

2110	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_JOURNAL.CH%, REGARDLESS
	USE
		CONTINUE 2900 IF ERR = 11%
		FILENAME$ = "IC_JOURNAL_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	IF LOCATION$ <> ""
	THEN
		GOTO 2110 &
			IF COMP_STRING(EDIT$(IC_JOURNAL::LOCATION, -1%), &
			LOCATION$) = 0%
	END IF

2230	!
	! Update IC_35BALANCE for transaction A
	!
	GOTO 2240 IF IC_JOURNAL::TYPE_A = "" OR &
		IC_JOURNAL::QUANTITY_A = 0.0

	GOTO 2240 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_JOURNAL::TYPE_A, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_JOURNAL::PRODUCT + &
			IC_JOURNAL::LOCATION + IC_JOURNAL::TYPE_A
	USE
		CONTINUE JAddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::RBALANCE = IC_35BALANCE::RBALANCE + &
		IC_JOURNAL::QUANTITY_A

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 2240

 JAddA:
	IC_35BALANCE::PRODUCT	= IC_JOURNAL::PRODUCT
	IC_35BALANCE::LOCATION	= IC_JOURNAL::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_JOURNAL::TYPE_A
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= 0.0
	IC_35BALANCE::RBALANCE	= IC_JOURNAL::QUANTITY_A

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		CONTINUE JAddA IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN


2240	!
	! Update IC_35BALANCE for Transaction B
	!
	GOTO 2330 IF IC_JOURNAL::TYPE_B = "" OR &
		IC_JOURNAL::QUANTITY_B = 0.0

	GOTO 2330 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_JOURNAL::TYPE_B, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ IC_JOURNAL::PRODUCT + &
			IC_JOURNAL::LOCATION + IC_JOURNAL::TYPE_B
	USE
		CONTINUE JAddB IF ERR = 155%
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::RBALANCE = IC_35BALANCE::RBALANCE + &
		IC_JOURNAL::QUANTITY_B

	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	GOTO 2330

 JAddB:
	IC_35BALANCE::PRODUCT	= IC_JOURNAL::PRODUCT
	IC_35BALANCE::LOCATION	= IC_JOURNAL::LOCATION
	IC_35BALANCE::TRANSTYPE	= IC_JOURNAL::TYPE_B
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= 0.0
	IC_35BALANCE::RBALANCE	= IC_JOURNAL::QUANTITY_B

	WHEN ERROR IN
		PUT #IC_35BALANCE.CH%
	USE
		CONTINUE JAddB IF ERR = 155%
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

2330	!
	! Done with this record
	!
	GOTO 2110

2900	!
	! Done with this file
	!
	CLOSE #IC_JOURNAL.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "") &
		IF EXIT_STATUS = CMC$_NORMAL

	RETURN

3000	!*******************************************************************
	! Scan for any open journals (if we are doing running balances)
	!*******************************************************************
	GOTO 4000 IF ONLY.HIST$ = "Y"

	!
	! Get list of journals (assuming they are all on this account)
	!
	CALL READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)
	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*.JRL", OEFILE$(), &
		16%, "", "")
	OEFILE% = VAL(OEFILE$(0%))

	!
	! Process each file
	!
	FOR LOOP% = 1% TO OEFILE%

		BATCH_NO$ = RIGHT(OEFILE$(LOOP%), 14%)
		I% = INSTR(1%, BATCH_NO$, ".")
		IF I%
		THEN
			BATCH_NO$ = LEFT(BATCH_NO$, I% - 1%)
		END IF

		GOSUB 3100

	NEXT LOOP%

	FOR DIRECT% = 1% TO 9%
		!
		! Get list of journals (assuming they are all on this account)
		!
		OE_ORDERJOUR.DEV$ = ""
		CALL READ_DEVICE("OE_ORDERJOUR_" + NUM1$(DIRECT%), &
			OE_ORDERJOUR.DEV$, STAT%)
		GOTO 3090 IF OE_ORDERJOUR.DEV$ = ""
		CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*.JRL", &
			OEFILE$(), 16%, "", "")
		OEFILE% = VAL(OEFILE$(0%))

		OE_ORDERLINE.DEV$ = ""
		CALL READ_DEVICE("OE_ORDERLINE_" + NUM1$(DIRECT%), &
			OE_ORDERLINE.DEV$, STAT%)
		OE_ORDERLINE.DEV$ = OE_ORDERJOUR.DEV$ IF OE_ORDERLINE.DEV$ = ""

		!
		! Process each file
		!
		FOR LOOP% = 1% TO OEFILE%

			BATCH_NO$ = RIGHT(OEFILE$(LOOP%), 14%)
			I% = INSTR(1%, BATCH_NO$, ".")
			IF I%
			THEN
				BATCH_NO$ = LEFT(BATCH_NO$, I% - 1%)
			END IF

			GOSUB 3100

		NEXT LOOP%

3090	NEXT DIRECT%

	GOTO 4000

3100	!
	! Open up this journal
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "OE_ORDERJOUR_" + BATCH_NO$ + &
		".JRL", "", YYYYPP$, "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
		RESET #OE_ORDERJOUR.CH%
	USE
		CONTINUE 3900 IF ERR = 11% OR ERR = 5% OR ERR = 162%
		FILENAME$ = "OE_ORDERJOUR_" + YYYPP$
		CONTINUE HelpError
	END WHEN

3105	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
	USE
		CONTINUE 3900
	END WHEN

3110	!
	! Get next record
	!
	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE 3900 IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	!
	! Early exit when we can
	!
	IF LOCATION$ <> ""
	THEN
		GOTO 3110 &
			IF (COMP_STRING(EDIT$(OE_ORDERJOUR::LOCATION, -1%), &
			LOCATION$) = 0% AND &
			COMP_STRING(EDIT$(OE_ORDERJOUR::SHIPLIN, -1%), &
			LOCATION$) = 0%)
	END IF

3300	!
	! Do this record
	!
	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE 3110
	END WHEN

3310	!
	! Look through all lines on this order
	!
	WHILE (OE_ORDERLINE::ORDNUM = OE_ORDERJOUR::ORDNUM)

		!
		! Process this line
		!
		CALL SUBR_TRANTYPE( &
			OE_ORDERJOUR::CUSNUM, &
			OE_ORDERLINE::LIN, &
			OE_ORDERLINE::ORDQTY, &
			OE_ORDERLINE::SHPQTY, &
			OE_ORDERLINE::BCKQTY, &
			TRANTYPE$(), TRANQTY())

		FOR I% = 1% TO VAL%(TRANTYPE$(0%))

			!
			! From Location
			!
			IF LOCATION$ = "" OR &
				COMP_STRING(EDIT$(OE_ORDERJOUR::LOCATION, &
				-1%), LOCATION$)
			THEN
				V% = IC_WRIT_35BALANCE( &
					OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					LEFT(TRANTYPE$(I%), 2%), &
					TRANQTY(I%))
			END IF

			!
			! To Location (TR Only)
			!
			IF (LEFT(TRANTYPE$(I%), 2%) = "TR") AND &
				(LOCATION$ = "" OR &
				COMP_STRING(EDIT$(OE_ORDERJOUR::SHIPLIN, &
				-1%), LOCATION$))
			THEN
				V% = IC_WRIT_35BALANCE( &
					OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::SHIPLIN, &
					LEFT(TRANTYPE$(I%), 2%), &
					-TRANQTY(I%))
			END IF
		NEXT I%

		WHEN ERROR IN
			GET #OE_ORDERLINE.CH%, &
				REGARDLESS
		USE
			CONTINUE 3110
		END WHEN

	NEXT

	GOTO 3110

3900	!
	! Done with this file
	!
	CLOSE #OE_ORDERJOUR.CH%
	CLOSE #OE_ORDERLINE.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "") &
		IF EXIT_STATUS = CMC$_NORMAL

	RETURN

4000	!

9000	!
	! Finished
	!

 Completed:
	!
	! Update period file
	!
	GET #IC_CONTROL.CH%, RECORD 1%

	IC_CONTROL::CONTROLFLAG = "0"

	UPDATE #IC_CONTROL.CH%

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
		UTL_REPORTX, "", "", CUR.PERIOD$, PERIOD$)

	GOTO ExitProgram

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
