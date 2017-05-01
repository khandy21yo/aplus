1	%TITLE "Select and Transfer Files"
	%SBTTL "UT_SPEC_TRANSOUT"
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
	!	This program is used to move data received by a
	!	satellite location during a nightly
	!	transfer (main -> satellite ) into the proper places.
	!	.b
	!	The information transfered includes:
	!	.b
	!	- New customers.
	!	.br
	!	- New products.
	!	.br
	!	- Price changes.
	!	.br
	!	- A/R Open records.
	!	.br
	!	- Inventory Transactions.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_TRANSOUT/LINE
	!	$ LINK/EXE=UTL_EXE: UT_SPEC_TRANSOUT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_TRANSOUT.OBJ;*
	!
	! Author:
	!
	!	11/27/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/25/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/21/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	02/23/94 - Kevin Handy
	!		Modify handling AR_OPEN files so that it looks
	!		at more than the first record in the existing
	!		register to decide if it already exists.
	!
	!	02/23/94 - Kevin Handy
	!		Add comments to break program into sections.
	!
	!	02/23/94 - Kevin Handy
	!		Changed a PUT on the PC_PRICE file after a record
	!		was found to an update. (So there aren't duplicates)
	!
	!	03/14/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/06/94 - Kevin Handy
	!		Modified to transfer in all IC_TRANSACTION files,
	!		and not just a hard coded "199312".
	!
	!	09/28/94 - Kevin Handy
	!		Made slight improvements in deciding if a record
	!		already exists in the final file for AR_OPEN and
	!		IC_TRANSACTION.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/17/95 - Kevin Handy
	!		Adjust source closer to 80 columns.
	!
	!	07/17/95 - Kevin Handy
	!		Modifications to handle files that come over
	!		trashed.
	!		Will also try again after a KILL in case
	!		there happen to be more than one version of a
	!		file there.
	!		maybe this will fix some of LL's problems.
	!
	!	08/17/95 - Kevin Handy
	!		Clean out lots of commented out code.
	!
	!	08/17/95 - Kevin Handy
	!		Added ability to transfer COST files.
	!
	!	01/08/96 - Kevin Handy
	!		Try to trap an end-of-file-on-device error that
	!		pasco is getting.
	!
	!	01/10/96 - Kevin Handy
	!		Fix error trap for line 1441, to also trap an
	!		eof <11> error. (LL)
	!
	!	09/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/09/2001 - Kevin Handy
	!		Increase number of files allowed from 100 to 300.
	!		Lose unused ARRAY_FILE and ARRAY_FILE_NEW arrays.
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM
	DECLARE		AR_35CUSTOM_CDD	AR_35CUSTOM_TEMP

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)	AR_OPEN_CDD	AR_OPEN
	DECLARE		AR_OPEN_CDD	AR_OPEN_TEMP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT
	DECLARE PD_PRODUCT_CDD	PD_PRODUCT_TEMP

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION
	DECLARE		IC_TRANSACTION_CDD	IC_TRANSACTION_TEMP

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)	PC_PRICE_CDD		PC_PRICE
	DECLARE		PC_PRICE_CDD		PC_PRICE_TEMP

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)	PC_COST_CDD		PC_COST
	DECLARE		PC_COST_CDD		PC_COST_TEMP

	DECLARE LONG	EXIT_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER
	DIM FILE_LIST$(300%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	TITLE(1%) = "DAILY UPDATE FILE PROTOCOL"
	TITLE(2%) = "Utility System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!******************************************************************
	! Open up batch control file and check if interrupted
	!******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSOUT", "", "", "")

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE 400
	END WHEN

	CUR.PERIOD$, YYYYPP$ = IC_CONTROL::PERIOD

	EXIT_STATUS = CMC$_NORMAL

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0"
		!
		! the control flag seems to be O.K.
		!

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_WARNING

	END SELECT

400	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSOUT", "TRANSFER", &
		"", "") <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	!
	! Set close flag in control file
	!

	TODAY$ = DATE_TODAY

1000	!*******************************************************************
	! Open customer file
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "AR_35CUSTOM.MAS", "", "", "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.CRE"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	AR_35CUSTOM.CH_WRITE% = AR_35CUSTOM.CH%
	AR_35CUSTOM.CH% = 0%

1010	AR_35CUSTOM.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		CONTINUE 1100 IF ERR = 5%
		CONTINUE 1050
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1020	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

1030	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%
	USE
		CONTINUE 1050
	END WHEN

	AR_35CUSTOM_TEMP = AR_35CUSTOM
	RECORDS% = RECORDS% + 1%

1040	WHEN ERROR IN
		GET #AR_35CUSTOM.CH_WRITE%, KEY #0% EQ AR_35CUSTOM::CUSNUM
		AR_35CUSTOM = AR_35CUSTOM_TEMP
		UPDATE #AR_35CUSTOM.CH_WRITE%
	USE
		CONTINUE 1042 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO 1030

1042	WHEN ERROR IN
		PUT #AR_35CUSTOM.CH_WRITE%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO 1030

1050	CLOSE AR_35CUSTOM.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Records updated"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:AR_35CUSTOM.MAS"
	USE
		CONTINUE 1100 IF ERR = 5%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO 1010

1100	RECORDS% = 0%
	CLOSE AR_35CUSTOM.CH_WRITE%

	!*******************************************************************
	! Open product file
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PD_PRODUCT.MAS", "", "", "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	PD_PRODUCT.CH_WRITE% = PD_PRODUCT.CH%
	PD_PRODUCT.CH% = 0%

1110	PD_PRODUCT.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 1200 IF ERR = 5%
		CONTINUE 1150
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1120	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

1130	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		CONTINUE 1150
	END WHEN

	PD_PRODUCT_TEMP = PD_PRODUCT
	RECORDS% = RECORDS% + 1%

1140	WHEN ERROR IN
		GET #PD_PRODUCT.CH_WRITE%, KEY #0% EQ PD_PRODUCT::PRODUCT_NUM
		PD_PRODUCT = PD_PRODUCT_TEMP
		UPDATE #PD_PRODUCT.CH_WRITE%
	USE
		CONTINUE 1142 IF ERR = 155%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1130

1142	WHEN ERROR IN
		PUT #PD_PRODUCT.CH_WRITE%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1130

1150	CLOSE PD_PRODUCT.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Updated records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:PD_PRODUCT.MAS"
	USE
		CONTINUE 1200 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO 1110

1200	RECORDS% = 0%
	CLOSE PD_PRODUCT.CH_WRITE%

	!*******************************************************************
	! Open price file
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PC_PRICE.MAS", "", "", "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	PC_PRICE.CH_WRITE% = PC_PRICE.CH%
	PC_PRICE.CH% = 0%

1210	PC_PRICE.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.OPN"
	USE
		CONTINUE 1300 IF ERR = 5%
		CONTINUE 1250
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1220	WHEN ERROR IN
		RESET #PC_PRICE.CH%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

1230	WHEN ERROR IN
		GET #PC_PRICE.CH%
	USE
		CONTINUE 1250
	END WHEN

	PC_PRICE_TEMP = PC_PRICE
	RECORDS% = RECORDS% + 1%

1240	WHEN ERROR IN
		GET #PC_PRICE.CH_WRITE%, &
			KEY #1% EQ PC_PRICE::PCTYPE + &
			PC_PRICE::PRODUCT_NUM + &
			PC_PRICE::LOCATION + &
			PC_PRICE::XDATE

		PC_PRICE = PC_PRICE_TEMP
		UPDATE #PC_PRICE.CH_WRITE%
	USE
		CONTINUE 1242 IF ERR = 155%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1230

1242	WHEN ERROR IN
		PUT #PC_PRICE.CH_WRITE%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1230

1250	CLOSE PC_PRICE.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Updated records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:PC_PRICE.MAS"
	USE
		CONTINUE 1300 IF ERR = 5%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1210

1300	!*******************************************************************
	! Open register file
	!*******************************************************************

	CLOSE PC_PRICE.CH_WRITE%

1305	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	AR_OPEN.CH_WRITE% = AR_OPEN.CH%
	AR_OPEN.CH% = 0%

1310	AR_OPEN.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 1400
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "AR_OPEN.LED", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1320	WHEN ERROR IN
		RESET #AR_OPEN.CH%
	USE
		CONTINUE 1350
	END WHEN

1330	WHEN ERROR IN
		GET #AR_OPEN.CH%
	USE
		CONTINUE 1350
	END WHEN

	AR_OPEN_TEMP = AR_OPEN

1340	WHEN ERROR IN
		GET #AR_OPEN.CH_WRITE%, &
			KEY #0% EQ AR_OPEN::CUSNUM + AR_OPEN::INVNUM + AR_OPEN::TRATYP
	USE
		CONTINUE 1342 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

1341	IF AR_OPEN::CUSNUM = AR_OPEN_TEMP::CUSNUM AND &
		AR_OPEN::INVNUM = AR_OPEN_TEMP::INVNUM AND &
		AR_OPEN::TRATYP = AR_OPEN_TEMP::TRATYP
	THEN
		GOTO 1330 IF AR_OPEN::BATCH = AR_OPEN_TEMP::BATCH AND &
			AR_OPEN::TRADAT = AR_OPEN_TEMP::TRADAT AND &
			AR_OPEN::SALAMT = AR_OPEN_TEMP::SALAMT

		WHEN ERROR IN
			GET #AR_OPEN.CH_WRITE%
		USE
			AR_OPEN = AR_OPEN_TEMP
			CONTINUE 1342 IF ERR = 11%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		GOTO 1341
	END IF

	AR_OPEN = AR_OPEN_TEMP

1342	WHEN ERROR IN
		PUT #AR_OPEN.CH_WRITE%
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1330

1350	CLOSE AR_OPEN.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Records updated"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:AR_OPEN.LED"
	USE
		CONTINUE 1400 IF ERR = 5%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 1310

1400	!*******************************************************************
	! Open ledger file
	!*******************************************************************

	CLOSE AR_OPEN.CH_WRITE%

	!
	! Get years for files
	!
	CALL FIND_FILE("CMC_INBOX:IC_TRANSACTION_*.LED", &
		FILE_LIST$(), &
		16%, &
		"IC_TRANSACTION_", &
		".LED")

	WHEN ERROR IN
		FILE_COUNT% = VAL%(FILE_LIST$(0%))
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

1405	GOTO 1500 IF FILE_COUNT% = 0%

	YYYYPP$ = FILE_LIST$(FILE_COUNT%)
	FILE_COUNT% = FILE_COUNT% - 1%

	RECORDS% = 0%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, &
		"IC_TRANSACTION_" + YYYYPP$ + ".LED", "", "", "")

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
	IC_TRANSACTION.CH_WRITE% = IC_TRANSACTION.CH%
	IC_TRANSACTION.CH% = 0%

1410	IC_TRANSACTION.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		CONTINUE 1450
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1420	WHEN ERROR IN
		RESET #IC_TRANSACTION.CH%
	USE
		CONTINUE 1450
	END WHEN

1430	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%
	USE
		CONTINUE 1450
	END WHEN

	IC_TRANSACTION_TEMP = IC_TRANSACTION

1440	WHEN ERROR IN
		GET #IC_TRANSACTION.CH_WRITE%, &
			KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + &
			IC_TRANSACTION::TRANS_DATE
	USE
		CONTINUE 1442 IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

1441	IF IC_TRANSACTION::PRODUCT = IC_TRANSACTION_TEMP::PRODUCT AND &
		IC_TRANSACTION::LOCATION = IC_TRANSACTION_TEMP::LOCATION AND &
		IC_TRANSACTION::TRANS_DATE = IC_TRANSACTION_TEMP::TRANS_DATE
	THEN
		GOTO 1430 &
			IF IC_TRANSACTION::CROSS_REF = &
				IC_TRANSACTION_TEMP::CROSS_REF AND &
			IC_TRANSACTION::SUBACCOUNT = &
				IC_TRANSACTION_TEMP::SUBACCOUNT AND &
			IC_TRANSACTION::QUANTITY_A = &
				IC_TRANSACTION_TEMP::QUANTITY_A

		WHEN ERROR IN
			GET #IC_TRANSACTION.CH_WRITE%
		USE
			CONTINUE 1442 IF ERR = 155% OR ERR=11%
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN

		GOTO 1441
	END IF

1442	IC_TRANSACTION = IC_TRANSACTION_TEMP

	WHEN ERROR IN
		PUT #IC_TRANSACTION.CH_WRITE%
		RECORDS% = RECORDS% + 1%
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	GOTO 1430

1450	CLOSE IC_TRANSACTION.CH%
	CLOSE IC_TRANSACTION.CH_WRITE%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Records updated"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:IC_TRANSACTION_" + YYYYPP$ + ".LED"
	USE
		CONTINUE Completed IF ERR = 5%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	GOTO 1405

1500	RECORDS% = 0%

	!*******************************************************************
	! Open COST file
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PC_COST.MAS", "", "", "")

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	PC_COST.CH_WRITE% = PC_COST.CH%
	PC_COST.CH% = 0%

1510	PC_COST.DEV$ = "CMC_INBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.OPN"
	USE
		CONTINUE 1600 IF ERR = 5%
		CONTINUE 1550
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1520	WHEN ERROR IN
		RESET #PC_COST.CH%
	USE
		FILENAME$ = "PC_COST"
		CONTINUE 1550
	END WHEN

1530	WHEN ERROR IN
		GET #PC_COST.CH%
	USE
		CONTINUE 1550
	END WHEN

	PC_COST_TEMP = PC_COST
	RECORDS% = RECORDS% + 1%

1540	WHEN ERROR IN
		GET #PC_COST.CH_WRITE%, &
			KEY #0% EQ PC_COST::PRODUCT + &
			PC_COST::LOCATION + &
			PC_COST::EFFDATE

		PC_COST = PC_COST_TEMP
		UPDATE #PC_COST.CH_WRITE%
	USE
		CONTINUE 1542 IF ERR = 155%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO 1530

1542	WHEN ERROR IN
		PUT #PC_COST.CH_WRITE%
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO 1530

1550	CLOSE PC_COST.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"Updated records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WHEN ERROR IN
		KILL "CMC_INBOX:PC_COST.MAS"
	USE
		CONTINUE 1600 IF ERR = 5%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO 1510

1600	!

 Completed:
	!*******************************************************************
	! Complete closing process and remove batch control
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

	!*******************************************************************
	! Exit
	!*******************************************************************

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, &
			"RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", CUR.PERIOD$, PERIOD$)

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

	RESUME HelpError

32767	END
