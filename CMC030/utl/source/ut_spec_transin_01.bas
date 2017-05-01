1	%TITLE "Select and Transfer Files"
	%SBTTL "UT_SPEC_TRANSIN_01"
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
	!	This program is used to create information to be transfered
	!	from a main location to a satellite location.
	!	.b
	!	The information transfered includes:
	!	.b
	!	- New customers (start date).
	!	.br
	!	- New products (start date).
	!	.br
	!	- Price changes (price date).
	!	.br
	!	- A/R Open records (batch number).
	!	.br
	!	- Inventory Transactions (batch number).
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_TRANSIN_01/LINE
	!	$ LINK/EXE=UTL_EXE: UT_SPEC_TRANSIN_01, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_TRANSIN_01.OBJ;*
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
	!		Reformatted to 80 columns.
	!
	!	02/21/94 - Kevin Handy
	!		Added comments.
	!
	!	02/28/94 - Kevin Handy
	!		Modified to transfer entire AR_OPEN file after a
	!		close. Involved creating a file called "STATUS.STATUS"
	!		which keeps track of the last period transfered.
	!
	!	03/01/94 - Kevin Handy
	!		Fix bug caused by ASSG_POSTBATCH deciding to set
	!		unsolicited trapping in all programs it is called
	!		by, even if it wasn't there to start with.
	!
	!	03/09/94 - Kevin Handy
	!		Modifications to transfer IC_35BALANCE when
	!		inventory system is closed (or reset), copied
	!		from the code that handles AR_OPEN.
	!
	!	03/14/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/15/94 - Kevin Handy
	!		Added "SYS$LOGIN" in the copy commands for the
	!		remote system.
	!
	!	04/06/94 - Kevin Handy
	!		Modified to pass through AR_OPEN and IC_HISTORY
	!		by using batch number key in a find, instead
	!		of looping through the entire file.
	!		(IC_HISTORY was not transfering for some reason)
	!
	!	04/06/94 - Kevin Handy
	!		Modified to look into control file for period to
	!		use for IC_HISTORY file, instead of hard-coding
	!		199312. Also changed to read in more than one
	!		period.
	!
	!	04/06/94 - Kevin Handy
	!		Modified to do only today's and yesterday's data
	!		to reduce transfer time a little.
	!
	!	04/14/94 - Kevin Handy
	!		Added back one more day's worth of data.
	!
	!	08/10/94 - Kevin Handy
	!		Modified so that the Customer file will transfer
	!		anything with a date after yesterday.
	!
	!	09/28/94 - Kevin Handy
	!		Modified to use ASSG_MAKEBATCH instead of having
	!		the batch number calculation hard coded.
	!
	!	02/20/95 - Kevin Handy
	!		Modified to send all prices since jan-1995 for LL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/08/95 - Kevin Handy
	!		Trap "record exists" error at line 1040
	!		while addinging to customer file.
	!
	!	07/17/95 - Kevin Handy
	!		Reset price lookup to PRIOR$. Make PRIOR$ be two
	!		days back instead on only one. Maybe this will
	!		reduce the 100,000 block price files at LL.
	!
	!	08/01/95 - Kevin Handy
	!		Fixed comparison on prices from < to >.
	!
	!	08/17/95 - Kevin Handy
	!		Added ability to transfer PC_COST information.
	!
	!	08/17/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/17/95 - Kevin Handy
	!		Change to add FUTURE date stuff, so that prices
	!		entered six months ahead of time don't transfer daily
	!		until the effective date passes.
	!
	!	12/08/95 - Kevin Handy
	!		Fixed bug where it was having a lock problem on
	!		IC_CONTROL by doing a regardless on the get, and
	!		closing it when done. Also did a regardless on
	!		AR_CONTROL, and added ASSG_FREECHANNEL calls after
	!		closing.
	!
	!	12/15/95 - Kevin Handy
	!		Added secret log file, to keep a history of all
	!		attempts to download stuff.
	!
	!	01/03/96 - Kevin Handy
	!		Modified to use period defined in the status file
	!		if we are unable to open the IC_CONTROL file
	!		for some reason.
	!
	!	01/04/96 - Kevin Handy
	!		Zero out RECORDS% at the start of each transfer.
	!
	!	01/04/96 - Kevin Handy
	!		Temp change to force 01/01/96 prices down.
	!		Really need to set up a "Force single date" option.
	!
	!	01/18/96 - Kevin Handy
	!		Added force date option to report settings screen.
	!
	!	01/31/96 - Kevin Handy
	!		Change extension used on IC_35HISTORY from .LED
	!		to .HIS.
	!
	!	09/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/10/96 - Kevin Handy
	!		Initialize IC_TRANSACTION.DEV$ to "" after
	!		doing the first file.
	!
	!	10/11/96 - Kevin Handy
	!		Zero RECORDS% between different IC_TRANSACTION
	!		files.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	12/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)	PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)	PC_COST_CDD		PC_COST

	DECLARE LONG	EXIT_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL STRING FUNCTION ASSG_MAKEBATCH

	EXTERNAL LONG		READ_3BROADCAST
	EXTERNAL LONG		OUTP_XUNSOL

	DIM ARRAY_FILE$(100%)
	DIM ARRAY_NEW_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize the report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	!
	! Set user variables
	!

	FORCE_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))
	FORCE_DATE$ = "9999999999" IF FORCE_DATE$ <= "00000000"
	!++
	!
	! Abstract:FLD01
	!	^*(01) Force Date\*
	!	.b
	!	.lm +5
	!	Specifies a date for which prices and costs are to be
	!	forced down. Ignored if blank.
	!	.lm -5
	!
	! Index:
	!
	!--

 !	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	!
	! Set up titles for report
	!
	TITLE(1%) = "DAILY CREATE DOWNLOAD PROTOCOL"
	TITLE(2%) = "Utility System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!
	! Open up secret log file to keep track of what files have been
	! transfered to the main system, and on what date.
	!
	CALL ASSG_CHANNEL(LOGFILE.CH%, EXIT_STATUS%)

	OPEN "FILESENT.TXT" AS FILE LOGFILE.CH%, &
		ACCESS APPEND

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
		"Transfer Started"

	!
	! Open up batch control file and check if interrupted
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSIN", "", "", "")

	%PAGE

300	!*******************************************************************
	! Get IC control information
	!*******************************************************************

	IC_CONTROL_FLAG% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		IC_CONTROL_FLAG% = 0%

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #IC_CONTROL.CH%
		CALL ASSG_FREECHANNEL(IC_CONTROL.CH%)
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE 310
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

310	!******************************************************************
	! Assign a batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSIN", "TRANSFER", &
		"", "") <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	TODAY$ = DATE_TODAY
	PRIOR$ = DATE_INVDCODE(DATE_DAYCODE(TODAY$) - 2%)
	FUTURE$ = DATE_INVDCODE(DATE_DAYCODE(TODAY$) + 1%)
	ZEROTIME$ = "0000"

	PRIOR_NUMBER$ = ASSG_MAKEBATCH(PRIOR$, ZEROTIME$)

390	!*******************************************************************
	! Read from file defining which periods were the last ones closed
	!
	!	1 - AR_OPEN
	!	2 - IC_35BALANCE
	!	3 -
	!	4 -
	!	5 -
	!	6 -
	!	7 -
	!	8 -
	!	9 -
	!
	!*******************************************************************

	STATUS$(I%) = "" FOR I% = 1% TO 9%
	CALL ASSG_CHANNEL(STATUS.CH%, STATUS%)
	WHEN ERROR IN
		OPEN "STATUS.STATUS" FOR INPUT AS FILE STATUS.CH%
		LINPUT #STATUS.CH%, STATUS$(I%) FOR I% = 1% TO 9%
		CLOSE #STATUS.CH%
	USE
		CONTINUE 400
	END WHEN

	IF IC_CONTROL_FLAG% = -1%
	THEN
		CUR.PERIOD$, YYYYPP$ = STATUS$(2%)

		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " IC_CONTROL "; &
			"-- File is locked, using period "; CUR.PERIOD$
	END IF

400	!

1000	!*******************************************************************
	! Process customer file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	AR_35CUSTOM.CH_READ% = AR_35CUSTOM.CH%
	AR_35CUSTOM.CH% = 0%
	RECORDS% = 0%

1010	AR_35CUSTOM.DEV$ = "CMC_OUTBOX:"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.CRE"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "AR_35CUSTOM.MAS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1020	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH_READ%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

1030	WHEN ERROR IN
		GET #AR_35CUSTOM.CH_READ%, REGARDLESS
	USE
		CONTINUE 1050 IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO 1040 IF AR_35CUSTOM::BDATE = FORCE_DATE$
	GOTO 1030 IF AR_35CUSTOM::BDATE < PRIOR$
	GOTO 1030 IF AR_35CUSTOM::BDATE > FUTURE$

1040	WHEN ERROR IN
		PUT #AR_35CUSTOM.CH%
	USE
		CONTINUE 1030 IF ERR = 134%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1030

1050	CLOSE AR_35CUSTOM.CH%
	CLOSE AR_35CUSTOM.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " AR_CUSTOMER "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

1100	RECORDS% = 0%

	!*******************************************************************
	! Process inventory product file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	PD_PRODUCT.CH_READ% = PD_PRODUCT.CH%
	PD_PRODUCT.CH% = 0%
	RECORDS% = 0%

1110	PD_PRODUCT.DEV$ = "CMC_OUTBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PD_PRODUCT.MAS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1120	WHEN ERROR IN
		RESET #PD_PRODUCT.CH_READ%
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

1130	WHEN ERROR IN
		GET #PD_PRODUCT.CH_READ%, REGARDLESS
	USE
		CONTINUE 1150 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO 1140 IF PD_PRODUCT::BDATE = FORCE_DATE$
	GOTO 1130 IF PD_PRODUCT::BDATE <> TODAY$

1140	WHEN ERROR IN
		PUT #PD_PRODUCT.CH%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1130

1150	CLOSE PD_PRODUCT.CH%
	CLOSE PD_PRODUCT.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " PD_PRODUCT "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

1200	RECORDS% = 0%

	!*******************************************************************
	! Open price file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.OPN"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	PC_PRICE.CH_READ% = PC_PRICE.CH%
	PC_PRICE.CH% = 0%
	RECORDS% = 0%

1210	PC_PRICE.DEV$ = "CMC_OUTBOX:"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PC_PRICE.MAS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1220	WHEN ERROR IN
		RESET #PC_PRICE.CH_READ%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

1230	WHEN ERROR IN
		GET #PC_PRICE.CH_READ%, REGARDLESS
	USE
		CONTINUE 1250 IF ERR = 11%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO 1240 IF PC_PRICE::XDATE = FORCE_DATE$
	GOTO 1230 IF PC_PRICE::XDATE < PRIOR$
	GOTO 1230 IF PC_PRICE::XDATE > FUTURE$

1240	WHEN ERROR IN
		PUT #PC_PRICE.CH%
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1230

1250	CLOSE PC_PRICE.CH%
	CLOSE PC_PRICE.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " PC_PRICE "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

1300	!*******************************************************************
	! Process A/R Open register
	!*******************************************************************

	RECORDS% = 0%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AR_CONTROL.CH%
		CALL ASSG_FREECHANNEL(AR_CONTROL.CH%)
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	ARSTATUS$ = AR_CONTROL::YEAR + FORMAT$(AR_CONTROL::LASTPERCLOSE, "<0>#")

1305	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	AR_OPEN.CH_READ% = AR_OPEN.CH%
	AR_OPEN.CH% = 0%

	!
	! Look in the control file to see if we transfer the entire AR_OPEN
	! file, or just the current transactions
	!
	! We do this after opening the AR_OPEN file so that we have it's
	! device name.
	!
	IF ARSTATUS$ <> STATUS$(1%)
	THEN
		!
		! Copy the control file as well as the entire open file
		!
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

		STATUS% = LIB$SPAWN("BACKUP " + &
			AR_CONTROL.DEV$ + &
			"AR_CONTROL.CTR CMC_OUTBOX:*.CTR_FULL/IGNORE=INTERLOCK")

		STATUS% = LIB$SPAWN("BACKUP " + &
			AR_OPEN.DEV$ + &
			"AR_OPEN.LED CMC_OUTBOX:*.LED_FULL/IGNORE=INTERLOCK")

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		!
		! Append commands to special file to make it transfer
		! the ledger over to the proper place on the remote
		! system.
		!
		OPEN "CMC_OUTBOX:SPECIAL.COM" AS FILE STATUS.CH%, &
			ACCESS APPEND

		PRINT #STATUS.CH%, "$ COPY CMC_INBOX:AR_CONTROL.CTR_FULL " + &
			"SYS$LOGIN:*.CTR"
		PRINT #STATUS.CH%, "$ DELETE CMC_INBOX:AR_CONTROL.CTR_FULL;*"
		PRINT #STATUS.CH%, "$ PURGE AR_CONTROL.*"
		PRINT #STATUS.CH%, "$ COPY CMC_INBOX:AR_OPEN.LED_FULL " + &
			"SYS$LOGIN:*.LED"
		PRINT #STATUS.CH%, "$ DELETE CMC_INBOX:AR_OPEN.LED_FULL;*"
		PRINT #STATUS.CH%, "$ PURGE AR_OPEN.*"

		CLOSE STATUS.CH%

		STATUS$(1%) = ARSTATUS$

		TEXT$ = "AR_OPEN"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + "Downloaded Entire File"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " AR_OPEN "; &
			"Entire File"
		GOTO 1400
	END IF

1310	AR_OPEN.DEV$ = "CMC_OUTBOX:"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "AR_OPEN.HIS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1320	WHEN ERROR IN
		FIND #AR_OPEN.CH_READ%, KEY #1% GE PRIOR_NUMBER$
	USE
		CONTINUE 1350 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

1330	WHEN ERROR IN
		GET #AR_OPEN.CH_READ%, REGARDLESS
	USE
		CONTINUE 1350 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 !	GOTO 1330 IF AR_OPEN::BATCH < PRIOR_NUMBER$

1340	WHEN ERROR IN
		PUT #AR_OPEN.CH%
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1330

1350	CLOSE AR_OPEN.CH%
	CLOSE AR_OPEN.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " AR_OPEN "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

1400	!*******************************************************************
	! Process inventory register
	!*******************************************************************

	YYYYPP$ = CUR.PERIOD$
	TRANFLAG% = 0%

1405	IC_TRANSACTION.DEV$ = ""
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		IF ERR = 5%
		THEN
			TRANFLAG% = -1%
			CONTINUE 1450
		END IF
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	IC_TRANSACTION.CH_READ% = IC_TRANSACTION.CH%
	IC_TRANSACTION.CH% = 0%
	RECORDS% = 0%

1410	IC_TRANSACTION.DEV$ = "CMC_OUTBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, &
		"IC_TRANSACTION_" + YYYYPP$ + ".HIS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1420	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH_READ%, KEY #1% GE PRIOR_NUMBER$
	USE
		CONTINUE 1450 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

1430	WHEN ERROR IN
		GET #IC_TRANSACTION.CH_READ%, REGARDLESS
	USE
		CONTINUE 1450 IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

 !	GOTO 1430 IF IC_TRANSACTION::BATCH < PRIOR_NUMBER$

1440	WHEN ERROR IN
		PUT #IC_TRANSACTION.CH%
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	RECORDS% = RECORDS% + 1%
	GOTO 1430

1450	CLOSE IC_TRANSACTION.CH%
	CLOSE IC_TRANSACTION.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); &
		" IC_TRANSACTION_"; YYYYPP$; " "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

	RECORDS% = 0%
	YYYY% = VAL%(LEFT(YYYYPP$, 4%))
	PP% = VAL%(RIGHT(YYYYPP$, 5%)) + 1%
	IF (PP% = 12%)
	THEN
		PP% = 1%
		YYYY% = YYYY% + 1%
	END IF
	YYYYPP$ = FORMAT$(YYYY%, "<0>###") + FORMAT$(PP%, "<0>#")
	GOTO 1405 IF TRANFLAG% = 0%

1500	!*******************************************************************
	! Download IC_35BALANCE if it has been closed
	!*******************************************************************

	!
	! Look in the control file to see if we transfer the entire AR_OPEN
	! file, or just the current transactions
	!
	! We do this after opening the AR_OPEN file so that we have it's
	! device name.
	!
	IF CUR.PERIOD$ <> STATUS$(2%)
	THEN
		!
		! Just getting device file name making sure everything
		! is OK.
		!
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"
		CLOSE #IC_35BALANCE.CH%

		!
		! Copy the control file as well as the entire open file
		!
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

		STATUS% = LIB$SPAWN("BACKUP " + &
			IC_CONTROL.DEV$ + &
			"IC_CONTROL.CTR CMC_OUTBOX:*.CTR_FULL/IGNORE=INTERLOCK")

		STATUS% = LIB$SPAWN("BACKUP " + &
			IC_35BALANCE.DEV$ + &
			"IC_35BALANCE.HIS CMC_OUTBOX:*.HIS_FULL/IGNORE=INTERLOCK")

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		!
		! Append commands to special file to make it transfer
		! the ledger over to the proper place on the remote
		! system.
		!
		OPEN "CMC_OUTBOX:SPECIAL.COM" AS FILE STATUS.CH%, &
			ACCESS APPEND

		PRINT #STATUS.CH%, "$ COPY CMC_INBOX:IC_CONTROL.CTR_FULL SYS$LOGIN:*.CTR"
		PRINT #STATUS.CH%, "$ DELETE CMC_INBOX:IC_CONTROL.CTR_FULL;*"
		PRINT #STATUS.CH%, "$ PURGE IC_CONTROL.*"
		PRINT #STATUS.CH%, "$ COPY CMC_INBOX:IC_35BALANCE.HIS_FULL SYS$LOGIN:*.HIS"
		PRINT #STATUS.CH%, "$ DELETE CMC_INBOX:IC_35BALANCE.HIS_FULL;*"
		PRINT #STATUS.CH%, "$ PURGE IC_35BALANCE.*"

		CLOSE STATUS.CH%

		STATUS$(2%) = CUR.PERIOD$

		TEXT$ = "IC_35BALANCE"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = SPACE$(9%) + "Downloaded Entire File"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " IC_BALANCE "; &
			"Entire File"

		GOTO 1600
	END IF

1600	RECORDS% = 0%

	!*******************************************************************
	! Open COST file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.OPN"
	USE
		CONTINUE 1700
	END WHEN

	PC_COST.CH_READ% = PC_COST.CH%
	PC_COST.CH% = 0%
	RECORDS% = 0%

1610	PC_COST.DEV$ = "CMC_OUTBOX:"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
	USE
		CONTINUE 1650
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PC_COST.MAS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

1620	WHEN ERROR IN
		RESET #PC_COST.CH_READ%
	USE
		CONTINUE 1650
	END WHEN

1630	WHEN ERROR IN
		GET #PC_COST.CH_READ%, REGARDLESS
	USE
		CONTINUE 1650
	END WHEN

	GOTO 1640 IF PC_COST::EFFDATE = FORCE_DATE$
	GOTO 1630 IF PC_COST::EFFDATE < PRIOR$
	GOTO 1630 IF PC_COST::EFFDATE > FUTURE$

1640	PUT #PC_COST.CH%

	RECORDS% = RECORDS% + 1%
	GOTO 1630

1650	CLOSE PC_COST.CH%
	CLOSE PC_COST.CH_READ%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
		"New records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " PC_COST "; &
		FORMAT$(RECORDS%, "######## "); &
		"New records"

1700	!*******************************************************************
	! Add new stuff here
	!*******************************************************************

 Completed:
	!*******************************************************************
	! Complete closing process and remove batch control
	!*******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

	!
	! Create new period file
	!
	OPEN "STATUS.STATUS" FOR OUTPUT AS FILE STATUS.CH%
	PRINT #STATUS.CH%, STATUS$(I%) FOR I% = 1% TO 9%
	CLOSE #STATUS.CH%

 ExitProgram:
	CLOSE LOGFILE.CH%
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

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); &
		"Error " + NUM1$(ERR) + " at " + NUM1$(ERL) + ": " + &
		FILENAME$

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
