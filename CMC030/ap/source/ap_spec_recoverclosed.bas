1	%TITLE "Accounts Payable Vendor Maintenance"
	%SBTTL "AP_SPEC_RECOVERCLOSED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	!	.P
	!
	! Index:
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_RECOVERCLOSED/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_SPEC_RECOVERCLOSED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_RECOVERCLOSED.OBJ;*
	!
	! Author:
	!
	!	01/26/2000 - Kevin Handy
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

	!
	! Constants
	!
	DECLARE INTEGER CONSTANT MAXGL = 2000%

	!
	! MAP's
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP	(AP_CLOSE)	AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP
	DIM GL_YYYY_PP_CDD KEEP_GL_YYYY_PP(MAXGL)

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.HB"
	MAP	(AP_CONTROL_ACCOUNT)	AP_CONTROL_ACCOUNT_CDD	AP_CONTROL_ACCOUNT

	%PAGE

	CALL READ_INITIALIZE

	!
	! Open files
	!
300	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.CRE"

305	!======================================================================
	! AP_CLOSE file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CLOSE_OLD.CH%, STAT%)
	CALL READ_DEVICE('AP_CLOSE',AP_CLOSE.DEV$, STAT%)

	AP_CLOSE_OLD.NAME$ = AP_CLOSE.DEV$+"AP_CLOSE_NEWSTUFF.LED"

	OPEN AP_CLOSE_OLD.NAME$ FOR INPUT AS FILE AP_CLOSE_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CLOSE, &
		PRIMARY KEY &
		( &
			AP_CLOSE::VENNUM, &
			AP_CLOSE::TRANKEY &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AP_CLOSE::VENNUM, &
			AP_CLOSE::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_CLOSE::BATCH &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


310	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"

320	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"

330	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"

	GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	CLOSE AP_CONTROL.CH%

	IF AP_CONTROL::AP_ACCT = ""
	THEN
		PRINT "AP account not defined"
		GOTO ExitProgram
	END IF

	EXTRA_CASH$ = "12%.%%"

340	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL_ACCOUNT.OPN"

	!
	! Load up an account list
	!
	ACCOUNT_LIST$ = AP_CONTROL::AP_ACCT

	RESET #AP_CONTROL_ACCOUNT.CH%

345	WHEN ERROR IN
		GET #AP_CONTROL_ACCOUNT.CH%

	USE
		CONTINUE 390
	END WHEN

	ACCOUNT_LIST$ = ACCOUNT_LIST$ + "," + AP_CONTROL_ACCOUNT::ACCOUNT

	GOTO 345

390	!
	! Handle one GL period
	!
	PRINT "GL period to read (YYYY_PP)";
	LINPUT YYYY_PP$

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

1000	LASTBATCH$ = "      "

1100	!
	! SKip to the next batch
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, KEY #4% GT LASTBATCH$, REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

	LASTBATCH$ = GL_YYYY_PP::BTHNUM

	!
	! Only handle if this batch is a Purchase Journal
	!
	SELECT GL_YYYY_PP::SOURCE
	CASE "PJ  "
		GOTO 2000
	CASE "CD  "
		GOTO 2100
	END SELECT

	GOTO 1100

2000	!
	! Process one entire batch
	!
	GL_COUNT% = 0%

	!
	! Load up all the records for this batch
	!
	TRANKEY$ = "--**--**--"
	WHILE LASTBATCH$ = GL_YYYY_PP::BTHNUM

		IF GL_YYYY_PP::TRANKEY <> TRANKEY$
		THEN
			GOSUB ParseChunk
			TRANKEY$ = GL_YYYY_PP::TRANKEY
		END IF

		GL_COUNT% = GL_COUNT% + 1%
		KEEP_GL_YYYY_PP(GL_COUNT%) = GL_YYYY_PP

		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			CONTINUE 2099
		END WHEN
	NEXT

	GOSUB ParseChunk

2099	GOTO 1100

2100	!
	! Process one entire batch
	!
	GL_COUNT% = 0%

	!
	! Load up all the records for this batch
	!
	TRANKEY$ = "--**--**--"
	WHILE LASTBATCH$ = GL_YYYY_PP::BTHNUM

		IF GL_YYYY_PP::TRANKEY <> TRANKEY$
		THEN
			GOSUB ParseCrjChunk
			TRANKEY$ = GL_YYYY_PP::TRANKEY
		END IF

		!
		! The cash account doesn't hold useful information
		! for cash receipts. bummer
		!
		IF GL_YYYY_PP::ACCT <> AP_CONTROL::CASH_ACCT
		THEN
			GL_COUNT% = GL_COUNT% + 1%
			KEEP_GL_YYYY_PP(GL_COUNT%) = GL_YYYY_PP
		END IF

		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			CONTINUE 2199
		END WHEN
	NEXT

	GOSUB ParseCrjChunk

2199	GOTO 1100

 ParseChunk:
8000	!*******************************************************************
	! Look at all the records we have accumulated, and try to generate
	! whatever information we can about it
	!*******************************************************************

	RETURN IF GL_COUNT% = 0%

	HEADER% = 0%
	LINE% = 0%

	!
	! Zip out the header
	!
	AP_CLOSE::VENNUM	= ""
	AP_CLOSE::TRANKEY	= ""
	AP_CLOSE::TRANKEY_DATE	= ""
	AP_CLOSE::INVNUM	= ""
	AP_CLOSE::INVDAT	= ""
	AP_CLOSE::INVAMT	= 0.0
	AP_CLOSE::CODE_1099	= ""
	AP_CLOSE::AMT_1099	= 0.0
	AP_CLOSE::USE_JOB_NUM	= ""
	AP_CLOSE::USE_AMT	= 0.0
	AP_CLOSE::DISCDAT	= ""
	AP_CLOSE::DISAMT	= 0.0
	AP_CLOSE::DUEDAT	= ""
	AP_CLOSE::PONUM		= ""
	AP_CLOSE::AP_ACCT	= ""
	AP_CLOSE::CASH_ACCT	= ""
	AP_CLOSE::CKNUM		= ""
	AP_CLOSE::CKDAT		= ""
	AP_CLOSE::CKDESC	= ""
	AP_CLOSE::CKAMT		= 0.0
	AP_CLOSE::UPDATED	= LEFT(YYYY_PP$, 4%) + &
		MID(YYYY_PP$, 6%, 2%) + "00"
	AP_CLOSE::CLOSEDATE	= "********"
	AP_CLOSE::SELECTED	= ""
	AP_CLOSE::BATCH		= ""

	FOR LOOP% = 1% TO GL_COUNT%

		!
		! If it matches the cash account, then we have a
		! paymenmt here
		!
		IF KEEP_GL_YYYY_PP(LOOP%)::ACCT = AP_CONTROL::CASH_ACCT OR &
			LEFT(KEEP_GL_YYYY_PP(LOOP%)::ACCT, 2%) = "12"
		THEN
			AP_CLOSE::INVNUM	= KEEP_GL_YYYY_PP(LOOP%)::REFNO
			AP_CLOSE::INVDAT	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::CKDESC	= KEEP_GL_YYYY_PP(LOOP%)::DESCR
			AP_CLOSE::VENNUM	= KEEP_GL_YYYY_PP(LOOP%)::XREFNO
			AP_CLOSE::TRANKEY	= KEEP_GL_YYYY_PP(LOOP%)::TRANKEY
			AP_CLOSE::TRANKEY_DATE	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::CKNUM		= KEEP_GL_YYYY_PP(LOOP%)::CKNO
			AP_CLOSE::CKAMT		= -KEEP_GL_YYYY_PP(LOOP%)::AMOUNT
			AP_CLOSE::CKDAT		= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::INVAMT	= AP_CLOSE::INVAMT - &
				KEEP_GL_YYYY_PP(LOOP%)::AMOUNT
			AP_CLOSE::BATCH		= KEEP_GL_YYYY_PP(LOOP%)::BTHNUM
			AP_CLOSE::CASH_ACCT	= KEEP_GL_YYYY_PP(LOOP%)::ACCT
			AP_CLOSE::AP_ACCT = AP_CONTROL::AP_ACCT  &
				IF AP_CLOSE::AP_ACCT = ""
			AP_CLOSE::USE_JOB_NUM	= KEEP_GL_YYYY_PP(LOOP%)::SUBACC

			GOTO EndParseLoop
		END IF

		!
		! If it matches an ap trade account, then...
		!
		IF INSTR(1%, ACCOUNT_LIST$, KEEP_GL_YYYY_PP(LOOP%)::ACCT)
		THEN
			AP_CLOSE::INVNUM	= KEEP_GL_YYYY_PP(LOOP%)::REFNO
			AP_CLOSE::INVDAT	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::CKDESC	= KEEP_GL_YYYY_PP(LOOP%)::DESCR
			AP_CLOSE::VENNUM	= KEEP_GL_YYYY_PP(LOOP%)::XREFNO
			AP_CLOSE::TRANKEY	= KEEP_GL_YYYY_PP(LOOP%)::TRANKEY
			AP_CLOSE::TRANKEY_DATE	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::INVAMT	= AP_CLOSE::INVAMT - &
				KEEP_GL_YYYY_PP(LOOP%)::AMOUNT
			AP_CLOSE::BATCH		= KEEP_GL_YYYY_PP(LOOP%)::BTHNUM
			AP_CLOSE::CASH_ACCT	= AP_CONTROL::CASH_ACCT &
				IF AP_CLOSE::CASH_ACCT = ""
			AP_CLOSE::AP_ACCT	= KEEP_GL_YYYY_PP(LOOP%)::ACCT
			AP_CLOSE::USE_JOB_NUM	= KEEP_GL_YYYY_PP(LOOP%)::SUBACC
			GOTO EndParseLoop
		END IF

		!
		! Othwerwise, it must be detail information
		!

 EndParseLoop:
	NEXT LOOP%

	!
	! Try for 1099 information
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY#0% EQ AP_CLOSE::VENNUM, REGARDLESS
	USE
		CONTINUE 8070
	END WHEN

	IF AP_VENDOR::FLG1099 = "Y"
	THEN
		AP_CLOSE::CODE_1099	= "01"
		AP_CLOSE::AMT_1099	= AP_CLOSE::INVAMT
	END IF

8070	!
	! Don't duplicate
	!
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #0% EQ AP_CLOSE::VENNUM + AP_CLOSE::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 8080
	END WHEN

	PRINT "-";

	GOTO 8090

8080	!
	! Don't duplicate
	!
	WHEN ERROR IN
		FIND #AP_CLOSE_OLD.CH%, &
			KEY #0% EQ AP_CLOSE::VENNUM + AP_CLOSE::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 8085
	END WHEN

	PRINT "-";

	GOTO 8090

8085	PUT #AP_CLOSE.CH%

	PRINT "+";

8090	PRINT IF CCPOS(0%) >= 50%
	GL_COUNT% = 0%
	RETURN

 ParseCrjChunk:
8100	!*******************************************************************
	! Look at all the records we have accumulated, and try to generate
	! whatever information we can about it
	!*******************************************************************

	RETURN IF GL_COUNT% = 0%

	HEADER% = 0%
	LINE% = 0%

	!
	! Zip out the header
	!
	AP_CLOSE::VENNUM	= ""
	AP_CLOSE::TRANKEY	= ""
	AP_CLOSE::TRANKEY_DATE	= ""
	AP_CLOSE::INVNUM	= ""
	AP_CLOSE::INVDAT	= ""
	AP_CLOSE::INVAMT	= 0.0
	AP_CLOSE::CODE_1099	= ""
	AP_CLOSE::AMT_1099	= 0.0
	AP_CLOSE::USE_JOB_NUM	= ""
	AP_CLOSE::USE_AMT	= 0.0
	AP_CLOSE::DISCDAT	= ""
	AP_CLOSE::DISAMT	= 0.0
	AP_CLOSE::DUEDAT	= ""
	AP_CLOSE::PONUM		= ""
	AP_CLOSE::AP_ACCT	= ""
	AP_CLOSE::CASH_ACCT	= ""
	AP_CLOSE::CKNUM		= ""
	AP_CLOSE::CKDAT		= ""
	AP_CLOSE::CKDESC	= ""
	AP_CLOSE::CKAMT		= 0.0
	AP_CLOSE::UPDATED	= LEFT(YYYY_PP$, 4%) + &
		MID(YYYY_PP$, 6%, 2%) + "00"
	AP_CLOSE::CLOSEDATE	= "********"
	AP_CLOSE::SELECTED	= ""
	AP_CLOSE::BATCH		= ""

	FOR LOOP% = 1% TO GL_COUNT%

		!
		! If it matches the cash account, then we have a
		! paymenmt here
		!
		IF KEEP_GL_YYYY_PP(LOOP%)::ACCT = AP_CONTROL::CASH_ACCT OR &
			LEFT(KEEP_GL_YYYY_PP(LOOP%)::ACCT, 2%) = "12"
		THEN
			AP_CLOSE::INVNUM	= KEEP_GL_YYYY_PP(LOOP%)::REFNO
			AP_CLOSE::INVDAT	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::CKDESC	= KEEP_GL_YYYY_PP(LOOP%)::DESCR
			AP_CLOSE::VENNUM	= KEEP_GL_YYYY_PP(LOOP%)::XREFNO
			AP_CLOSE::CKNUM		= KEEP_GL_YYYY_PP(LOOP%)::CKNO
			AP_CLOSE::CKAMT		= -KEEP_GL_YYYY_PP(LOOP%)::AMOUNT
			AP_CLOSE::CKDAT		= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::INVAMT	= 0.0
			AP_CLOSE::BATCH		= KEEP_GL_YYYY_PP(LOOP%)::BTHNUM
			AP_CLOSE::CASH_ACCT	= KEEP_GL_YYYY_PP(LOOP%)::ACCT
			AP_CLOSE::AP_ACCT = AP_CONTROL::AP_ACCT  &
				IF AP_CLOSE::AP_ACCT = ""
			AP_CLOSE::USE_JOB_NUM	= KEEP_GL_YYYY_PP(LOOP%)::SUBACC

			GOTO EndCrjParseLoop
		END IF

		!
		! If it matches an ap trade account, then...
		!
		IF INSTR(1%, ACCOUNT_LIST$, KEEP_GL_YYYY_PP(LOOP%)::ACCT)
		THEN
			AP_CLOSE::INVNUM	= KEEP_GL_YYYY_PP(LOOP%)::REFNO
			AP_CLOSE::INVDAT	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::CKDESC	= KEEP_GL_YYYY_PP(LOOP%)::DESCR
			AP_CLOSE::VENNUM	= KEEP_GL_YYYY_PP(LOOP%)::XREFNO
			AP_CLOSE::CKNUM		= KEEP_GL_YYYY_PP(LOOP%)::CKNO
			AP_CLOSE::TRANKEY	= KEEP_GL_YYYY_PP(LOOP%)::TRANKEY
			AP_CLOSE::TRANKEY_DATE	= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::BATCH		= KEEP_GL_YYYY_PP(LOOP%)::BTHNUM
			AP_CLOSE::CASH_ACCT	= AP_CONTROL::CASH_ACCT &
				IF AP_CLOSE::CASH_ACCT = ""
			AP_CLOSE::AP_ACCT	= KEEP_GL_YYYY_PP(LOOP%)::ACCT

			AP_CLOSE::CKAMT		= KEEP_GL_YYYY_PP(LOOP%)::AMOUNT
			AP_CLOSE::CKDAT		= KEEP_GL_YYYY_PP(LOOP%)::TRANDAT
			AP_CLOSE::INVAMT	= 0.0
			AP_CLOSE::USE_JOB_NUM	= KEEP_GL_YYYY_PP(LOOP%)::SUBACC

			GOTO EndCrjParseLoop
		END IF

		!
		! Othwerwise, it must be detail information or
		! pull out of ap account
		!

 EndCrjParseLoop:
	NEXT LOOP%

	!
	! Try for 1099 information
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY#0% EQ AP_CLOSE::VENNUM, REGARDLESS
	USE
		CONTINUE 8170
	END WHEN

	IF AP_VENDOR::FLG1099 = "Y"
	THEN
		AP_CLOSE::CODE_1099	= "01"
		AP_CLOSE::AMT_1099	= AP_CLOSE::CKAMT
	END IF

8170	!
	! Don't duplicate
	!
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #0% EQ AP_CLOSE::VENNUM + AP_CLOSE::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 8180
	END WHEN

	PRINT "X";

	GOTO 8190

8180	!
	! Don't duplicate
	!
	WHEN ERROR IN
		FIND #AP_CLOSE_OLD.CH%, &
			KEY #0% EQ AP_CLOSE::VENNUM + AP_CLOSE::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 8185
	END WHEN

	PRINT "X";

	GOTO 8090

8185	PUT #AP_CLOSE.CH%

	PRINT "x";

8190	PRINT IF CCPOS(0%) >= 50%
	GL_COUNT% = 0%
	RETURN

	!*******************************************************************
	! Exit the program
	!*******************************************************************

 ExitProgram:

	PRINT "Done."

	GOTO 32767

32767	END
