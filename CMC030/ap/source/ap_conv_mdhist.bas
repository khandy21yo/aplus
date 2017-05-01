1	%TITLE "Convert AP OPEN from MicroData"
	%SBTTL "AP_CONV_MDHIST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	.p
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CONV_MDHIST/LINE
	!	$ LINK/EXEC:AP_EXE AP_CONV_MDHIST,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CONV_MDHIST.OBJ;*
	!
	! Author:
	!
	!	07/05/91 - Kevin Handy
	!
	! Modification history:
	!
	!	08/26/91 - Kevin Handy
	!		Modified tolose two digits of account number,
	!		and to add -STORE to end of account.
	!
	!	08/28/91 - Kevin Handy
	!		Modified to make sure we get the invoice number.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose fnform_acct$ (Dead Code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

 !	def fnform_acct$(a$) = left(a$, 2%) + "-" + mid(a$,3%, 3%) + "-" + &
 !		mid(trm$(a$), 6%, 2%) + "-" + accnoc$

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE
	MAP (AP_CLOSE_BLANK)	AP_CLOSE_CDD	AP_CLOSE_BLANK

	EXTERNAL LONG    FUNCTION DATE_DAYCODE
	EXTERNAL STRING  FUNCTION DATE_INVDCODE

	START_DATE% = DATE_DAYCODE("19671231")

	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$
	accnoc$ = mid(storen0$, 2%, 2%)

200	! RESUME LINE

 !	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.CRE"
	!======================================================================
	! AP_CLOSE file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CLOSE.CH%, STAT%)
	CALL READ_DEVICE("AP_CLOSE",AP_CLOSE.DEV$, STAT%)
	CALL READ_PROTECTION("AP_CLOSE",AP_CLOSE.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AP_CLOSE.PRO$, STAT%)

	AP_CLOSE.NAME$ = AP_CLOSE.DEV$+"AP_CLOSE.LED"

	OPEN AP_CLOSE.NAME$ FOR OUTPUT AS FILE AP_CLOSE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CLOSE, &
		BUFFER 32%, &
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
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN APSYS_ASC.DEV$ + "APHIST.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

	AP_CLOSE_BLANK::VENNUM		= ""
	AP_CLOSE_BLANK::TRANKEY		= ""
	AP_CLOSE_BLANK::TRANKEY_DATE	= ""
	AP_CLOSE_BLANK::INVNUM		= ""
	AP_CLOSE_BLANK::INVDAT		= ""
	AP_CLOSE_BLANK::CODE_1099	= ""
	AP_CLOSE_BLANK::AMT_1099		= 0.0
	AP_CLOSE_BLANK::USE_JOB_NUM	= ""
	AP_CLOSE_BLANK::USE_AMT		= 0.0
	AP_CLOSE_BLANK::DISCDAT		= ""
	AP_CLOSE_BLANK::DISAMT		= 0.0
	AP_CLOSE_BLANK::DUEDAT		= ""
	AP_CLOSE_BLANK::PONUM		= ""
	AP_CLOSE_BLANK::AP_ACCT		= ""
	AP_CLOSE_BLANK::CASH_ACCT	= ""
	AP_CLOSE_BLANK::CKNUM		= ""
	AP_CLOSE_BLANK::CKDAT		= ""
	AP_CLOSE_BLANK::CKDESC		= ""
	AP_CLOSE_BLANK::CKAMT		= 0.0
	AP_CLOSE_BLANK::UPDATED		= ""
	AP_CLOSE_BLANK::CLOSEDATE	= ""
	AP_CLOSE_BLANK::SELECTED	= "N"
	AP_CLOSE_BLANK::BATCH		= ""

	AP_CLOSE = AP_CLOSE_BLANK

550	INPUT LINE #APSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #APSYS.CH%, INP1$
		INP$ = LEFT(inp$, LEN(INP$)-2%) + EDIT$(INP1$, 4%)
		goto 560
	end if

	GOTO 550 IF INP$ = ""

	I2% = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2%-1%)
	DTA$ = RIGHT(INP$, I2%+1%)

 ! PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

		CASE "0", "CO#"
			AP_CLOSE::TRANKEY = RIGHT(DTA$, 8%)
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

		CASE "1"
			AP_CLOSE::VENNUM = DTA$

		CASE "2"
			AP_CLOSE::TRANKEY = DTA$

		CASE "3", "INV#", "INV.NO", "BANKACCT"
			AP_CLOSE::INVNUM = DTA$

		CASE "4", "INV.DATE"
			AP_CLOSE::INVDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))
			AP_CLOSE::CKDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

		CASE "5", "GROSS.AMT"
			AP_CLOSE::INVAMT = VAL(DTA$) / 100.0
			AP_CLOSE::CKAMT = VAL(DTA$) / 100.0
			AP_CLOSE::CKNUM = "1"

		case "6"
			! Not used (Net amount)

		CASE "7", "DISC.AMT"
			AP_CLOSE::DISAMT = VAL(DTA$) / 100.0

		CASE "8", "MONTH"
			AP_CLOSE::UPDATED = LEFT(AP_CLOSE::INVDAT, 4%) + DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				AP_CLOSE = AP_CLOSE_BLANK
				GOTO 550
			END IF

690			!
			! Write out record
			!
			PUT #AP_CLOSE.CH%

			PRINT AP_CLOSE::VENNUM

695			AP_CLOSE = AP_CLOSE_BLANK

		CASE ELSE
			PRINT "Undefined code '"; FLD$; "'"; DTA$; "'"
	END SELECT

	GOTO 550

800	!
	! Done
	!
	CLOSE AP_CLOSE.CH%

	PRINT "Store list"; STORELIST$

	GOTO 32767

	%PAGE

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

		CASE 550%
			RESUME 800

		CASE 690%
			PRINT "%Put"; AP_CLOSE::VENNUM; " "; ERR; " "; ERT$(ERR)
			RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
