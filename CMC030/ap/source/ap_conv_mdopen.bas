1	%TITLE "Convert AP OPEN from MicroData"
	%SBTTL "AP_CONV_MDOPEN"
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
	!	$ BAS AP_SOURCE:AP_CONV_MDOPEN/LINE
	!	$ LINK/EXEC:AP_EXE AP_CONV_MDOPEN,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CONV_MDOPEN.OBJ;*
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
	!		Lose line 800 (Dead Code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	def fnform_acct$(a$) = left(a$, 2%) + "-" + mid(a$,3%, 3%) + "-" + &
		mid(trm$(a$), 6%, 2%) + "-" + accnoc$

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN
	MAP (AP_OPEN_BLANK)	AP_OPEN_CDD	AP_OPEN_BLANK

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

 !	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.CRE"
	!======================================================================
	! AP_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_OPEN.CH%, STAT%)
	CALL READ_DEVICE("AP_OPEN",AP_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AP_OPEN",AP_OPEN.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AP_OPEN.PRO$, STAT%)

	AP_OPEN.NAME$ = AP_OPEN.DEV$+"AP_OPEN.LED"

	OPEN AP_OPEN.NAME$ FOR OUTPUT AS FILE AP_OPEN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::TRANKEY &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_OPEN::BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN APSYS_ASC.DEV$ + "VOUCH.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

	AP_OPEN_BLANK::VENNUM		= ""
	AP_OPEN_BLANK::TRANKEY		= ""
	AP_OPEN_BLANK::TRANKEY_DATE	= ""
	AP_OPEN_BLANK::INVNUM		= ""
	AP_OPEN_BLANK::INVDAT		= ""
	AP_OPEN_BLANK::CODE_1099	= ""
	AP_OPEN_BLANK::AMT_1099		= 0.0
	AP_OPEN_BLANK::USE_JOB_NUM	= ""
	AP_OPEN_BLANK::USE_AMT		= 0.0
	AP_OPEN_BLANK::DISCDAT		= ""
	AP_OPEN_BLANK::DISAMT		= 0.0
	AP_OPEN_BLANK::DUEDAT		= ""
	AP_OPEN_BLANK::PONUM		= ""
	AP_OPEN_BLANK::AP_ACCT		= ""
	AP_OPEN_BLANK::CASH_ACCT	= ""
	AP_OPEN_BLANK::CKNUM		= ""
	AP_OPEN_BLANK::CKDAT		= ""
	AP_OPEN_BLANK::CKDESC		= ""
	AP_OPEN_BLANK::CKAMT		= 0.0
	AP_OPEN_BLANK::UPDATED		= ""
	AP_OPEN_BLANK::CLOSEDATE	= ""
	AP_OPEN_BLANK::SELECTED		= "N"
	AP_OPEN_BLANK::BATCH		= ""

	AP_OPEN = AP_OPEN_BLANK

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
			AP_OPEN::TRANKEY = RIGHT(DTA$, 8%)
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

		CASE "1"
			AP_OPEN::VENNUM = DTA$

		CASE "2", "INV.NO", "BANKACCT"
			AP_OPEN::INVNUM = DTA$

		CASE "3", "INV.DATE"
			AP_OPEN::INVDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

		CASE "4", "*A4"
			SELECT DTA$
			CASE "PAID"
				AP_OPEN::CKAMT = AP_OPEN::INVAMT
				AP_OPEN::CKDAT = AP_OPEN::INVDAT
				AP_OPEN::CKNUM = "99999"
			CASE "HOLD"
			CASE ELSE
				AP_OPEN::DISCDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))
			END SELECT

		CASE "5", "GROSS.AMT"
			AP_OPEN::INVAMT = VAL(DTA$) / 100.0

		case "6"
			! Not used (Net amount)

		CASE "7", "DISC.AMT"
			AP_OPEN::DISAMT = VAL(DTA$) / 100.0

		CASE "8", "CTR"
			! Not used (Number of distributations)

		CASE "9", "LOC"
			AP_OPEN::AP_ACCT = fnform_acct$(DTA$)

		CASE "11", "AUX.DESC"
			AP_OPEN::CKDESC = DTA$

		CASE "12", "JOB#"
			AP_OPEN::USE_JOB_NUM = DTA$

		CASE "13", "REC#"
			AP_OPEN::PONUM = DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				AP_OPEN = AP_OPEN_BLANK
				GOTO 550
			END IF

690			!
			! Write out record
			!
			PUT #AP_OPEN.CH%

			PRINT AP_OPEN::VENNUM

695			AP_OPEN = AP_OPEN_BLANK

		CASE ELSE
			PRINT "Undefined code '"; FLD$; "'"; DTA$; "'"
	END SELECT

	GOTO 550

 !800
	!
	! Done
	!
 !	CLOSE AP_OPEN.CH%
 !
 !	PRINT "Store list"; STORELIST$
 !
 !	GOTO 32767

	%PAGE

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

		CASE 550%

		CASE 690%
			PRINT "%Put"; AP_OPEN::VENNUM; " "; ERR; " "; ERT$(ERR)
			RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
