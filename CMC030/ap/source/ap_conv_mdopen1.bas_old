1	%TITLE "Convert AP OPEN from MicroData"
	%SBTTL "AP_CONV_MDOPEN1"
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
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CONV_MDOPEN1/LINE
	!	$ LINK/EXEC:AP_EXE AP_CONV_MDOPEN1,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CONV_MDOPEN1.OBJ;*
	!
	! Author:
	!
	!	07/05/91 - Kevin Handy
	!
	! Modification history:
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
	!	03/05/99 - Kevin Handy
	!		Fix up some CDD weirdness (convert to not use CDD)
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP (AP_OPEN_DIST)		AP_OPEN_DIST_CDD	AP_OPEN_DIST
	MAP (AP_OPEN_DIST_BLANK)	AP_OPEN_DIST_CDD	AP_OPEN_DIST_BLANK

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	EXTERNAL LONG    FUNCTION DATE_DAYCODE
	EXTERNAL STRING  FUNCTION DATE_INVDCODE

	START_DATE% = DATE_DAYCODE("19671231")

	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$
	accnoc$ = mid(storeno$, 2%, 2%)

100	%include "source:[ap.open]ap_control.opn"

	get #ap_control.ch%, record 1%

	close #ap_control.ch%

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


	!======================================================================
	! AP_OPEN_DIST file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_OPEN_DIST.CH%, STAT%)
	CALL READ_DEVICE("AP_OPEN_DIST",AP_OPEN_DIST.DEV$, STAT%)
	CALL READ_PROTECTION("AP_OPEN_DIST",AP_OPEN_DIST.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AP_OPEN_DIST.PRO$, STAT%)

	AP_OPEN_DIST.NAME$ = AP_OPEN_DIST.DEV$+"AP_OPEN_DIST.LED"

	OPEN AP_OPEN_DIST.NAME$ FOR OUTPUT AS FILE AP_OPEN_DIST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN_DIST, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AP_OPEN_DIST::TRANKEY, &
			AP_OPEN_DIST::SLINE &
		), &
		ALTERNATE KEY &
			AP_OPEN_DIST::BTHNUM &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)



250	OPEN APSYS_ASC.DEV$ + "APO.ASC" FOR INPUT AS FILE APSYS.CH%, &
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
	AP_OPEN_BLANK::AP_ACCT		= ap_control::ap_acct
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
	A1$, A2$, A3$, A4$, A5$ = ""

550	INPUT LINE #APSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #APSYS.CH%, INP1$
		INP$ = LEFT(inp$,LEN(INP$)-2%) + EDIT$(INP1$, 4%)
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

		CASE "1", "VOUNUMBERS"
			AP_OPEN::VENNUM = DTA$

		CASE "2", "BANKACCT"
			AP_OPEN::ckdesc = dta$

		CASE "3", "INV.DATE"
			AP_OPEN::INVDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

		CASE "4", "GAMT"
			SELECT DTA$
			CASE "HOLD"
			CASE "PAID"
			CASE ELSE
				AP_OPEN::DISCDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))
			END SELECT

		CASE "5", "DAMT"
			AP_OPEN::INVAMT = VAL(DTA$) / 100.0

		CASE "9", "G/L#"
			A1$ = DTA$

		CASE "10", "DIST.AMT"
			A2$ = DTA$

		CASE "11", "AMT-PD-DT"
			A3$ = DTA$

		CASE "12", "DATE-PAID"
			A4$ = DTA$

		CASE "13", "JOB#"
			AP_OPEN::USE_JOB_NUM = DTA$

		CASE "14", "REC#"
			DTA$ = DTA$ + "01"
			DTA$ = STRING$(LEN(AP_OPEN::PONUM)-LEN(DTA$),ASCII("0")) + DTA$
			AP_OPEN::PONUM = DTA$

		CASE "15", "DISB.ACCT"
			AP_OPEN::cash_ACCT = fnform_acct$(DTA$)

		CASE "16", "*A16"
			A5$ = DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				AP_OPEN = AP_OPEN_BLANK
				A1$, A2$, A3$, A4$, A5$ = ""
				GOTO 550
			END IF

			if ap_open::invnum = ""
			then
				ap_open::invnum = ap_open::ckdesc
			end if

690			IF (A3$ <> "")
			THEN
				I1% = INSTR(1%, A3$+'253'C, '253'C)
				A3A$ = LEFT(A3$, I1% - 1%)
				A3$ = RIGHT(A3$, I1%+1%)
				I2% = INSTR(1%, A4$+'253'C, '253'C)
				A4A$ = LEFT(A4$, I1% - 1%)
				A4$ = RIGHT(A4$, I1%+1%)
				!I3% = INSTR(1%, A5$+'253'C, '253'C)
				A5A$ = LEFT(A5$, I1% - 1%)
				A5$ = RIGHT(A5$, I1%+1%)

				AP_OPEN::CKNUM = A5A$
				AP_OPEN::CKDAT = DATE_INVDCODE(START_DATE% + VAL%(A4A$))
				AP_OPEN::CKAMT = VAL(A3A$) / 100.0
			END IF

			!
			! Write out record
			!
			PUT #AP_OPEN.CH%

			PRINT AP_OPEN::VENNUM

691			WHILE (A3$ <> "")
				AP_OPEN::INVAMT = 0.0
				AP_OPEN::DISCDAT = ""
				AP_OPEN::DISAMT = 0.0

				I1% = INSTR(1%, A3$+'253'C, '253'C)
				A3A$ = LEFT(A3$, I1% - 1%)
				A3$ = RIGHT(A3$, I1%+1%)
				I2% = INSTR(1%, A4$+'253'C, '253'C)
				A4A$ = LEFT(A4$, I1% - 1%)
				A4$ = RIGHT(A4$, I1%+1%)
				!I3% = INSTR(1%, A5$+'253'C, '253'C)
				A5A$ = LEFT(A5$, I1% - 1%)
				A5$ = RIGHT(A5$, I1%+1%)

				AP_OPEN::CKNUM = A5A$
				AP_OPEN::CKDAT = DATE_INVDCODE(START_DATE% + VAL%(A4A$))
				AP_OPEN::CKAMT = VAL(A3A$) / 100.0

				PUT #AP_OPEN.CH%
			NEXT

692			LC% = 0%

			WHILE (A1$ <> "")
				LC% = LC% + 1%
				I1% = INSTR(1%, A1$+'253'C, '253'C)
				A1A$ = LEFT(A1$, I1% - 1%)
				A1$ = RIGHT(A1$, I1%+1%)
				I1% = INSTR(1%, A2$+'253'C, '253'C)
				A2A$ = LEFT(A2$, I1% - 1%)
				A2$ = RIGHT(A2$, I1%+1%)

				AP_OPEN_DIST::TRANKEY = AP_OPEN::TRANKEY
				AP_OPEN_DIST::SLINE = FORMAT$(LC%, "<0>###")
				AP_OPEN_DIST::PONUM = AP_OPEN::PONUM
				AP_OPEN_DIST::PO_LINE = ""
				AP_OPEN_DIST::ACCT = fnform_acct$(A1A$)
				AP_OPEN_DIST::SUBACC = AP_OPEN::USE_JOB_NUM
				AP_OPEN_DIST::OPERATION = ""
				AP_OPEN_DIST::UNITS = 0.0
				AP_OPEN_DIST::AMOUNT = VAL(A2A$) / 100.0
				AP_OPEN_DIST::DISCAMT = 0.0
				AP_OPEN_DIST::USE_TAX_FLAG = "N"
				AP_OPEN_DIST::BTHNUM = ""

				PUT #AP_OPEN_DIST.CH%
			NEXT

695			AP_OPEN = AP_OPEN_BLANK
			A1$, A2$, A3$, A4$, A5$ = ""

		CASE ELSE
 !			PRINT "Undefined code '"; FLD$; "'"
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
