1	! Convert robisons customer file

	option size = (integer long, real gfloat)

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN) AR_OPEN_CDD AR_OPEN

100	open "[rob]RPAY." for input as file 1%, &
		recordsize 512%

110	map (XXX) robmac$ = 512%

120	map (XXX) &
		rob.cusnum$ = 8%, &
		rob.str$ = 4%, &
		rob.date$ = 10%, &
		rob.type$ = 6%, &
		rob.ref$ = 11%, &
		rob.discount$ = 17%, &
		rob.payment$ = 14%, &
		rob.total$ = 13%, &
		rob.s$ = 6%

210 !	%include "source:[ar.open]ar_open.cre"

	!======================================================================
	! AR_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPENM.CH%, STAT%)
	CALL READ_DEVICE("AR_OPEN",AR_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AR_OPEN",AR_OPEN.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AR_OPEN.PRO$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$+"AR_OPEN.MARC"

	OPEN AR_OPEN.NAME$ AS FILE AR_OPENM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_OPEN, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::BATCH, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::SALNUM, &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::INVNUM, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

	!======================================================================
	! AR_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPENR.CH%, STAT%)
	CALL READ_DEVICE("AR_OPEN",AR_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AR_OPEN",AR_OPEN.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AR_OPEN.PRO$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$+"AR_OPEN.ROBI"

	OPEN AR_OPEN.NAME$ AS FILE AR_OPENR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_OPEN, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::BATCH, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::SALNUM, &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::INVNUM, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


1000	linput #1%, xx$

	robmac$ = xx$

	GOTO 1000 IF EDIT$(ROB.S$, -1%) <> "OP"

2100	IF INSTR(1%, "00", EDIT$(ROB.STR$,01%))
	THEN
		acct$ = "1200.4000"
	ELSE
		ACCT$ = "1200.1000"
	END IF

	inum$ = edit$(rob.ref$, -1%)
	inum$ = string$(6%-len(inum$), ascii("0")) + inum$

	tdate$ = edit$(rob.date$,-1%)

	AR_OPEN::CUSNUM		= ROB.CUSNUM$
	AR_OPEN::INVNUM		= inum$
	AR_OPEN::TRATYP		= "09"
	AR_OPEN::TRADAT		= "19" + mid(tdate$,7%,2%) + &
				mid(tdate$, 1%, 2%) + &
				mid(tdate$, 4%, 2%)
	AR_OPEN::SALAMT		= -val(rob.total$)
	AR_OPEN::DISAMT		= val(rob.discount$)
	AR_OPEN::OTHCHG		= 0.0
	AR_OPEN::RECNUM		= ""
	AR_OPEN::CHKNUM		= ""
	AR_OPEN::ARACCT		= ACCT$
	AR_OPEN::SUBACC		= ""
	AR_OPEN::DESCR		= ""
	AR_OPEN::SALNUM		= ""
	AR_OPEN::BATCH		= "000020"
	AR_OPEN::UPDATED	= ""
	AR_OPEN::CLOSEDATE	= ""

	IF acct$ = "1200.4000"
	THEN
		PUT #AR_OPENM.CH%
	ELSE
		PUT #AR_OPENR.CH%
	END IF

	print ar_open::cusnum; " "; ar_open::invnum; " "; tdate$

	goto 1000

32767	end
