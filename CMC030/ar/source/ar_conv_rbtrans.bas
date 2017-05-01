1	! Convert robisons customer file

	option size = (integer long, real gfloat)

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN) AR_OPEN_CDD AR_OPEN

100	open "[rob]rtrn." for input as file 1%, &
		recordsize 512%

110	map (XXX) robmac$ = 512%

120	map (XXX) &
		rob.cusnum$ = 8%, &
		rob.str$ = 3%, &
		rob.date$ = 9%, &
		rob.type$ = 3%, &
		rob.ref$ = 7%, &
		rob.core$ = 8%, &
		rob.fet$ = 8%, &
		rob.amount$ = 11%, &
		rob.discount$ = 9%, &
		rob.tax$ = 8%, &
		rob.code$ = 5%, &
		rob.freight$ = 7%, &
		rob.other$ = 9%, &
		rob.ref2$ = 5%, &
		rob.labor$ = 8%, &
		rob.total$ = 10%, &
		rob.s$ = 3%, &
		rob.cost$ = 10%

210 !	%include "source:[ar.open]ar_open.cre"
	!======================================================================
	! AR_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPENM.CH%, STAT%)
	CALL READ_DEVICE("AR_OPEN", AR_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AR_OPEN", AR_OPEN.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(AR_OPEN.PRO$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$ + "AR_OPEN.MARC"

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
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

	!======================================================================
	! AR_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPENR.CH%, STAT%)
	CALL READ_DEVICE("AR_OPEN", AR_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AR_OPEN", AR_OPEN.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(AR_OPEN.PRO$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$ + "AR_OPEN.ROBI"

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
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


1000	linput #1%, xx$

	robmac$ = xx$

2100	IF INSTR(1%, "00", EDIT$(ROB.STR$, -1%))
	THEN
		acct$ = "1200.4000"	! MARCO
	ELSE
		acct$ = "1200.1000"	! MARCO
	END IF

	xtype$ = left(edit$(rob.type$, -1%), 1%)

	SIGN = 1.0

	select xtype$

	case "S"
		AR_OPEN::INVNUM		= "SC" + edit$(rob.ref$, -1%)
		AR_OPEN::TRATYP		= "04"

	case "M"
		AR_OPEN::INVNUM		= edit$(rob.ref$, -1%)
		AR_OPEN::TRATYP		= "08"
		SIGN = -1.0

	case "P", "C"
		AR_OPEN::INVNUM		= edit$(rob.ref$, -1%)
		AR_OPEN::TRATYP		= "09"
		SIGN = -1.0

	case "R"
		AR_OPEN::INVNUM		= edit$(rob.ref$, -1%)
		AR_OPEN::TRATYP		= "01"
		SIGN = -1.0

	case else
		AR_OPEN::INVNUM		= edit$(rob.ref$, -1%)
		AR_OPEN::TRATYP		= "01"

	end select

	AR_OPEN::CUSNUM		= ROB.CUSNUM$
	AR_OPEN::TRADAT		= "19" + mid(rob.date$, 8%, 2%) + &
				mid(rob.date$, 2%, 2%) + &
				mid(rob.date$, 5%, 2%)
	AR_OPEN::SALAMT		= val(rob.total$) * SIGN
	AR_OPEN::DISAMT		= val(rob.discount$) * SIGN
	AR_OPEN::OTHCHG		= (val(rob.tax$) + &
				val(rob.freight$) + &
				val(rob.other$) + &
				val(rob.labor$)) * SIGN
	AR_OPEN::RECNUM		= ""
	AR_OPEN::CHKNUM		= ""
	AR_OPEN::ARACCT		= ACCT$
	AR_OPEN::SUBACC		= ""
	AR_OPEN::DESCR		= edit$(rob.ref2$, -1%)
	AR_OPEN::SALNUM		= ""
	AR_OPEN::BATCH		= "000010"
	AR_OPEN::UPDATED	= ""
	AR_OPEN::CLOSEDATE	= ""

	IF acct$ = "1200.4000"	! MARCO
	THEN
		PUT #AR_OPENM.CH%
	ELSE
		PUT #AR_OPENR.CH%
	END IF

	IF EDIT$(ROB.S$, -1%) = "PD"
	THEN
		AR_OPEN::TRATYP		= "09"
		AR_OPEN::SALAMT		= -val(rob.total$) * SIGN
		AR_OPEN::DISAMT		= 0.0
		AR_OPEN::OTHCHG		= 0.0
		AR_OPEN::ARACCT		= ACCT$
		AR_OPEN::BATCH		= "000020"

		IF acct$ = "1200.4000"
		THEN
			PUT #AR_OPENM.CH%
		ELSE
			PUT #AR_OPENR.CH%
		END IF
	END IF

	print ar_open::cusnum; " "; ar_open::invnum; " "; ROB.S$

	goto 1000

32767	end
