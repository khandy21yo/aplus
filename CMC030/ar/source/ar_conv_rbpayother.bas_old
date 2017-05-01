1	! Convert robisons customer file

	option size = (integer long, real gfloat)

	external real function func_round

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN) AR_OPEN_CDD AR_OPEN

100	open "RPAY." for input as file 1%, &
		recordsize 512%

	open "RTRN." for input as file 2%, &
		recordsize 512%

110	map (XXX) robmac$ = 512%

120	record rbpay_cdd
		string cusnum = 8%, &
		string xstr = 4%, &
		string xdate = 10%, &
		string xtype = 6%, &
		string xref = 11%, &
		string discount = 17%, &
		string payment = 14%, &
		string total = 13%, &
		string s = 6%
	end record

	map (XXX) rbpay_cdd rbpay

	record rbtrn_cdd
		string cusnum = 8%, &
		string xstr = 3%, &
		string xdate = 9%, &
		string xtype = 3%, &
		string xref = 7%, &
		string core = 8%, &
		string fet = 8%, &
		string amount = 11%, &
		string discount = 9%, &
		string tax = 8%, &
		string code = 5%, &
		string freight = 7%, &
		string other = 9%, &
		string ref2 = 5%, &
		string labor = 8%, &
		string total = 10%, &
		string s = 3%, &
		string cost = 10%
	end record

	map (YYY) rbtrn_cdd rbtrn
	map (YYY) robMMM$ = 512%

	dim rbpay_cdd pays(50%)

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

	OPEN AR_OPEN.NAME$ for output AS FILE AR_OPENM.CH%, &
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

	OPEN AR_OPEN.NAME$ for output AS FILE AR_OPENR.CH%, &
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

	this_cusnum$ = ""
	this% = 0%
	tinv = 0.0
	tdis = 0.0

	LINPUT #2%, YY$
	ROBMMM$ = YY$

1000	linput #1%, xx$

	robmac$ = xx$

	GOTO 1000 IF EDIT$(rbpay::s, -1%) = "OP"

	if rbpay::cusnum <> this_cusnum$
	then
		gosub DoVendor
		this_cusnum$ = rbpay::cusnum
		this% = 0%
		tinv = 0.0
		tdis = 0.0
	end if

	this% = this% + 1%
	pays(this%) = rbpay

	goto 1000

 DoVendor:
1100	!
	! Seek first record in TRN file
	!
	GOTO 2900 IF THIS% = 0%

	WHILE (RBTRN::CUSNUM < PAYS(1%)::CUSNUM)

		LINPUT #2%, YY$
		ROBMMM$ = YY$
	NEXT

1200	WHILE (RBTRN::CUSNUM = PAYS(1%)::CUSNUM)

		!
		! Handle according to type
		!
		SELECT EDIT$(RBTRN::S, -1)
		CASE "PD"
			tinv = func_round(tinv + val(rBTRN::total), 2%)
			tdis = func_round(tdis + val(rBTRN::discount), 2%)

		CASE "PP"
 !			tinv = func_round(tinv + val(rBTRN::total), 2%)
 !			tdis = func_round(tdis + val(rBTRN::discount), 2%)
			AR_OPEN::INVNUM = EDIT$(RbTRN::xREF,-1%)

		END SELECT

		LINPUT #2%, YY$
		ROBMMM$ = YY$
	NEXT

1300	!
	! Spread out invoce information now
	!
	for i% = 1% to this%
		x = func_round(val(pays(i%)::total) - tinv, 2%)
		tinv = 0.0
		if x < 0
		then
			tinv = -x
			x = 0
		end if
		rset pays(i%)::total = num1$(x)

		x = func_round(val(pays(i%)::discount) - tdis, 2%)
		tdis = 0.0
		if x < 0
		then
			tdis = -x
			x = 0
		end if
		pays(i%)::discount = num1$(x)

	next i%


2100	for i% = 1% to this%

		goto 2190 if val(pays(i%)::total) = 0.0

		IF INSTR(1%, "00", EDIT$(pays(i%)::xstr,01%))
		THEN
			acct$ = "1200.4000"
		ELSE
			ACCT$ = "1200.1000"
		END IF

		inum$ = edit$(pays(i%)::xref, -1%)
		inum$ = string$(6%-len(inum$), ascii("0")) + inum$

		tdate$ = edit$(pays(i%)::xdate,-1%)

		AR_OPEN::CUSNUM		= pays(i%)::cusnum
		AR_OPEN::TRATYP		= "09"
		AR_OPEN::TRADAT		= "19" + mid(tdate$,7%,2%) + &
					mid(tdate$, 1%, 2%) + &
					mid(tdate$, 4%, 2%)
		AR_OPEN::SALAMT		= -val(pays(i%)::total)
		AR_OPEN::DISAMT		= val(pays(i%)::discount)
		AR_OPEN::OTHCHG		= 0.0
		AR_OPEN::RECNUM		= ""
		AR_OPEN::CHKNUM		= ""
		AR_OPEN::ARACCT		= ACCT$
		AR_OPEN::SUBACC		= ""
		AR_OPEN::DESCR		= ""
		AR_OPEN::SALNUM		= ""
		AR_OPEN::BATCH		= "000021"
		AR_OPEN::UPDATED	= ""
		AR_OPEN::CLOSEDATE	= ""

		IF acct$ = "1200.4000"
		THEN
			PUT #AR_OPENM.CH%
		ELSE
			PUT #AR_OPENR.CH%
		END IF

		print ar_open::cusnum; " "; ar_open::invnum; " "; tdate$; " "; &
			format$(AR_OPEN::SALAMT, "###,###.##")


2190	next i%

2900	return

32767	end
