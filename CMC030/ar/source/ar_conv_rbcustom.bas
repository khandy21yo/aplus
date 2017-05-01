1	! Convert robisons customer file

	option size = (integer long, real gfloat)

	%include "source:[ar.open]ar_35custom.hb"
	map (ar_35custom) ar_35custom_cdd ar_35custom

100	open "rcust." for input as file 1%, &
		recordsize 512%

110	map (XXX) robmac$ = 512%

120	map (XXX) &
		rob.cusnum$ = 10%, &
		rob.code$ = 7%, &
		rob.st$ = 3%, &
		rob.crlmt$ = 8%, &
		rob.30$ = 12%, &
		rob.name$ = 29%, &
		rob.slsman$ = 2%, &
		rob.crbal$ = 9%, &
		rob.60$ = 11%, &
		rob.crntmosales$ = 12%, &
		rob.mcost$ = 13%, &
		rob.add$ = 33%, &
		rob.hdc$ = 2%, &
		rob.90$ = 11%, &
		rob.crntyrsales$ = 12%, &
		rob.ycost$ = 13%, &
		rob.city$ = 16%, &
		rob.state$ = 3%, &
		rob.zip$ = 10%, &
		rob.taxco$ = 5%, &
		rob.foreward$ = 9%, &
		rob.current$ = 12%, &
		rob.lastyrsales$ = 12%, &
		rob.lycost$ = 13%, &
		rob.phone$ = 12%, &
		rob.taxpercent$ = 7%, &
		rob.surplus$ = 11%, &
		rob.servchg$ = 11%, &
		rob.memo$ = 23%, &
		rob.lastsale$ = 5%, &
		rob.bank$ = 11%, &
		rob.futures$ = 12%, &
		rob.lastpmt$ = 8%, &
		rob.lastpmtamt$ = 10%, &
		rob.acctbal$ = 11%

200 !	%include "source:[ar.open]ar_35custom.cre"
	!======================================================================
	! AR_35CUSTOM file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_35CUSTOMM.CH%, STAT%)
	CALL READ_DEVICE("AR_35CUSTOM", AR_35CUSTOM.DEV$, STAT%)
	CALL READ_PROTECTION("AR_35CUSTOM", AR_35CUSTOM.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(AR_35CUSTOM.PRO$, STAT%)

	AR_35CUSTOM.NAME$ = AR_35CUSTOM.DEV$ + "AR_35CUSTOM.MARC"

	OPEN AR_35CUSTOM.NAME$ AS FILE AR_35CUSTOMM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_35CUSTOM, &
		BUFFER 32%, &
		EXTENDSIZE 27%, &
		PRIMARY KEY &
			AR_35CUSTOM::CUSNUM, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::TTYPE, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::CATEGORY, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::ALPSRT, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

	!======================================================================
	! AR_35CUSTOM file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_35CUSTOMR.CH%, STAT%)
	CALL READ_DEVICE("AR_35CUSTOM", AR_35CUSTOM.DEV$, STAT%)
	CALL READ_PROTECTION("AR_35CUSTOM", AR_35CUSTOM.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(AR_35CUSTOM.PRO$, STAT%)

	AR_35CUSTOM.NAME$ = AR_35CUSTOM.DEV$ + "AR_35CUSTOM.ROBI"

	OPEN AR_35CUSTOM.NAME$ AS FILE AR_35CUSTOMR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_35CUSTOM, &
		BUFFER 32%, &
		EXTENDSIZE 27%, &
		PRIMARY KEY &
			AR_35CUSTOM::CUSNUM, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::TTYPE, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::CATEGORY, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::ALPSRT, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


1000	linput #1%, xx$

	robmac$ = xx$
	code$ = edit$(rob.CoDe$, -1%)
	taxco$ = edit$(rob.taxco$, -1%)

2000	ar_35custom::cusnum	= rob.cusnum$
	ar_35custom::cusnam	= rob.name$
	IF ROB.STATE$ = "ID"
	THEN
		ar_35custom::taxflag	= "1"
	ELSE
		ar_35custom::taxflag	= "5"
	END IF

	SELECT LEFT(CODE$, 1%)
	CASE "1"
		ar_35custom::ttype	= "P1" !RT
	CASE "2"
		ar_35custom::ttype	= "P2" !WS
	CASE "3"
		ar_35custom::ttype	= "P3" !DL
	CASE "4"
		ar_35custom::ttype	= "P4" !JP
	CASE "5"
		ar_35custom::ttype	= "P5" !DS
	CASE "6"
		ar_35custom::ttype	= "P6" !CS
	CASE ELSE
		ar_35custom::ttype	= "P1"
	END SELECT

	SELECT TAXCO$
		CASE "0002"
			ar_35custom::taxflag	= "4"
		CASE "0004"
			ar_35custom::taxflag	= "4"
	END SELECT

	ar_35custom::category	= ""
	ar_35custom::bdate	= "19920101"
	ar_35custom::sstatus	= "A"
	ar_35custom::edate	= ""
	ar_35custom::add1	= rob.add$
	ar_35custom::add2	= ""
	ar_35custom::add3	= ""
	CITY$ = TRM$(ROB.CITY$)
	CITY$ = LEFT(CITY$, LEN(CITY$) - 1%) IF RIGHT(CITY$, LEN(CITY$)) = ","
	ar_35custom::city	= city$
	ar_35custom::state	= rob.state$
	ar_35custom::zip	= rob.zip$
	ar_35custom::country	= ""
	ar_35custom::county	= ""
	ar_35custom::phone	= LEFT(rob.phone$, 3%) + &
		MID(ROB.PHONE$, 5%, 3%) + &
		RIGHT(ROB.PHONE$, 9%)
	ar_35custom::method	= "O"
	SELECT MID(CODE$, 4%, 1%)
	CASE "X"
		ar_35custom::stmtflg	= "N"
		ar_35custom::serchrg	= "N"
	CASE "N"
		ar_35custom::stmtflg	= "N"
		ar_35custom::serchrg	= "Y"
	CASE "Y"
		ar_35custom::stmtflg	= "Y"
		ar_35custom::serchrg	= "Y"
	CASE "S"
		ar_35custom::stmtflg	= "Y"
		ar_35custom::serchrg	= "N"
	END SELECT

	ar_35custom::ALPSRT	= rob.name$
	ar_35custom::taxcode	= ROB.STATE$
	ar_35custom::taxexemp	= ""
	IF INSTR(1%, "00", EDIT$(ROB.ST$, -1%))
	THEN
		ar_35custom::location	= "MARC"
		!acct$ = "1200.4000"
	ELSE
		ar_35custom::location	= "ROBI"
		!acct$ = "1200.1000"
	END IF
	ar_35custom::terms	= MID(CODE$, 5%, 1%)
	ar_35custom::carrier	= "U"
	ar_35custom::salesman	= ""
	ar_35custom::creditlim	= val(rob.crlmt$)
	ar_35custom::discount	= 0.0
	if mid(code$, 3%, 1%) = "N"
	then
		ar_35custom::backorder	= "N"
	ELSE
		ar_35custom::backorder	= "Y"
	END IF

2090	IF ar_35custom::location	= "MARC"
	THEN
		put #ar_35customM.ch%
	ELSE
		put #ar_35customR.ch%
	END IF

	print ar_35custom::cusnum, CODE$


	goto 1000

32767	end
