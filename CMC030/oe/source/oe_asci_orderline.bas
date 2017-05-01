1	!
	! Dump ORDER JOURNAL in to an ascii file
	!
	! History:
	!
	!	08/31/2000 - Kevin Handy
	!		Original version
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%include "source:[OE.open]OE_ORDERLINE.HB"
	map (OE_ORDERLINE) OE_ORDERLINE_cdd OE_ORDERLINE

	fmt% = 132%

	def fnformat$(a$, fmtflg%)
		select fmtflg%
		case 0%
			fnformat$ = EDIT$(a$, 140%)
		case 132%
			b$ = a$
			qbz% = instr(1%, b$, '"')
			while(qbz%)
				mid(b$, qbz%, 1%) = " "
				qbz% = instr(qbz%, b$, '"')
			next
			qbz% = instr(1%, b$, ',')
			while(qbz%)
				mid(b$, qbz%, 1%) = " "
				qbz% = instr(qbz%, b$, ',')
			next
			fnformat$ = '"' + EDIT$(b$, 140%) + '"'
		end select
	fnend

	select fmt%
	case 132%
		fmtflag$ = ", "
	case else
		fmtflag$ = "	"
	end select

1000	input "Batch number: "; batch_no$
	%include "source:[OE.open]OE_ORDERLINE.opn"

1010	open "OEORDERLINE_" + batch_no$ + ".txt" for output as file 1%, &
		recordsize 511%

	TEXT$ = "ORDNUM" + fmtflag$ + &
		"PRODUCT" + fmtflag$ + &
		"ORDQTY" + fmtflag$ + &
		"SHPQTY" + fmtflag$ + &
		"PRICE" + fmtflag$ + &
		"DISCOUNT" + fmtflag$ + &
		"COST" + fmtflag$ + &
		"REQDATE" + fmtflag$ + &
		"PROMO" + fmtflag$ + &
		"MISCH" + fmtflag$ + &
		"BCKQTY" + fmtflag$ + &
		"NOTES1" + fmtflag$ + &
		"NOTES2" + fmtflag$ + &
		"SUBACCT" + fmtflag$ + &
		"LIN" + fmtflag$ + &
		"MISCH2" + fmtflag$ + &
		"NETLINE"

	print #1%, text$

2000	reset #OE_ORDERLINE.ch%

2100	when error in
		get #OE_ORDERLINE.ch%, regardless
	use
		continue 9000 if err = 11%
		exit handler
	end when

	NET = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
		(OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - OE_ORDERLINE::DISCOUNT / 100.0) + &
		OE_ORDERLINE::MISCH + &
		OE_ORDERLINE::MISCH2, 2%)

	TEXT$ = &
		fnformat$(OE_ORDERLINE::ORDNUM, fmt%) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::PRODUCT, fmt%) + fmtflag$ + &
		num1$(OE_ORDERLINE::ORDQTY) + fmtflag$ + &
		num1$(OE_ORDERLINE::SHPQTY) + fmtflag$ + &
		num1$(OE_ORDERLINE::PRICE) + fmtflag$ + &
		num1$(OE_ORDERLINE::DISCOUNT) + fmtflag$ + &
		num1$(OE_ORDERLINE::COST) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::REQDATE, fmt%) + fmtflag$ + &
		num1$(OE_ORDERLINE::PROMO) + fmtflag$ + &
		num1$(OE_ORDERLINE::MISCH) + fmtflag$ + &
		num1$(OE_ORDERLINE::BCKQTY) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::NOTES1, fmt%) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::NOTES2, fmt%) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::SUBACCT, fmt%) + fmtflag$ + &
		fnformat$(OE_ORDERLINE::LIN, fmt%) + fmtflag$ + &
		num1$(OE_ORDERLINE::MISCH2) + fmtflag$ + &
		NUM1$(NET)

	print #1%, text$

	goto 2100

9000	close #1%
	close OE_ORDERLINE.ch%

32767	end
