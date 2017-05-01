1	! Convert robisons customer file

	option size = (integer long, real gfloat)

	%include "source:[ar.open]ar_35custom.hb"
	map (ar_35custom) ar_35custom_cdd ar_35custom

	%INCLUDE "SOURCE:[OE.OPEN]OE_regheader.HB"
	MAP (OE_regheader) OE_regheader_CDD OE_regheader

	%INCLUDE "SOURCE:[OE.OPEN]OE_regline.HB"
	MAP (OE_regline) OE_regline_CDD OE_regline

100	open "[rob]rback." for input as file 1%, &
		recordsize 512%

110	map (XXX) robmac$ = 512%

120	map (XXX) &
		rob.cusnum$ = 8%, &
		rob.flag$ = 3%, &
		rob.cusname$ = 30%, &
		rob.vc$ = 3%, &
		rob.pn$ = 16%, &
		rob.desc$ = 16%, &
		rob.ord$ = 6%, &
		rob.str$ = 5%, &
		rob.date$ = 9%, &
		rob.qbo$ = 6%, &
		rob.unitprice$ = 8%, &
		rob.totalprice$ = 11%, &
		rob.unitcrfet$ = 9%, &
		rob.totalcrfet$ = 9%, &
		rob.pcd$ = 4%, &
		rob.shipto$ = 6%, &
		rob.unitcost$ = 11%, &
		rob.totalcost$ = 11%

	thisorder$ = ""
	thisline% = 0%

200	%include "source:[ar.open]ar_35custom.cre"

210	%include "source:[OE.open]OE_regheader.cre"

220	%include "source:[OE.open]OE_regline.cre"

1000	linput #1%, xx$

	robmac$ = xx$

2100	IF THISORDER$ <> ROB.ORD$
	THEN
		THISORDER$ = ROB.ORD$
		THISLINE% = 0%

		OE_REGHEADER::ORDNUM	= STRING$(10%-LEN(ROB.ORD$),ASCII("0"))+ROB.ORD$
		OE_REGHEADER::ORDTYPE	= ""
		OE_REGHEADER::ORDCAT	= ""
		OE_REGHEADER::ORDDATE	= "19" + &
			MID(ROB.DATE$,7%,2%) + &
			MID(ROB.DATE$, 1%, 2%) + &
			MID(ROB.DATE$, 4%, 2%)
		OE_REGHEADER::ASTATUS	= "O"
		OE_REGHEADER::SDATE	= OE_REGHEADER::ORDDATE
		OE_REGHEADER::CUSNUM	= ROB.CUSNUM$
		OE_REGHEADER::SHIPNAM	= ""
		OE_REGHEADER::SHIPLIN	= ""
		OE_REGHEADER::ADD1	= ""
		OE_REGHEADER::ADD2	= ""
		OE_REGHEADER::ADD3	= ""
		OE_REGHEADER::CITY	= ""
		OE_REGHEADER::STATE	= ""
		OE_REGHEADER::ZIP	= ""
		OE_REGHEADER::COUNTRY	= ""
		OE_REGHEADER::CUSTPO	= ""
		OE_REGHEADER::SHIPVIA	= "U"
		OE_REGHEADER::TERMS	= ""
		OE_REGHEADER::DISC	= 0.0
		OE_REGHEADER::TAXCODE	= ""
		OE_REGHEADER::TAXFLAG	= ""
		OE_REGHEADER::LOCATION	= "ROBI"
		OE_REGHEADER::COMMAMT	= 0.0
		OE_REGHEADER::SALESMAN(0%)	= ""
		OE_REGHEADER::SALESMAN(1%)	= ""
		OE_REGHEADER::SALCOMM(0%)	= 0.0
		OE_REGHEADER::SALCOMM(0%)	= 0.0
		OE_REGHEADER::SHIPNO	= ""
		OE_REGHEADER::BATCH	= "000023"

		PUT #OE_REGHEADER.CH%
	END IF

	THISLINE% = THISLINE% + 1%

	OE_REGLINE::ORDNUM	= OE_REGHEADER::ORDNUM
	OE_REGLINE::LIN		= FORMAT$(THISLINE%, "<0>###")
	OE_REGLINE::TRANTYPE	= "01"
	OE_REGLINE::PRODUCT	= ROB.PN$
	OE_REGLINE::QTY		= VAL(ROB.QBO$)
	OE_REGLINE::TDATE	= "19" + &
			MID(ROB.DATE$,7%,2%) + &
			MID(ROB.DATE$, 1%, 2%) + &
			MID(ROB.DATE$, 4%, 2%)
	OE_REGLINE::PRICE	= VAL(ROB.UNITPRICE$)
	OE_REGLINE::DISCOUNT	= 0.0
	OE_REGLINE::COST	= VAL(ROB.UNITCOST$)
	OE_REGLINE::POSTDATE	= ""
	OE_REGLINE::POSTTIME	= ""
	OE_REGLINE::BATCH	= "000023"
	OE_REGLINE::SHIPNO	= ""
	OE_REGLINE::PROMO	= 0.0

	PUT #OE_REGLINE.CH%

	print "'"; ROB.ORD$; "' "; OE_REGLINE::LIN; "'";ROB.SHIPTO$;"'"

	goto 1000

32767	end
