1	OPTION SIZE = (INTEGER LONG, REAL DOUBLE)

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP (PP_MONTHLY) PP_MONTHLY_CDD PP_MONTHLY

	MAP (CVT) &
		DOUBLE XVALUE
	MAP (CVT) &
		XVALUE$ = 8%


	DEF FNCVT(X$) = CVT$F(X$)

	MAP (DXA) &
		BFR$ = 256%
	MAP (DXA) &
		GASTRA.CUSNUM$=06%, &
		GASTRA.VEHICLE$=07%, &
		GASTRA.DRIVER$=07%, &
		GASTRA.TRNDATE$=02%, &
		GASTRA.TRNTIME$=02%, &
		GASTRA.SELFRAN$=03%, &
		GASTRA.SITECODE$=02%, &
		GASTRA.SITETYPE$=01%, &
		GASTRA.PRODUCT$=05%, &
		GASTRA.UNIT$=02%, &
		GASTRA.QUANTITY$=08%, &
		GASTRA.ODOM$=08%, &
		GASTRA.STYPE$=01%, &
		GASTRA.FTYPE$=01%, &
		GASTRA.SELLPRICE$=08%, &
		GASTRA.TRANCOST$=08%, &
		GASTRA.MISCKEYB$=09%, &
		GASTRA.TRNTYPE$=02%, &
		GASTRA.DISCOUNT$=03%, &
		GASTRA.ICBDATE$=02%, &
		GASTRA.TRNNUM$=05%, &
		GASTRA.STAXRATE$=08%, &
		GASTRA.PUMP$=02%, &
		GASTRA.BUYFRAN$=03%, &
		GASTRA.CAPDATE$=02%, &
		GASTRA.CAPTIME$=02%, &
		GASTRA.POSTBNUM$=04%, &
		GASTRA.TRANSOURCE$=01%, &
		GASTRA.EDITACT$=01%, &
		GASTRA.JULIANDAY$=03%, &
		GASTRA.RSTATION$=01%, &
		GASTRA.IDENTITY$

	ON ERROR GOTO 19000

10	LINPUT "GS Date (0293)"; GSDATE$
	GSNAME$ = "GS" + LEFT(GSDATE$, 2%) + "00." + RIGHT(GS, 3%)
	OPEN GSNAME$ FOR INPUT AS FILE 2%, &
		ORGANIZATION UNDEFINED, &
		RECORDTYPE NONE, &
		RECORDSIZE 512%


20	YYYY_PP$ = "19" + RIGHT(GSDATE$, 3%) + LEFT(GSDATE$,2%)

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.CRE"

100	!
	! Get 1st record
	!
	get #2%

	stop if recount <> 512%

	field #2%, 4% as x$

	rec_count% = cvt$%(x$)
	rec_len% = cvt$%(mid(x$,3%,1%))
	block_num% = 0%

	print "record count";rec_count%; "record length"; rec_len%


200	!
	! Scan through file
	!
	FOR I% = 1% TO REC_COUNT%

		character% = i% * rec_len%

		need_block% = character% / 512%

		if block_num% <> need_block%
		then
			get #2%
			block_num% = need_block%
		end if

		character% = character% and 511%

		field #2%, character% as junk$, &
			rec_len% as dta$

		gosub ProcessOne

	NEXT i%

	CLOSE 2%

	GOTO 32767

 ProcessOne:
400	!
	! Map out record
	!
	BFR$ = DTA$
	RETURN IF GASCRD.CUSNUM$ = "DDDDDD"

410	PP_MONTHLY::CUSNUM		= GASCRD.CUSNUM$
	PP_MONTHLY::CARD		= GASCRD.CARD$
	PP_MONTHLY::CTYPE		= GASCRD.TYPE$
	PP_MONTHLY::DESCRIPTION	= GASCRD.DESC$
	PP_MONTHLY::ODOMETER	= FNCVT(GASCUG.BEGODOM$)

	PUT #PP_MONTHLY.CH%

	RETURN

19000	!

	SELECT ERL
	CASE 400%
		RESUME 410 IF ERR = 155%
	CASE 410%
		RESUME 600 IF ERR = 134%
	END SELECT

	PRINT ERR,ERL
	on error goto 0

32767	end
