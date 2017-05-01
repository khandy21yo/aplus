1	OPTION SIZE = (INTEGER LONG, REAL DOUBLE)

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD) PP_CARD_CDD PP_CARD

	MAP (CVT) &
		DOUBLE XVALUE
	MAP (CVT) &
		XVALUE$ = 8%


	DEF FNCVT(X$) = CVT$F(X$)

	MAP (DXA) &
		BFR$ = 256%
	MAP (DXA) &
		GASCUS.COCUSN$ = 6%, &
		GASCUS.SYCUSN$ = 6%, &
		GASCUS.DISCOUNT$ = 3%

	ON ERROR GOTO 19000

10	OPEN "gasCUS.daS" FOR INPUT AS FILE 2%, &
		ORGANIZATION UNDEFINED, &
		RECORDTYPE NONE, &
		RECORDSIZE 512%


20	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.CRE"

100	!
	! Get 1st record
	!
	get #2%

	stop if recount <> 512%

	field #2%, 4% as x$

	rec_count% = cvt$%(x$)
	rec_len% = cvt$%(mid(x$,3%,2%))
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

	GET #PP_CARD.CH%, KEY #0% EQ GASCUS.COCUSN$

410	WHILE PP_CARD::CUSNUM = GASCUS.COCUSN$

		PP_CARD::SYSCUS		= GASCUS.SYCUSN$
		PP_CARD::DISCOUNT	= GASCUS.DISCOUNT$

		UPDATE #PP_CARD.CH%

		GET #PP_CARD.CH%
	NEXT

500	RETURN

19000	!

	SELECT ERL
	CASE 400%
		RESUME 500 IF ERR = 155%
	CASE 410%
		RESUME 500
	END SELECT

	PRINT ERR,ERL
	on error goto 0

32767	end
