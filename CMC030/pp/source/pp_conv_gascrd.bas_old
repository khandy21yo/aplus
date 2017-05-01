1	OPTION SIZE = (INTEGER LONG, REAL DOUBLE)

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD) PP_CARD_CDD PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT) PP_CARDEXEMPT_CDD PP_CARDEXEMPT

	MAP (CVT) &
		DOUBLE XVALUE
	MAP (CVT) &
		XVALUE$ = 8%


	DEF FNCVT(X$) = CVT$F(X$)

	MAP (DXA) &
		BFR$ = 256%
	MAP (DXA) &
		GASCRD.CUSNUM$ = 6%, &
		GASCRD.CARD$ = 7%, &
		GASCRD.TYPE$ = 1%, &
		GASCRD.DESC$ = 30%, &
		GASCRD.STATE$(19%) = 21%, &
		GASCRD.BEGODOM$ = 8%

	ON ERROR GOTO 19000

10	OPEN "gasCRD.da1" FOR INPUT AS FILE 2%, &
		ORGANIZATION UNDEFINED, &
		RECORDTYPE NONE, &
		RECORDSIZE 512%


20	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.CRE"

30	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.CRE"

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

410	PP_CARD::CUSNUM		= GASCRD.CUSNUM$
	PP_CARD::CARD		= GASCRD.CARD$
	PP_CARD::CTYPE		= GASCRD.TYPE$
	PP_CARD::DESCRIPTION	= GASCRD.DESC$
	PP_CARD::ODOMETER	= FNCVT(GASCUG.BEGODOM$)

	PUT #PP_CARD.CH%

500	!
	! See if site product exists
	!
	PP_CARDEXEMPT::CUSNUM	= GASCRD.CUSNUM$
	PP_CARDEXEMPT::CARD	= GASCRD.CARD$

	FOR IX% = 0% TO 19%

510		GOTO 600 IF EDIT$(RIGHT(GASCRD.STATE$(IX%), 8%), -1%) = ""

		PP_CARDEXEMPT::STATE	= LEFT(GASCRD.STATE$(IX%), 2%)
		PP_CARDEXEMPT::AUTHORITY= MID(GASCRD.STATE$(IX%), 3%, 5%)

		TEMP$ = TRM$(RIGHT(GASCRD.STATE$(IX%), 8%)) + ","

		WHILE INSTR(1%, TEMP$, ",")

			J% = INSTR(1%, TEMP$, ",")
			PP_CARDEXEMPT::PRODUCT = LEFT(TEMP$, J% - 1%)
			TEMP$ = RIGHT(TEMP$, J% + 1%)

			PUT #PP_CARDEXEMPT.CH%
		NEXT

600	NEXT IX%

	RETURN

19000	!

	SELECT ERL
	CASE 400%
		RESUME 410 IF ERR = 155%
	CASE 410%
		RESUME 600 IF ERR = 134%
	CASE 500%
		RESUME 510 IF ERR = 155%
	CASE 510%
		RESUME 600 IF ERR = 134%
	END SELECT

	PRINT ERR,ERL
	on error goto 0

32767	end
