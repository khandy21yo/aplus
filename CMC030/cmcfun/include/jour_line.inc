	!
	! These records cannot be stuck into CDD because CDD will not
	! allow a RFA to be declared in it.
	!
	RECORD JOUR_LINE
		RFA	LINERFA
		REAL	TOTAL1
		REAL	TOTAL2
		REAL	TOTAL3
		REAL	TOTAL4
	END RECORD

	RECORD JOUR_LINE_HEADER
		LONG	CURRENT
		LONG	TOPLINE
		LONG	XCOUNT
	END RECORD

