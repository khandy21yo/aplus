	!======================================================================
	! OE_INVLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OE_INVLINE.CH%, STAT%)
	CALL READ_DEVICE('OE_INVLINE',OE_INVLINE.DEV$, STAT%)

	OE_INVLINE.NAME$ = OE_INVLINE.DEV$+"OE_INVLINE_"+BATCH_NO$+".JRL"

	OPEN OE_INVLINE.NAME$ FOR INPUT AS FILE OE_INVLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_INVLINE, &
		PRIMARY KEY &
		( &
			OE_INVLINE::ORDNUM, &
			OE_INVLINE::LIN &
		)	, &
		ALTERNATE KEY &
		( &
			OE_INVLINE::LIN, &
			OE_INVLINE::ORDNUM &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

