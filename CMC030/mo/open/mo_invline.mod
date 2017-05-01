	!======================================================================
	! MO_INVLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(MO_INVLINE.CH%, STAT%)
	CALL READ_DEVICE('MO_INVLINE',MO_INVLINE.DEV$, STAT%)

	MO_INVLINE.NAME$ = MO_INVLINE.DEV$+"MO_INVLINE_"+BATCH_NO$+".JRL"

	OPEN MO_INVLINE.NAME$ FOR INPUT AS FILE MO_INVLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP MO_INVLINE, &
		PRIMARY KEY &
		( &
			MO_INVLINE::ORDNUM, &
			MO_INVLINE::OLINE &
		)	, &
		ALTERNATE KEY &
		( &
			MO_INVLINE::OLINE, &
			MO_INVLINE::ORDNUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

