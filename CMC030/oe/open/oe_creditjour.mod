	!======================================================================
	! OE_CREDITJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OE_CREDITJOUR.CH%, STAT%)
	CALL READ_DEVICE('OE_CREDITJOUR',OE_CREDITJOUR.DEV$, STAT%)

	OE_CREDITJOUR.NAME$ = OE_CREDITJOUR.DEV$+"OE_CREDITJOUR_"+BATCH_NO$+".JRL"

	OPEN OE_CREDITJOUR.NAME$ FOR INPUT AS FILE OE_CREDITJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_CREDITJOUR, &
		PRIMARY KEY &
			OE_CREDITJOUR::MEMONUM, &
		ALTERNATE KEY &
		( &
			OE_CREDITJOUR::CUSNUM, &
			OE_CREDITJOUR::MEMONUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_CREDITJOUR::REASON, &
			OE_CREDITJOUR::CUSNUM, &
			OE_CREDITJOUR::MEMONUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

