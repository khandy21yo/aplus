	!======================================================================
	! AR_SALTAXLED file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_SALTAXLED.CH%, STAT%)
	CALL READ_DEVICE('AR_SALTAXLED',AR_SALTAXLED.DEV$, STAT%)

	AR_SALTAXLED.NAME$ = AR_SALTAXLED.DEV$+"AR_SALTAXLED.LED"

	OPEN AR_SALTAXLED.NAME$ FOR INPUT AS FILE AR_SALTAXLED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_SALTAXLED, &
		PRIMARY KEY &
		( &
			AR_SALTAXLED::TAXTYP, &
			AR_SALTAXLED::CUSNUM, &
			AR_SALTAXLED::INVNUM &
		)	DUPLICATES , &
		ALTERNATE KEY &
			AR_SALTAXLED::BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

