	!======================================================================
	! AR_OPEN file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPEN.CH%, STAT%)
	CALL READ_DEVICE('AR_OPEN',AR_OPEN.DEV$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$+"AR_OPEN.LED"

	OPEN AR_OPEN.NAME$ FOR INPUT AS FILE AR_OPEN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_OPEN, &
		PRIMARY KEY &
		( &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			AR_OPEN::BATCH, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::SALNUM, &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::INVNUM, &
			AR_OPEN::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

