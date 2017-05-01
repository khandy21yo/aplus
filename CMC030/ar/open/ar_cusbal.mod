	!======================================================================
	! AR_CUSBAL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CUSBAL.CH%, STAT%)
	CALL READ_DEVICE('AR_CUSBAL',AR_CUSBAL.DEV$, STAT%)

	AR_CUSBAL.NAME$ = AR_CUSBAL.DEV$+"AR_CUSBAL.MAS"

	OPEN AR_CUSBAL.NAME$ FOR INPUT AS FILE AR_CUSBAL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CUSBAL, &
		PRIMARY KEY &
		( &
			AR_CUSBAL::CUSNUM, &
			AR_CUSBAL::ACCT &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

