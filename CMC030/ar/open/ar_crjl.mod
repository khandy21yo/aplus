	!======================================================================
	! AR_CRJL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CRJL.CH%, STAT%)
	CALL READ_DEVICE('AR_CRJL',AR_CRJL.DEV$, STAT%)

	AR_CRJL.NAME$ = AR_CRJL.DEV$+"AR_CRJL_"+BATCH_NO$+".JRL"

	OPEN AR_CRJL.NAME$ FOR INPUT AS FILE AR_CRJL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CRJL, &
		PRIMARY KEY &
		( &
			AR_CRJL::RECNUM, &
			AR_CRJL::LLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

