	!======================================================================
	! AR_CRJH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CRJH.CH%, STAT%)
	CALL READ_DEVICE('AR_CRJH',AR_CRJH.DEV$, STAT%)

	AR_CRJH.NAME$ = AR_CRJH.DEV$+"AR_CRJH_"+BATCH_NO$+".JRL"

	OPEN AR_CRJH.NAME$ FOR INPUT AS FILE AR_CRJH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CRJH, &
		PRIMARY KEY &
			AR_CRJH::RECNUM, &
		ACCESS MODIFY, ALLOW MODIFY

