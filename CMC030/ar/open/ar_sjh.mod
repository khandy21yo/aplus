	!======================================================================
	! AR_SJH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_SJH.CH%, STAT%)
	CALL READ_DEVICE('AR_SJH',AR_SJH.DEV$, STAT%)

	AR_SJH.NAME$ = AR_SJH.DEV$+"AR_SJH_"+BATCH_NO$+".JRL"

	OPEN AR_SJH.NAME$ FOR INPUT AS FILE AR_SJH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_SJH, &
		PRIMARY KEY &
			AR_SJH::INVNUM, &
		ACCESS MODIFY, ALLOW MODIFY

