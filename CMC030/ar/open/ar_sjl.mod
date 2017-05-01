	!======================================================================
	! AR_SJL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_SJL.CH%, STAT%)
	CALL READ_DEVICE('AR_SJL',AR_SJL.DEV$, STAT%)

	AR_SJL.NAME$ = AR_SJL.DEV$+"AR_SJL_"+BATCH_NO$+".JRL"

	OPEN AR_SJL.NAME$ FOR INPUT AS FILE AR_SJL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_SJL, &
		PRIMARY KEY &
		( &
			AR_SJL::INVNUM, &
			AR_SJL::SLINE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

