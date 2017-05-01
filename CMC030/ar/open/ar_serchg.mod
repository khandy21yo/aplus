	!======================================================================
	! AR_SERCHG file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_SERCHG.CH%, STAT%)
	CALL READ_DEVICE('AR_SERCHG',AR_SERCHG.DEV$, STAT%)

	AR_SERCHG.NAME$ = AR_SERCHG.DEV$+"AR_SERCHG.TBL"

	OPEN AR_SERCHG.NAME$ FOR INPUT AS FILE AR_SERCHG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_SERCHG, &
		PRIMARY KEY &
		( &
			AR_SERCHG::COUNTRY, &
			AR_SERCHG::STATE, &
			AR_SERCHG::ACCT &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

