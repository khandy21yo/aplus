	!======================================================================
	! AR_SALTAX file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_SALTAX.CH%, STAT%)
	CALL READ_DEVICE('AR_SALTAX',AR_SALTAX.DEV$, STAT%)

	AR_SALTAX.NAME$ = AR_SALTAX.DEV$+"AR_SALTAX.TBL"

	OPEN AR_SALTAX.NAME$ FOR INPUT AS FILE AR_SALTAX.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_SALTAX, &
		PRIMARY KEY &
		( &
			AR_SALTAX::COUNTRY, &
			AR_SALTAX::STATE, &
			AR_SALTAX::COUNTY &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

