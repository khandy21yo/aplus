	!======================================================================
	! AR_CUSTOM file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CUSTOM.CH%, STAT%)
	CALL READ_DEVICE('AR_CUSTOM',AR_CUSTOM.DEV$, STAT%)

	AR_CUSTOM.NAME$ = AR_CUSTOM.DEV$+"AR_CUSTOM.MAS"

	OPEN AR_CUSTOM.NAME$ FOR INPUT AS FILE AR_CUSTOM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CUSTOM, &
		PRIMARY KEY &
			AR_CUSTOM::CUSNUM, &
		ALTERNATE KEY &
			AR_CUSTOM::CUSNAM &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AR_CUSTOM::ALPSRT &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

