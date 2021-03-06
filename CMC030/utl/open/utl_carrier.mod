	!======================================================================
	! UTL_CARRIER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_CARRIER.CH%, STAT%)
	CALL READ_DEVICE('UTL_CARRIER',UTL_CARRIER.DEV$, STAT%)

	UTL_CARRIER.NAME$ = UTL_CARRIER.DEV$+"UTL_CARRIER.TBL"

	OPEN UTL_CARRIER.NAME$ FOR INPUT AS FILE UTL_CARRIER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_CARRIER, &
		PRIMARY KEY &
			UTL_CARRIER::CODE, &
		ALTERNATE KEY &
			UTL_CARRIER::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

