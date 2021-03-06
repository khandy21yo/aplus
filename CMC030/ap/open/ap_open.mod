	!======================================================================
	! AP_OPEN file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_OPEN.CH%, STAT%)
	CALL READ_DEVICE('AP_OPEN',AP_OPEN.DEV$, STAT%)

	AP_OPEN.NAME$ = AP_OPEN.DEV$+"AP_OPEN.LED"

	OPEN AP_OPEN.NAME$ FOR INPUT AS FILE AP_OPEN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN, &
		PRIMARY KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::TRANKEY &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_OPEN::BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

