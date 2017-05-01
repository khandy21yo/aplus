	!======================================================================
	! AP_CLOSE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CLOSE.CH%, STAT%)
	CALL READ_DEVICE('AP_CLOSE',AP_CLOSE.DEV$, STAT%)

	AP_CLOSE.NAME$ = AP_CLOSE.DEV$+"AP_CLOSE.LED"

	OPEN AP_CLOSE.NAME$ FOR INPUT AS FILE AP_CLOSE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CLOSE, &
		PRIMARY KEY &
		( &
			AP_CLOSE::VENNUM, &
			AP_CLOSE::TRANKEY &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			AP_CLOSE::VENNUM, &
			AP_CLOSE::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_CLOSE::BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

