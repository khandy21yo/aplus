	!======================================================================
	! AP_1099_YYYY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_1099_YYYY.CH%, STAT%)
	CALL READ_DEVICE('AP_1099_YYYY',AP_1099_YYYY.DEV$, STAT%)

	AP_1099_YYYY.NAME$ = AP_1099_YYYY.DEV$ + "AP_1099_" + YEAR_1099$ + ".HIS"

	OPEN AP_1099_YYYY.NAME$ FOR INPUT AS FILE AP_1099_YYYY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_1099_YYYY, &
		PRIMARY KEY &
		( &
			AP_1099_YYYY::VENNUM, &
			AP_1099_YYYY::CODE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

