	!======================================================================
	! AP_VENDOR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_VENDOR.CH%, STAT%)
	CALL READ_DEVICE('AP_VENDOR',AP_VENDOR.DEV$, STAT%)

	AP_VENDOR.NAME$ = AP_VENDOR.DEV$+"AP_VENDOR.MAS"

	OPEN AP_VENDOR.NAME$ FOR INPUT AS FILE AP_VENDOR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_VENDOR, &
		PRIMARY KEY &
			AP_VENDOR::VENNUM, &
		ALTERNATE KEY &
			AP_VENDOR::VENNAM &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ALPSRT &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::STATE &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ZIP &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY
