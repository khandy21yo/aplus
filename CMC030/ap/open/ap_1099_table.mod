	!======================================================================
	! AP_1099_TABLE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_1099_TABLE.CH%, STAT%)
	CALL READ_DEVICE('AP_1099_TABLE',AP_1099_TABLE.DEV$, STAT%)

	AP_1099_TABLE.NAME$ = AP_1099_TABLE.DEV$ + "AP_1099_TABLE.TBL"

	OPEN AP_1099_TABLE.NAME$ FOR INPUT AS FILE AP_1099_TABLE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_1099_TABLE, &
		PRIMARY KEY &
			AP_1099_TABLE::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

