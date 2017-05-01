	!======================================================================
	! AP_CLOSE_DIST file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CLOSE_DIST.CH%, STAT%)
	CALL READ_DEVICE('AP_CLOSE_DIST',AP_CLOSE_DIST.DEV$, STAT%)

	AP_CLOSE_DIST.NAME$ = AP_CLOSE_DIST.DEV$+"AP_CLOSE_DIST.LED"

	OPEN AP_CLOSE_DIST.NAME$ FOR INPUT AS FILE AP_CLOSE_DIST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CLOSE_DIST, &
		PRIMARY KEY &
		( &
			AP_CLOSE_DIST::TRANKEY, &
			AP_CLOSE_DIST::SLINE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

