	!======================================================================
	! AP_PJL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_PJL.CH%, STAT%)
	CALL READ_DEVICE('AP_PJL',AP_PJL.DEV$, STAT%)

	AP_PJL.NAME$ = AP_PJL.DEV$ + "AP_PJL_" + BATCH_NO$ + ".JRL"

	OPEN AP_PJL.NAME$ FOR INPUT AS FILE AP_PJL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_PJL, &
		PRIMARY KEY &
		( &
			AP_PJL::TRANKEY, &
			AP_PJL::SLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

