	!======================================================================
	! AP_PJH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_PJH.CH%, STAT%)
	CALL READ_DEVICE('AP_PJH',AP_PJH.DEV$, STAT%)

	AP_PJH.NAME$ = AP_PJH.DEV$ + "AP_PJH_" + BATCH_NO$ + ".JRL"

	OPEN AP_PJH.NAME$ FOR INPUT AS FILE AP_PJH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_PJH, &
		PRIMARY KEY &
			AP_PJH::TRANKEY, &
		ACCESS MODIFY, ALLOW MODIFY

