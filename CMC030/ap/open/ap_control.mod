	!======================================================================
	! AP_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('AP_CONTROL',AP_CONTROL.DEV$, STAT%)

	AP_CONTROL.NAME$ = AP_CONTROL.DEV$ + "AP_CONTROL.CTR"

	OPEN AP_CONTROL.NAME$ FOR INPUT AS FILE AP_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP AP_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

