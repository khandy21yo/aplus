	!======================================================================
	! AD_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('AD_CONTROL',AD_CONTROL.DEV$, STAT%)

	AD_CONTROL.NAME$ = AD_CONTROL.DEV$+"AD_CONTROL.CTR"

	OPEN AD_CONTROL.NAME$ FOR INPUT AS FILE AD_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP AD_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

