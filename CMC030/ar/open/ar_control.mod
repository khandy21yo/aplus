	!======================================================================
	! AR_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('AR_CONTROL',AR_CONTROL.DEV$, STAT%)

	AR_CONTROL.NAME$ = AR_CONTROL.DEV$+"AR_CONTROL.CTR"

	OPEN AR_CONTROL.NAME$ FOR INPUT AS FILE AR_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP AR_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

