	!======================================================================
	! PS_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PS_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('PS_CONTROL',PS_CONTROL.DEV$, STAT%)

	PS_CONTROL.NAME$ = PS_CONTROL.DEV$+"PS_CONTROL.CTR"

	OPEN PS_CONTROL.NAME$ FOR INPUT AS FILE PS_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP PS_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY
