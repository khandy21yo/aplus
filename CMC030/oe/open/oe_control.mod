	!======================================================================
	! OE_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OE_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('OE_CONTROL',OE_CONTROL.DEV$, STAT%)

	OE_CONTROL.NAME$ = OE_CONTROL.DEV$+"OE_CONTROL.CTR"

	OPEN OE_CONTROL.NAME$ FOR INPUT AS FILE OE_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP OE_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

