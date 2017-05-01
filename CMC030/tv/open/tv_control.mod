	!======================================================================
	! TV_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('TV_CONTROL',TV_CONTROL.DEV$, STAT%)

	TV_CONTROL.NAME$ = TV_CONTROL.DEV$+"TV_CONTROL.CTR"

	OPEN TV_CONTROL.NAME$ FOR INPUT AS FILE TV_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP TV_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

