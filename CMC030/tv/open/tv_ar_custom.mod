	!======================================================================
	! TV_AR_CUSTOM file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_AR_CUSTOM.CH%, STAT%)
	CALL READ_DEVICE('TV_AR_CUSTOM',TV_AR_CUSTOM.DEV$, STAT%)

	TV_AR_CUSTOM.NAME$ = TV_AR_CUSTOM.DEV$+"TV_AR_CUSTOM.MAS"

	OPEN TV_AR_CUSTOM.NAME$ FOR INPUT AS FILE TV_AR_CUSTOM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_AR_CUSTOM, &
		ACCESS MODIFY, ALLOW MODIFY

