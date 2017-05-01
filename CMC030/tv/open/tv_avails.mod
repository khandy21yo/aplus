	!======================================================================
	! TV_AVAILS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_AVAILS.CH%, STAT%)
	CALL READ_DEVICE('TV_AVAILS',TV_AVAILS.DEV$, STAT%)

	TV_AVAILS.NAME$ = TV_AVAILS.DEV$+"TV_AVAILS.TBL"

	OPEN TV_AVAILS.NAME$ FOR INPUT AS FILE TV_AVAILS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_AVAILS, &
		ACCESS MODIFY, ALLOW MODIFY

