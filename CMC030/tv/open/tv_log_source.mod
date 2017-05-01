	!======================================================================
	! TV_LOG_SOURCE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_LOG_SOURCE.CH%, STAT%)
	CALL READ_DEVICE('TV_LOG_SOURCE',TV_LOG_SOURCE.DEV$, STAT%)

	TV_LOG_SOURCE.NAME$ = TV_LOG_SOURCE.DEV$+"TV_LOG_SOURCE.TBL"

	OPEN TV_LOG_SOURCE.NAME$ FOR INPUT AS FILE TV_LOG_SOURCE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_LOG_SOURCE, &
		PRIMARY KEY &
			TV_LOG_SOURCE::SOURCE, &
		ACCESS MODIFY, ALLOW MODIFY

