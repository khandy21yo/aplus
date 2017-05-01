	!======================================================================
	! TV_LOG_TYPE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_LOG_TYPE.CH%, STAT%)
	CALL READ_DEVICE('TV_LOG_TYPE',TV_LOG_TYPE.DEV$, STAT%)

	TV_LOG_TYPE.NAME$ = TV_LOG_TYPE.DEV$+"TV_LOG_TYPE.TBL"

	OPEN TV_LOG_TYPE.NAME$ FOR INPUT AS FILE TV_LOG_TYPE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_LOG_TYPE, &
		PRIMARY KEY &
			TV_LOG_TYPE::LTYPE, &
		ACCESS MODIFY, ALLOW MODIFY

