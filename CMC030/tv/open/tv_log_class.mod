	!======================================================================
	! TV_LOG_CLASS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_LOG_CLASS.CH%, STAT%)
	CALL READ_DEVICE('TV_LOG_CLASS',TV_LOG_CLASS.DEV$, STAT%)

	TV_LOG_CLASS.NAME$ = TV_LOG_CLASS.DEV$+"TV_LOG_CLASS.TBL"

	OPEN TV_LOG_CLASS.NAME$ FOR INPUT AS FILE TV_LOG_CLASS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_LOG_CLASS, &
		PRIMARY KEY &
			TV_LOG_CLASS::CLASS, &
		ACCESS MODIFY, ALLOW MODIFY

