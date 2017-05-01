	!======================================================================
	! TV_BREAK_TYPE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_BREAK_TYPE.CH%, STAT%)
	CALL READ_DEVICE('TV_BREAK_TYPE',TV_BREAK_TYPE.DEV$, STAT%)

	TV_BREAK_TYPE.NAME$ = TV_BREAK_TYPE.DEV$+"TV_BREAK_TYPE.TBL"

	OPEN TV_BREAK_TYPE.NAME$ FOR INPUT AS FILE TV_BREAK_TYPE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_BREAK_TYPE, &
		PRIMARY KEY &
			TV_BREAK_TYPE::BTYPE, &
		ACCESS MODIFY, ALLOW MODIFY

