	!======================================================================
	! TV_FILL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_FILL.CH%, STAT%)
	CALL READ_DEVICE('TV_FILL',TV_FILL.DEV$, STAT%)

	TV_FILL.NAME$ = TV_FILL.DEV$+"TV_FILL.MAS"

	OPEN TV_FILL.NAME$ FOR INPUT AS FILE TV_FILL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_FILL, &
		PRIMARY KEY &
			TV_FILL::FILNUM, &
		ACCESS MODIFY, ALLOW MODIFY

