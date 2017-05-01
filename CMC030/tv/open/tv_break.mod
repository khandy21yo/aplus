	!======================================================================
	! TV_BREAK file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_BREAK.CH%, STAT%)
	CALL READ_DEVICE('TV_BREAK',TV_BREAK.DEV$, STAT%)

	TV_BREAK.NAME$ = TV_BREAK.DEV$+"TV_BREAK.MAS"

	OPEN TV_BREAK.NAME$ FOR INPUT AS FILE TV_BREAK.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_BREAK, &
		PRIMARY KEY &
		( &
			TV_BREAK::PRGNUM, &
			TV_BREAK::RUN_TIME &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

