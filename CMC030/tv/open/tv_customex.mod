	!======================================================================
	! TV_CUSTOMEX file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_CUSTOMEX.CH%, STAT%)
	CALL READ_DEVICE('TV_CUSTOMEX',TV_CUSTOMEX.DEV$, STAT%)

	TV_CUSTOMEX.NAME$ = TV_CUSTOMEX.DEV$+"TV_CUSTOMEX.MAS"

	OPEN TV_CUSTOMEX.NAME$ FOR INPUT AS FILE TV_CUSTOMEX.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_CUSTOMEX, &
		PRIMARY KEY &
			TV_CUSTOMEX::CUSNUM, &
		ACCESS MODIFY, ALLOW MODIFY

