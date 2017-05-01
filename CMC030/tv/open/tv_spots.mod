	!======================================================================
	! TV_SPOTS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_SPOTS.CH%, STAT%)
	CALL READ_DEVICE('TV_SPOTS',TV_SPOTS.DEV$, STAT%)

	TV_SPOTS.NAME$ = TV_SPOTS.DEV$+"TV_SPOTS_"+SCHED_DATE$+".LED"

	OPEN TV_SPOTS.NAME$ FOR INPUT AS FILE TV_SPOTS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_SPOTS, &
		ACCESS MODIFY, ALLOW MODIFY

