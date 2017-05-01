	!======================================================================
	! TV_COMMER_HEADER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_COMMER_HEADER.CH%, STAT%)
	CALL READ_DEVICE('TV_COMMER_HEADER',TV_COMMER_HEADER.DEV$, STAT%)

	TV_COMMER_HEADER.NAME$ = TV_COMMER_HEADER.DEV$+"TV_COMMER_HEADER.LED"

	OPEN TV_COMMER_HEADER.NAME$ FOR INPUT AS FILE TV_COMMER_HEADER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_COMMER_HEADER, &
		ACCESS MODIFY, ALLOW MODIFY

