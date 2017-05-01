	!======================================================================
	! TV_SPOTS_FLAG file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_SPOTS_FLAG.CH%, STAT%)
	CALL READ_DEVICE('TV_SPOTS_FLAG',TV_SPOTS_FLAG.DEV$, STAT%)

	TV_SPOTS_FLAG.NAME$ = TV_SPOTS_FLAG.DEV$+"TV_SPOTS_FLAG.TBL"

	OPEN TV_SPOTS_FLAG.NAME$ FOR INPUT AS FILE TV_SPOTS_FLAG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_SPOTS_FLAG, &
		PRIMARY KEY &
		( &
			TV_SPOTS_FLAG::FLAG, &
			TV_SPOTS_FLAG::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

