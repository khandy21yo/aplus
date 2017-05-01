	!======================================================================
	! TV_COMMER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_COMMER.CH%, STAT%)
	CALL READ_DEVICE('TV_COMMER',TV_COMMER.DEV$, STAT%)

	TV_COMMER.NAME$ = TV_COMMER.DEV$+"TV_COMMER.LED"

	OPEN TV_COMMER.NAME$ FOR INPUT AS FILE TV_COMMER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_COMMER, &
		PRIMARY KEY &
			TV_COMMER::FRMNUM, &
		ALTERNATE KEY &
		( &
			TV_COMMER::CUSNUM, &
			TV_COMMER::FRMNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

