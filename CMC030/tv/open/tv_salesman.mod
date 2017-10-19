	!======================================================================
	! TV_SALESMAN file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_SALESMAN.CH%, STAT%)
	CALL READ_DEVICE('TV_SALESMAN',TV_SALESMAN.DEV$, STAT%)

	TV_SALESMAN.NAME$ = TV_SALESMAN.DEV$+"TV_SALESMAN.TBL"

	OPEN TV_SALESMAN.NAME$ FOR INPUT AS FILE TV_SALESMAN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_SALESMAN, &
		PRIMARY KEY &
			TV_SALESMAN::SALNUM, &
		ALTERNATE KEY &
			TV_SALESMAN::SNAME &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			TV_SALESMAN::ALPSRT &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY
