	!======================================================================
	! TV_REP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_REP.CH%, STAT%)
	CALL READ_DEVICE('TV_REP',TV_REP.DEV$, STAT%)

	TV_REP.NAME$ = TV_REP.DEV$+"TV_REP.TBL"

	OPEN TV_REP.NAME$ FOR INPUT AS FILE TV_REP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_REP, &
		PRIMARY KEY &
			TV_REP::REP_NUM, &
		ALTERNATE KEY &
			TV_REP::RNAME &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			TV_REP::ALPSRT &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

