	!======================================================================
	! UTL_COUNTRY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_COUNTRY.CH%, STAT%)
	CALL READ_DEVICE('UTL_COUNTRY',UTL_COUNTRY.DEV$, STAT%)

	UTL_COUNTRY.NAME$ = "CMC:UTL_COUNTRY.TBL"

	OPEN UTL_COUNTRY.NAME$ FOR INPUT AS FILE UTL_COUNTRY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_COUNTRY, &
		PRIMARY KEY &
			UTL_COUNTRY::COUNTRY, &
		ALTERNATE KEY &
			UTL_COUNTRY::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

