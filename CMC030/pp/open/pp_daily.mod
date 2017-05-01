	!======================================================================
	! PP_DAILY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PP_DAILY.CH%, STAT%)
	CALL READ_DEVICE('PP_DAILY',PP_DAILY.DEV$, STAT%)

	PP_DAILY.NAME$ = PP_DAILY.DEV$+"PP_DAILY_"+BATCH_NO$+".JRL"

	OPEN PP_DAILY.NAME$ FOR INPUT AS FILE PP_DAILY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_DAILY, &
		PRIMARY KEY &
		( &
			PP_DAILY::CUSNUM, &
			PP_DAILY::VEHICLE, &
			PP_DAILY::TRANDATE, &
			PP_DAILY::TRANTIME &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PP_DAILY::HOST, &
			PP_DAILY::SITE, &
			PP_DAILY::STYPE, &
			PP_DAILY::TRANDATE, &
			PP_DAILY::TRANTIME &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

