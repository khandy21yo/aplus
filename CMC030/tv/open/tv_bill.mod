	!======================================================================
	! TV_BILL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_BILL.CH%, STAT%)
	CALL READ_DEVICE('TV_BILL',TV_BILL.DEV$, STAT%)

	TV_BILL.NAME$ = TV_BILL.DEV$+"TV_BILL.LED"

	OPEN TV_BILL.NAME$ FOR INPUT AS FILE TV_BILL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_BILL, &
		PRIMARY KEY &
		( &
			TV_BILL::CUSNUM, &
			TV_BILL::FRMNUM, &
			TV_BILL::SKEDNUM &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			TV_BILL::FRMNUM, &
			TV_BILL::SKEDNUM, &
			TV_BILL::SCH_DATE, &
			TV_BILL::SCH_TIME &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			TV_BILL::GL_BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

