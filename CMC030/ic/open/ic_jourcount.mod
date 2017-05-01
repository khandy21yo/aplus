	!======================================================================
	! IC_JOURCOUNT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_JOURCOUNT.CH%, STAT%)
	CALL READ_DEVICE('IC_JOURCOUNT',IC_JOURCOUNT.DEV$, STAT%)

	IC_JOURCOUNT.NAME$ = IC_JOURCOUNT.DEV$+"IC_JOURCOUNT_"+BATCH_NO$+".JRL"

	OPEN IC_JOURCOUNT.NAME$ FOR INPUT AS FILE IC_JOURCOUNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_JOURCOUNT, &
		PRIMARY KEY &
		( &
			IC_JOURCOUNT::LOCATION, &
			IC_JOURCOUNT::PRODUCT &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

