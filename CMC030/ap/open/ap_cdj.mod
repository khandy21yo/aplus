	!======================================================================
	! AP_CDJ file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CDJ.CH%, STAT%)
	CALL READ_DEVICE('AP_CDJ',AP_CDJ.DEV$, STAT%)

	AP_CDJ.NAME$ = AP_CDJ.DEV$+"AP_CDJ_"+CDJ_BATCH$+".JRL"

	OPEN AP_CDJ.NAME$ FOR INPUT AS FILE AP_CDJ.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CDJ, &
		PRIMARY KEY &
		( &
			AP_CDJ::VENNUM, &
			AP_CDJ::TRANKEY &
		)	, &
		ALTERNATE KEY &
		( &
			AP_CDJ::CKNUM, &
			AP_CDJ::VENNUM, &
			AP_CDJ::TRANKEY &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

