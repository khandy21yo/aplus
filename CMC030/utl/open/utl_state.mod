	!======================================================================
	! UTL_STATE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_STATE.CH%, STAT%)
	CALL READ_DEVICE('UTL_STATE',UTL_STATE.DEV$, STAT%)

	UTL_STATE.NAME$ = "CMC:UTL_STATE.TBL"

	OPEN UTL_STATE.NAME$ FOR INPUT AS FILE UTL_STATE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_STATE, &
		PRIMARY KEY &
		( &
			UTL_STATE::COUNTRY, &
			UTL_STATE::STATE &
		)	, &
		ALTERNATE KEY &
			UTL_STATE::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

