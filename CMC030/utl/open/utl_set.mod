	!======================================================================
	! UTL_SET file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_SET.CH%, STAT%)
	CALL READ_DEVICE('UTL_SET',UTL_SET.DEV$, STAT%)

	UTL_SET.NAME$ = UTL_SET.DEV$+"UTL_SET.IDX"

	OPEN UTL_SET.NAME$ FOR INPUT AS FILE UTL_SET.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_SET, &
		PRIMARY KEY &
		( &
			UTL_SET::PROGRAMNAME, &
			UTL_SET::ITEM &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

