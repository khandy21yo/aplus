	!======================================================================
	! SA_SALEGROUP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(SA_SALEGROUP.CH%, STAT%)
	CALL READ_DEVICE('SA_SALEGROUP',SA_SALEGROUP.DEV$, STAT%)

	SA_SALEGROUP.NAME$ = SA_SALEGROUP.DEV$+"SA_SALEGROUP.MAS"

	OPEN SA_SALEGROUP.NAME$ FOR INPUT AS FILE SA_SALEGROUP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP SA_SALEGROUP, &
		PRIMARY KEY &
		( &
			SA_SALEGROUP::SALGROUP, &
			SA_SALEGROUP::SALESMAN &
		)	, &
		ALTERNATE KEY &
		( &
			SA_SALEGROUP::SALESMAN, &
			SA_SALEGROUP::SALGROUP &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

