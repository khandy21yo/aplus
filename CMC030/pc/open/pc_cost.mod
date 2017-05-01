	!======================================================================
	! PC_COST file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PC_COST.CH%, STAT%)
	CALL READ_DEVICE('PC_COST',PC_COST.DEV$, STAT%)

	PC_COST.NAME$ = PC_COST.DEV$+"PC_COST.MAS"

	OPEN PC_COST.NAME$ FOR INPUT AS FILE PC_COST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PC_COST, &
		PRIMARY KEY &
		( &
			PC_COST::PRODUCT, &
			PC_COST::LOCATION, &
			PC_COST::EFFDATE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

