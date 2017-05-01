	!======================================================================
	! PC_PRCTYPE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PC_PRCTYPE.CH%, STAT%)
	CALL READ_DEVICE('PC_PRCTYPE',PC_PRCTYPE.DEV$, STAT%)

	PC_PRCTYPE.NAME$ = PC_PRCTYPE.DEV$+"PC_PRCTYPE.TBL"

	OPEN PC_PRCTYPE.NAME$ FOR INPUT AS FILE PC_PRCTYPE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PC_PRCTYPE, &
		PRIMARY KEY &
			PC_PRCTYPE::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

