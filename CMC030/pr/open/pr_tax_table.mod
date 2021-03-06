	!======================================================================
	! PR_TAX_TABLE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TAX_TABLE.CH%, STAT%)
	CALL READ_DEVICE('PR_TAX_TABLE',PR_TAX_TABLE.DEV$, STAT%)

	PR_TAX_TABLE.NAME$ = PR_TAX_TABLE.DEV$+"PR_TAX_TABLE_"+YYYY$+".TBL"

	OPEN PR_TAX_TABLE.NAME$ FOR INPUT AS FILE PR_TAX_TABLE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TAX_TABLE, &
		PRIMARY KEY &
		( &
			PR_TAX_TABLE::AUTH, &
			PR_TAX_TABLE::CODE, &
			PR_TAX_TABLE::TSTATUS &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

