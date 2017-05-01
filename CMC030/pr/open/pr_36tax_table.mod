	!======================================================================
	! PR_36TAX_TABLE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_36TAX_TABLE.CH%, STAT%)
	CALL READ_DEVICE('PR_36TAX_TABLE',PR_36TAX_TABLE.DEV$, STAT%)

	PR_36TAX_TABLE.NAME$ = PR_36TAX_TABLE.DEV$+"PR_36TAX_TABLE_" + YYYY$ + ".TBL"

	OPEN PR_36TAX_TABLE.NAME$ FOR INPUT AS FILE PR_36TAX_TABLE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_36TAX_TABLE, &
		PRIMARY KEY &
		( &
			PR_36TAX_TABLE::AUTH, &
			PR_36TAX_TABLE::CODE, &
			PR_36TAX_TABLE::TSTATUS &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

