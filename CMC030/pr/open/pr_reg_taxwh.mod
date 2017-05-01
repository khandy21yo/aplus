	!======================================================================
	! PR_REG_TAXWH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_REG_TAXWH.CH%, STAT%)
	CALL READ_DEVICE('PR_REG_TAXWH',PR_REG_TAXWH.DEV$, STAT%)

	PR_REG_TAXWH.NAME$ = PR_REG_TAXWH.DEV$+"PR_REG_TAXWH_"+YYYY$+".LED"

	OPEN PR_REG_TAXWH.NAME$ FOR INPUT AS FILE PR_REG_TAXWH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_TAXWH, &
		PRIMARY KEY &
		( &
			PR_REG_TAXWH::EMPNUM, &
			PR_REG_TAXWH::TTYPE, &
			PR_REG_TAXWH::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

