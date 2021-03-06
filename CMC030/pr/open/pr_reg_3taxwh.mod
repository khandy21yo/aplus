	!======================================================================
	! PR_REG_3TAXWH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_REG_3TAXWH.CH%, STAT%)
	CALL READ_DEVICE('PR_REG_3TAXWH',PR_REG_3TAXWH.DEV$, STAT%)

	PR_REG_3TAXWH.NAME$ = PR_REG_TAXWH.DEV$+"PR_REG_3TAXWH_"+YYYY$+".LED"

	OPEN PR_REG_3TAXWH.NAME$ FOR INPUT AS FILE PR_REG_3TAXWH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_3TAXWH, &
		PRIMARY KEY &
		( &
			PR_REG_3TAXWH::EMPNUM, &
			PR_REG_3TAXWH::TTYPE, &
			PR_REG_3TAXWH::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

