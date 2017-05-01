	!======================================================================
	! PR_REG_ERNDED file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_REG_ERNDED.CH%, STAT%)
	CALL READ_DEVICE('PR_REG_ERNDED',PR_REG_ERNDED.DEV$, STAT%)

	PR_REG_ERNDED.NAME$ = PR_REG_ERNDED.DEV$+"PR_REG_ERNDED_"+YYYY$+".LED"

	OPEN PR_REG_ERNDED.NAME$ FOR INPUT AS FILE PR_REG_ERNDED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_ERNDED, &
		PRIMARY KEY &
		( &
			PR_REG_ERNDED::EMPNUM, &
			PR_REG_ERNDED::ETYPE, &
			PR_REG_ERNDED::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

