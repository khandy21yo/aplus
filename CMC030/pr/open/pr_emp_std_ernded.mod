	!======================================================================
	! PR_EMP_STD_ERNDED file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_STD_ERNDED.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_STD_ERNDED',PR_EMP_STD_ERNDED.DEV$, STAT%)

	PR_EMP_STD_ERNDED.NAME$ = PR_EMP_STD_ERNDED.DEV$+"PR_EMP_STD_ERNDED.MAS"

	OPEN PR_EMP_STD_ERNDED.NAME$ FOR INPUT AS FILE PR_EMP_STD_ERNDED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_STD_ERNDED, &
		PRIMARY KEY &
		( &
			PR_EMP_STD_ERNDED::EMPNUM, &
			PR_EMP_STD_ERNDED::RTYPE, &
			PR_EMP_STD_ERNDED::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

