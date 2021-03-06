	!======================================================================
	! PR_EMP_STATUS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_STATUS.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_STATUS',PR_EMP_STATUS.DEV$, STAT%)

	PR_EMP_STATUS.NAME$ = PR_EMP_STATUS.DEV$+"PR_EMP_STATUS.MAS"

	OPEN PR_EMP_STATUS.NAME$ FOR INPUT AS FILE PR_EMP_STATUS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_STATUS, &
		PRIMARY KEY &
		( &
			PR_EMP_STATUS::EMPNUM, &
			PR_EMP_STATUS::STTYPE, &
			PR_EMP_STATUS::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

