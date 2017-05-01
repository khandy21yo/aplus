	!======================================================================
	! PR_EMP_ACCRUAL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_ACCRUAL.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_ACCRUAL',PR_EMP_ACCRUAL.DEV$, STAT%)

	PR_EMP_ACCRUAL.NAME$ = PR_EMP_ACCRUAL.DEV$+"PR_EMP_ACCRUAL.MAS"

	OPEN PR_EMP_ACCRUAL.NAME$ FOR INPUT AS FILE PR_EMP_ACCRUAL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_ACCRUAL, &
		PRIMARY KEY &
		( &
			PR_EMP_ACCRUAL::EMPNUM, &
			PR_EMP_ACCRUAL::ATYPE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

