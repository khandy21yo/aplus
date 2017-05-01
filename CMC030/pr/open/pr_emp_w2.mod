	!======================================================================
	! PR_EMP_W2 file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_W2.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_W2',PR_EMP_W2.DEV$, STAT%)

	PR_EMP_W2.NAME$ = PR_EMP_W2.DEV$+"PR_EMP_W2.MAS"

	OPEN PR_EMP_W2.NAME$ FOR INPUT AS FILE PR_EMP_W2.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_W2, &
		PRIMARY KEY &
			PR_EMP_W2::EMPNUM, &
		ALTERNATE KEY &
			PR_EMP_W2::SSN &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_EMP_W2::LOCATION, &
			PR_EMP_W2::SSN &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

