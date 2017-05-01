	!======================================================================
	! PR_EMP_DATES file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_DATES.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_DATES',PR_EMP_DATES.DEV$, STAT%)

	PR_EMP_DATES.NAME$ = PR_EMP_DATES.DEV$+"PR_EMP_DATES.TBL"

	OPEN PR_EMP_DATES.NAME$ FOR INPUT AS FILE PR_EMP_DATES.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_DATES, &
		PRIMARY KEY &
		( &
			PR_EMP_DATES::EMPLOYEE, &
			PR_EMP_DATES::DATECD, &
			PR_EMP_DATES::DATEBEGIN &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

