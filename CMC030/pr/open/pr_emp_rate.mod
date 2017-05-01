	!======================================================================
	! PR_EMP_RATE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_EMP_RATE.CH%, STAT%)
	CALL READ_DEVICE('PR_EMP_RATE',PR_EMP_RATE.DEV$, STAT%)

	PR_EMP_RATE.NAME$ = PR_EMP_RATE.DEV$+"PR_EMP_RATE.MAS"

	OPEN PR_EMP_RATE.NAME$ FOR INPUT AS FILE PR_EMP_RATE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_RATE, &
		PRIMARY KEY &
		( &
			PR_EMP_RATE::EMPNUM, &
			PR_EMP_RATE::OPER, &
			PR_EMP_RATE::EFFDAT &
		)	, &
		ALTERNATE KEY &
		( &
			PR_EMP_RATE::EVAL_DATE, &
			PR_EMP_RATE::EMPNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

