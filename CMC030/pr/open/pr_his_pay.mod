	!======================================================================
	! PR_HIS_PAY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_HIS_PAY.CH%, STAT%)
	CALL READ_DEVICE('PR_HIS_PAY',PR_HIS_PAY.DEV$, STAT%)

	PR_HIS_PAY.NAME$ = PR_HIS_PAY.DEV$+"PR_HIS_PAY_"+BATCH_NO$+".ARC"

	OPEN PR_HIS_PAY.NAME$ FOR INPUT AS FILE PR_HIS_PAY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_HIS_PAY, &
		PRIMARY KEY &
		( &
			PR_HIS_PAY::EMPNUM, &
			PR_HIS_PAY::PR_END_DATE &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PR_HIS_PAY::SUBACC, &
			PR_HIS_PAY::OPER, &
			PR_HIS_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_HIS_PAY::LOCATION, &
			PR_HIS_PAY::DEPT, &
			PR_HIS_PAY::WORK_CENTER, &
			PR_HIS_PAY::EMPNUM, &
			PR_HIS_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

