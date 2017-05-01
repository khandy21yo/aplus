	!======================================================================
	! PR_TRN_PAY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TRN_PAY.CH%, STAT%)
	CALL READ_DEVICE('PR_TRN_PAY',PR_TRN_PAY.DEV$, STAT%)

	PR_TRN_PAY.NAME$ = PR_TRN_PAY.DEV$+"PR_TRN_PAY_"+BATCH_NO$+".JRL"

	OPEN PR_TRN_PAY.NAME$ FOR INPUT AS FILE PR_TRN_PAY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_PAY, &
		PRIMARY KEY &
		( &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::SUBACC, &
			PR_TRN_PAY::OPER, &
			PR_TRN_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::LOCATION, &
			PR_TRN_PAY::DEPT, &
			PR_TRN_PAY::WORK_CENTER, &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

