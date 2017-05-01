	!======================================================================
	! PR_TRN_CHECK file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TRN_CHECK.CH%, STAT%)
	CALL READ_DEVICE('PR_TRN_CHECK',PR_TRN_CHECK.DEV$, STAT%)

	PR_TRN_CHECK.NAME$ = PR_TRN_CHECK.DEV$+"PR_TRN_CHECK_"+BATCH_NO$+".JRL"

	OPEN PR_TRN_CHECK.NAME$ FOR INPUT AS FILE PR_TRN_CHECK.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_CHECK, &
		PRIMARY KEY &
		( &
			PR_TRN_CHECK::EMPNUM, &
			PR_TRN_CHECK::PR_END_DATE &
		)	, &
		ALTERNATE KEY &
		( &
			PR_TRN_CHECK::CHECK, &
			PR_TRN_CHECK::EMPNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

