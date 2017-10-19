	!======================================================================
	! PR_WC_DEFINITION file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_WC_DEFINITION.CH%, STAT%)
	CALL READ_DEVICE('PR_WC_DEFINITION',PR_WC_DEFINITION.DEV$, STAT%)

	PR_WC_DEFINITION.NAME$ = PR_WC_DEFINITION.DEV$+"PR_WC_DEFINITION.TBL"

	OPEN PR_WC_DEFINITION.NAME$ FOR INPUT AS FILE PR_WC_DEFINITION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_WC_DEFINITION, &
		PRIMARY KEY &
			PR_WC_DEFINITION::CODE &
			DUPLICATES , &
		ALTERNATE KEY &
		( &
			PR_WC_DEFINITION::SUBJ_ACCT, &
			PR_WC_DEFINITION::OPER &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY
