	!======================================================================
	! UT_TERMS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UT_TERMS.CH%, STAT%)
	CALL READ_DEVICE('UT_TERMS',UT_TERMS.DEV$, STAT%)

	UT_TERMS.NAME$ = UT_TERMS.DEV$+"UT_TERMS.TBL"

	OPEN UT_TERMS.NAME$ FOR INPUT AS FILE UT_TERMS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UT_TERMS, &
		PRIMARY KEY &
			UT_TERMS::CODE, &
		ALTERNATE KEY &
			UT_TERMS::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

