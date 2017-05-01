	!======================================================================
	! BI_BILLL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BI_BILLL.CH%, STAT%)
	CALL READ_DEVICE('BI_BILLL',BI_BILLL.DEV$, STAT%)

	BI_BILLL.NAME$ = BI_BILLL.DEV$+"BI_BILLL_"+BATCH_NO$+".JRL"

	OPEN BI_BILLL.NAME$ FOR INPUT AS FILE BI_BILLL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BI_BILLL, &
		PRIMARY KEY &
		( &
			BI_BILLL::PATIENT, &
			BI_BILLL::INSURED, &
			BI_BILLL::SERVDATE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

