	!======================================================================
	! BI_BILLH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BI_BILLH.CH%, STAT%)
	CALL READ_DEVICE('BI_BILLH',BI_BILLH.DEV$, STAT%)

	BI_BILLH.NAME$ = BI_BILLH.DEV$+"BI_BILLH_"+BATCH_NO$+".JRL"

	OPEN BI_BILLH.NAME$ FOR INPUT AS FILE BI_BILLH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BI_BILLH, &
		PRIMARY KEY &
		( &
			BI_BILLH::PATIENT, &
			BI_BILLH::INSURED &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

