	!======================================================================
	! BC_BILLH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BC_BILLH.CH%, STAT%)
	CALL READ_DEVICE('BC_BILLH',BC_BILLH.DEV$, STAT%)

	BC_BILLH.NAME$ = BC_BILLH.DEV$+"BC_BILLH_"+BATCH_NO$+".JRL"

	OPEN BC_BILLH.NAME$ FOR INPUT AS FILE BC_BILLH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BC_BILLH, &
		PRIMARY KEY &
			BC_BILLH::ORDER, &
		ACCESS MODIFY, ALLOW MODIFY

