	!======================================================================
	! BC_BILLL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BC_BILLL.CH%, STAT%)
	CALL READ_DEVICE('BC_BILLL',BC_BILLL.DEV$, STAT%)

	BC_BILLL.NAME$ = BC_BILLL.DEV$+"BC_BILLL_"+BATCH_NO$+".JRL"

	OPEN BC_BILLL.NAME$ FOR INPUT AS FILE BC_BILLL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BC_BILLL, &
		PRIMARY KEY &
		( &
			BC_BILLL::ORDER, &
			BC_BILLL::LINENO &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

