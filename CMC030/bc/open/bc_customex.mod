	!======================================================================
	! BC_CUSTOMEX file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BC_CUSTOMEX.CH%, STAT%)
	CALL READ_DEVICE('BC_CUSTOMEX',BC_CUSTOMEX.DEV$, STAT%)

	BC_CUSTOMEX.NAME$ = BC_CUSTOMEX.DEV$+"BC_CUSTOMEX.TBL"

	OPEN BC_CUSTOMEX.NAME$ FOR INPUT AS FILE BC_CUSTOMEX.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BC_CUSTOMEX, &
		PRIMARY KEY &
			BC_CUSTOMEX::CUSNUM, &
		ACCESS MODIFY, ALLOW MODIFY

