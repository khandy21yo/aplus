	!======================================================================
	! IC_CYCLEJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_CYCLEJOUR.CH%, STAT%)
	CALL READ_DEVICE('IC_CYCLEJOUR',IC_CYCLEJOUR.DEV$, STAT%)

	IC_CYCLEJOUR.NAME$ = IC_CYCLEJOUR.DEV$+"IC_CYCLEJOUR_"+BATCH_NO$+".JRL"

	OPEN IC_CYCLEJOUR.NAME$ FOR INPUT AS FILE IC_CYCLEJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_CYCLEJOUR, &
		PRIMARY KEY &
			IC_CYCLEJOUR::LOCATION, &
		ACCESS MODIFY, ALLOW MODIFY

