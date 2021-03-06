	!======================================================================
	! PD_CATEGORY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PD_CATEGORY.CH%, STAT%)
	CALL READ_DEVICE('PD_CATEGORY',PD_CATEGORY.DEV$, STAT%)

	PD_CATEGORY.NAME$ = PD_CATEGORY.DEV$+"PD_CATEGORY.TBL"

	OPEN PD_CATEGORY.NAME$ FOR INPUT AS FILE PD_CATEGORY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PD_CATEGORY, &
		PRIMARY KEY &
			PD_CATEGORY::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

