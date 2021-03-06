	!======================================================================
	! PO_CATEGORY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_CATEGORY.CH%, STAT%)
	CALL READ_DEVICE('PO_CATEGORY',PO_CATEGORY.DEV$, STAT%)

	PO_CATEGORY.NAME$ = PO_CATEGORY.DEV$+"PO_CATEGORY.TBL"

	OPEN PO_CATEGORY.NAME$ FOR INPUT AS FILE PO_CATEGORY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_CATEGORY, &
		PRIMARY KEY &
			PO_CATEGORY::CODE, &
		ALTERNATE KEY &
			PO_CATEGORY::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

