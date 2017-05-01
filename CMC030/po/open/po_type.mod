	!======================================================================
	! PO_TYPE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_TYPE.CH%, STAT%)
	CALL READ_DEVICE('PO_TYPE',PO_TYPE.DEV$, STAT%)

	PO_TYPE.NAME$ = PO_TYPE.DEV$+"PO_TYPE.TBL"

	OPEN PO_TYPE.NAME$ FOR INPUT AS FILE PO_TYPE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_TYPE, &
		PRIMARY KEY &
			PO_TYPE::POTYPE, &
		ACCESS MODIFY, ALLOW MODIFY

