	!======================================================================
	! PO_NOTES file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_NOTES.CH%, STAT%)
	CALL READ_DEVICE('PO_NOTES',PO_NOTES.DEV$, STAT%)

	PO_NOTES.NAME$ = PO_NOTES.DEV$+"PO_NOTES.TBL"

	OPEN PO_NOTES.NAME$ FOR INPUT AS FILE PO_NOTES.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_NOTES, &
		PRIMARY KEY &
			PO_NOTES::NOTECODE, &
		ALTERNATE KEY &
			PO_NOTES::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

