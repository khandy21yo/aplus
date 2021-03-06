	!======================================================================
	! PO_REG_SUB_LINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_REG_SUB_LINE.CH%, STAT%)
	CALL READ_DEVICE('PO_REG_SUB_LINE',PO_REG_SUB_LINE.DEV$, STAT%)

	PO_REG_SUB_LINE.NAME$ = PO_REG_SUB_LINE.DEV$+"PO_REG_SUB_LINE.LED"

	OPEN PO_REG_SUB_LINE.NAME$ FOR INPUT AS FILE PO_REG_SUB_LINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_REG_SUB_LINE, &
		PRIMARY KEY &
		( &
			PO_REG_SUB_LINE::PO, &
			PO_REG_SUB_LINE::PO_LINE, &
			PO_REG_SUB_LINE::PO_ACTION &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PO_REG_SUB_LINE::BATCH, &
			PO_REG_SUB_LINE::PO, &
			PO_REG_SUB_LINE::PO_LINE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

