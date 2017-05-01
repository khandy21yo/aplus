	!======================================================================
	! PO_REG_LINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_REG_LINE.CH%, STAT%)
	CALL READ_DEVICE('PO_REG_LINE',PO_REG_LINE.DEV$, STAT%)

	PO_REG_LINE.NAME$ = PO_REG_LINE.DEV$+"PO_REG_LINE.LED"

	OPEN PO_REG_LINE.NAME$ FOR INPUT AS FILE PO_REG_LINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_REG_LINE, &
		PRIMARY KEY &
		( &
			PO_REG_LINE::PO, &
			PO_REG_LINE::PO_LINE &
		)	, &
		ALTERNATE KEY &
		( &
			PO_REG_LINE::PO_TYPE, &
			PO_REG_LINE::PO, &
			PO_REG_LINE::PO_LINE &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			PO_REG_LINE::VENDOR, &
			PO_REG_LINE::PO, &
			PO_REG_LINE::PO_LINE &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			PO_REG_LINE::BATCH, &
			PO_REG_LINE::PO, &
			PO_REG_LINE::PO_LINE &
		)	, &
		ALTERNATE KEY &
		( &
			PO_REG_LINE::PRODUCT, &
			PO_REG_LINE::PO, &
			PO_REG_LINE::PO_LINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

