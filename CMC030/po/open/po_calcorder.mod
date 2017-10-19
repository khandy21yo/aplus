	!======================================================================
	! PO_CALCORDER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_CALCORDER.CH%, STAT%)
	CALL READ_DEVICE('PO_CALCORDER',PO_CALCORDER.DEV$, STAT%)

	PO_CALCORDER.NAME$ = PO_CALCORDER.DEV$+"PO_CALCORDER.JRL"

	OPEN PO_CALCORDER.NAME$ FOR INPUT AS FILE PO_CALCORDER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_CALCORDER, &
		PRIMARY KEY &
		( &
			PO_CALCORDER::LOCATION, &
			PO_CALCORDER::VENDOR, &
			PO_CALCORDER::PRODUCT &
		)	, &
		ALTERNATE KEY &
		( &
			PO_CALCORDER::PRODUCT, &
			PO_CALCORDER::LOCATION &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY
