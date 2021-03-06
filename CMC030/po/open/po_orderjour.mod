	!======================================================================
	! PO_ORDERJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_ORDERJOUR.CH%, STAT%)
	CALL READ_DEVICE('PO_ORDERJOUR',PO_ORDERJOUR.DEV$, STAT%)

	PO_ORDERJOUR.NAME$ = PO_ORDERJOUR.DEV$+"PO_ORDERJOUR_"+BATCH_NO$+".JRL"

	OPEN PO_ORDERJOUR.NAME$ FOR INPUT AS FILE PO_ORDERJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_ORDERJOUR, &
		PRIMARY KEY &
			PO_ORDERJOUR::PO, &
		ALTERNATE KEY &
		( &
			PO_ORDERJOUR::VENDOR, &
			PO_ORDERJOUR::PO &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			PO_ORDERJOUR::POTYPE, &
			PO_ORDERJOUR::PO &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

