	!======================================================================
	! PO_PURCHASE_LINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_PURCHASE_LINE.CH%, STAT%)
	CALL READ_DEVICE('PO_PURCHASE_LINE',PO_PURCHASE_LINE.DEV$, STAT%)

	PO_PURCHASE_LINE.NAME$ = PO_PURCHASE_LINE.DEV$+"PO_PURCHASE_LINE_"+BATCH_NO$+".J

	OPEN PO_PURCHASE_LINE.NAME$ FOR INPUT AS FILE PO_PURCHASE_LINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_PURCHASE_LINE, &
		PRIMARY KEY &
		( &
			PO_PURCHASE_LINE::PO_NUM, &
			PO_PURCHASE_LINE::ITEM &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

