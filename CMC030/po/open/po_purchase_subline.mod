	!======================================================================
	! PO_PURCHASE_SUBLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_PURCHASE_SUBLINE.CH%, STAT%)
	CALL READ_DEVICE('PO_PURCHASE_SUBLINE',PO_PURCHASE_SUBLINE.DEV$, STAT%)

	PO_PURCHASE_SUBLINE.NAME$ = PO_PURCHASE_SUBLINE.DEV$+"PO_PURCHASE_SUBLINE_"+BATCH_N

	OPEN PO_PURCHASE_SUBLINE.NAME$ FOR INPUT AS FILE PO_PURCHASE_SUBLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_PURCHASE_SUBLINE, &
		ACCESS MODIFY, ALLOW MODIFY

