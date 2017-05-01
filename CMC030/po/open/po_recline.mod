	!======================================================================
	! PO_RECLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_RECLINE.CH%, STAT%)
	CALL READ_DEVICE('PO_RECLINE',PO_RECLINE.DEV$, STAT%)

	PO_RECLINE.NAME$ = PO_RECLINE.DEV$+"PO_RECLINE_"+BATCH_NO$+".JRL"

	OPEN PO_RECLINE.NAME$ FOR INPUT AS FILE PO_RECLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_RECLINE, &
		PRIMARY KEY &
		( &
			PO_RECLINE::PO, &
			PO_RECLINE::PO_LINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

