	!======================================================================
	! PO_RECEIVER_LINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_RECEIVER_LINE.CH%, STAT%)
	CALL READ_DEVICE('PO_RECEIVER_LINE',PO_RECEIVER_LINE.DEV$, STAT%)

	PO_RECEIVER_LINE.NAME$ = PO_RECEIVER_LINE.DEV$+"PO_RECEIVER_LINE."

	OPEN PO_RECEIVER_LINE.NAME$ FOR INPUT AS FILE PO_RECEIVER_LINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_RECEIVER_LINE, &
		PRIMARY KEY &
		( &
			PO_RECEIVER_LINE::PO_NUM, &
			PO_RECEIVER_LINE::LINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

