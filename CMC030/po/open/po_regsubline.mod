	!======================================================================
	! PO_REGSUBLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_REGSUBLINE.CH%, STAT%)
	CALL READ_DEVICE('PO_REGSUBLINE',PO_REGSUBLINE.DEV$, STAT%)

	PO_REGSUBLINE.NAME$ = PO_REGSUBLINE.DEV$+"PO_REGSUBLINE.HIS"

	OPEN PO_REGSUBLINE.NAME$ FOR INPUT AS FILE PO_REGSUBLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_REGSUBLINE, &
		PRIMARY KEY &
		( &
			PO_REGSUBLINE::PO_NUM, &
			PO_REGSUBLINE::ITEM &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PO_REGSUBLINE::TRANS_DATE, &
			PO_REGSUBLINE::PO_NUM, &
			PO_REGSUBLINE::ITEM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			PO_REGSUBLINE::POST_DATE &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PO_REGSUBLINE::SUBACCOUNT, &
			PO_REGSUBLINE::PO_NUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

