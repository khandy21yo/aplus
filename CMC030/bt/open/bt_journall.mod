	!======================================================================
	! BT_JOURNALL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BT_JOURNALL.CH%, STAT%)
	CALL READ_DEVICE('BT_JOURNALL',BT_JOURNALL.DEV$, STAT%)

	BT_JOURNALL.NAME$ = BT_JOURNALL.DEV$+"BT_JOURNALL.JRL"

	OPEN BT_JOURNALL.NAME$ FOR INPUT AS FILE BT_JOURNALL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BT_JOURNALL, &
		PRIMARY KEY &
		( &
			BT_JOURNALL::CUSNUM, &
			BT_JOURNALL::CHILD &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

