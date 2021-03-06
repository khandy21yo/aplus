	!======================================================================
	! BT_JOURNALH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BT_JOURNALH.CH%, STAT%)
	CALL READ_DEVICE('BT_JOURNALH',BT_JOURNALH.DEV$, STAT%)

	BT_JOURNALH.NAME$ = BT_JOURNALH.DEV$+"BT_JOURNALH_"+BATCH_NO$+".JRL"

	OPEN BT_JOURNALH.NAME$ FOR INPUT AS FILE BT_JOURNALH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BT_JOURNALH, &
		PRIMARY KEY &
			BT_JOURNALH::CUSNUM, &
		ACCESS MODIFY, ALLOW MODIFY

