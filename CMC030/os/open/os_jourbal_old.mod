	!======================================================================
	! OS_JOURNAL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OS_JOURNAL.CH%, STAT%)
	CALL READ_DEVICE('OS_JOURNAL',OS_JOURNAL.DEV$, STAT%)

	OS_JOURNAL.NAME$ = OS_JOURNAL.DEV$+"OS_JOURNAL_"+BATCH_NO$+".JRL"

	OPEN OS_JOURNAL.NAME$ FOR INPUT AS FILE OS_JOURNAL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OS_JOURNAL, &
		PRIMARY KEY &
			OS_JOURNAL::TRANS, &
		ALTERNATE KEY &
			OS_JOURNAL::CUSTOMER &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

