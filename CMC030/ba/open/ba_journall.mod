	!======================================================================
	! BA_JOURNALL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BA_JOURNALL.CH%, STAT%)
	CALL READ_DEVICE('BA_JOURNALL',BA_JOURNALL.DEV$, STAT%)

	BA_JOURNALL.NAME$ = BA_JOURNALL.DEV$+"BA_JOURNALL_"+BATCH_NO$+".JRL"

	OPEN BA_JOURNALL.NAME$ FOR INPUT AS FILE BA_JOURNALL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BA_JOURNALL, &
		PRIMARY KEY &
		( &
			BA_JOURNALL::BILLNUM, &
			BA_JOURNALL::EMPNUM &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

