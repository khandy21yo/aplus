	!======================================================================
	! BA_CUSTYP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BA_CUSTYP.CH%, STAT%)
	CALL READ_DEVICE('BA_CUSTYP',BA_CUSTYP.DEV$, STAT%)

	BA_CUSTYP.NAME$ = BA_CUSTYP.DEV$+"BA_CUSTYP.TBL"

	OPEN BA_CUSTYP.NAME$ FOR INPUT AS FILE BA_CUSTYP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BA_CUSTYP, &
		PRIMARY KEY &
			BA_CUSTYP::CUSTYP, &
		ACCESS MODIFY, ALLOW MODIFY

