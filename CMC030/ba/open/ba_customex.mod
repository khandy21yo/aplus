	!======================================================================
	! BA_CUSTOMEX file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BA_CUSTOMEX.CH%, STAT%)
	CALL READ_DEVICE('BA_CUSTOMEX',BA_CUSTOMEX.DEV$, STAT%)

	BA_CUSTOMEX.NAME$ = BA_CUSTOMEX.DEV$+"BA_CUSTOMEX.MAS"

	OPEN BA_CUSTOMEX.NAME$ FOR INPUT AS FILE BA_CUSTOMEX.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BA_CUSTOMEX, &
		PRIMARY KEY &
			BA_CUSTOMEX::CUSNUM, &
		ACCESS MODIFY, ALLOW MODIFY

