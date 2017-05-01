	!======================================================================
	! BA_BILTBL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BA_BILTBL.CH%, STAT%)
	CALL READ_DEVICE('BA_BILTBL',BA_BILTBL.DEV$, STAT%)

	BA_BILTBL.NAME$ = BA_BILTBL.DEV$+"BA_BILTBL.TBL"

	OPEN BA_BILTBL.NAME$ FOR INPUT AS FILE BA_BILTBL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BA_BILTBL, &
		PRIMARY KEY &
		( &
			BA_BILTBL::EMPNUM, &
			BA_BILTBL::CUSNUM, &
			BA_BILTBL::FROMDATE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

