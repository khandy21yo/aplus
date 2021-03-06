	!======================================================================
	! CK_CKMNT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(CK_CKMNT.CH%, STAT%)
	CALL READ_DEVICE('CK_CKMNT',CK_CKMNT.DEV$, STAT%)

	CK_CKMNT.NAME$ = CK_CKMNT.DEV$+"CK_CKMNT.MAS"

	OPEN CK_CKMNT.NAME$ FOR INPUT AS FILE CK_CKMNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP CK_CKMNT, &
		PRIMARY KEY &
		( &
			CK_CKMNT::BANK_ACCT, &
			CK_CKMNT::CKNUM, &
			CK_CKMNT::ETYPE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

