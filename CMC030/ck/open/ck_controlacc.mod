	!======================================================================
	! CK_CONTROLACC file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(CK_CONTROLACC.CH%, STAT%)
	CALL READ_DEVICE('CK_CONTROLACC',CK_CONTROLACC.DEV$, STAT%)

	CK_CONTROLACC.NAME$ = CK_CONTROLACC.DEV$+"CK_CONTROLACC.CTR"

	OPEN CK_CONTROLACC.NAME$ FOR INPUT AS FILE CK_CONTROLACC.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP CK_CONTROLACC, &
		PRIMARY KEY &
			CK_CONTROLACC::ACCOUNT &
			DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

