	!======================================================================
	! PW_JH file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PW_JH.CH%, STAT%)
	CALL READ_DEVICE('PW_JH',PW_JH.DEV$, STAT%)

	PW_JH.NAME$ = PW_JH.DEV$+"PW_JH_"+BATCH_NO$+".JRL"

	OPEN PW_JH.NAME$ FOR INPUT AS FILE PW_JH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PW_JH, &
		PRIMARY KEY &
			PW_JH::ORDNUM, &
		ACCESS MODIFY, ALLOW MODIFY

