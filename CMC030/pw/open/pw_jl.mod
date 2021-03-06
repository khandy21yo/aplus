	!======================================================================
	! PW_JL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PW_JL.CH%, STAT%)
	CALL READ_DEVICE('PW_JL',PW_JL.DEV$, STAT%)

	PW_JL.NAME$ = PW_JL.DEV$+"PW_JL_"+BATCH_NO$+".JRL"

	OPEN PW_JL.NAME$ FOR INPUT AS FILE PW_JL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PW_JL, &
		PRIMARY KEY &
		( &
			PW_JL::ORDNUM, &
			PW_JL::WINDOW, &
			PW_JL::JLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

