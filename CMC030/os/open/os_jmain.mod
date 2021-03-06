	!======================================================================
	! OS_JMAIN file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OS_JMAIN.CH%, STAT%)
	CALL READ_DEVICE('OS_JMAIN',OS_JMAIN.DEV$, STAT%)

	OS_JMAIN.NAME$ = OS_JMAIN.DEV$+"OS_JMAIN_"+BATCH_NO$+".JRL"

	OPEN OS_JMAIN.NAME$ FOR INPUT AS FILE OS_JMAIN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OS_JMAIN, &
		PRIMARY KEY &
		( &
			OS_JMAIN::ORDNUM, &
			OS_JMAIN::JLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

