	!======================================================================
	! OS_JLINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OS_JLINE.CH%, STAT%)
	CALL READ_DEVICE('OS_JLINE',OS_JLINE.DEV$, STAT%)

	OS_JLINE.NAME$ = OS_JLINE.DEV$+"OS_JLINE_"+BATCH_NO$+".JRL"

	OPEN OS_JLINE.NAME$ FOR INPUT AS FILE OS_JLINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OS_JLINE, &
		PRIMARY KEY &
		( &
			OS_JLINE::ORDNUM, &
			OS_JLINE::JLINE, &
			OS_JLINE::LLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

