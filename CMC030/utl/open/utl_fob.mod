	!======================================================================
	! UTL_FOB file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_FOB.CH%, STAT%)
	CALL READ_DEVICE('UTL_FOB',UTL_FOB.DEV$, STAT%)

	UTL_FOB.NAME$ = UTL_FOB.DEV$+"UTL_FOB.MAS"

	OPEN UTL_FOB.NAME$ FOR INPUT AS FILE UTL_FOB.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_FOB, &
		PRIMARY KEY &
			UTL_FOB::FOBCODE, &
		ALTERNATE KEY &
			UTL_FOB::DESCR &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

