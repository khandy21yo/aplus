	!======================================================================
	! UTL_BATCH_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_BATCH_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('UTL_BATCH_CONTROL',UTL_BATCH_CONTROL.DEV$, STAT%)

	UTL_BATCH_CONTROL.NAME$ = UTL_BATCH_CONTROL.DEV$+"UTL_BATCH_CONTROL.CTR"

	OPEN UTL_BATCH_CONTROL.NAME$ FOR INPUT AS FILE UTL_BATCH_CONTROL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_BATCH_CONTROL, &
		PRIMARY KEY &
			UTL_BATCH_CONTROL::BATCH, &
		ACCESS MODIFY, ALLOW MODIFY

