	!======================================================================
	! UTL_DEVICE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_DEVICE.CH%, STAT%)
	CALL READ_DEVICE('UTL_DEVICE',UTL_DEVICE.DEV$, STAT%)

	UTL_DEVICE.NAME$ = "CMC$DEVICE:UTL_DEVICE.TBL"

	OPEN UTL_DEVICE.NAME$ FOR INPUT AS FILE UTL_DEVICE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_DEVICE, &
		PRIMARY KEY &
			UTL_DEVICE::FILENAM, &
		ALTERNATE KEY &
		( &
			UTL_DEVICE::CATAG, &
			UTL_DEVICE::FILENAM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

