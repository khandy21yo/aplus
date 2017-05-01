	!======================================================================
	! PC_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PC_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('PC_CONTROL',PC_CONTROL.DEV$, STAT%)

	PC_CONTROL.NAME$ = PC_CONTROL.DEV$+"PC_CONTROL.CTR"

	OPEN PC_CONTROL.NAME$ FOR INPUT AS FILE PC_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP PC_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

