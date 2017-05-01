	!======================================================================
	! BC_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BC_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('BC_CONTROL',BC_CONTROL.DEV$, STAT%)

	BC_CONTROL.NAME$ = BC_CONTROL.DEV$+"BC_CONTROL.CTR"

	OPEN BC_CONTROL.NAME$ FOR INPUT AS FILE BC_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP BC_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

