	!======================================================================
	! IC_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('IC_CONTROL',IC_CONTROL.DEV$, STAT%)

	IC_CONTROL.NAME$ = IC_CONTROL.DEV$+"IC_CONTROL.CTR"

	OPEN IC_CONTROL.NAME$ FOR INPUT AS FILE IC_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP IC_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

