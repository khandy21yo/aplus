	!======================================================================
	! TS_CONTROL file (open read/write)
	!======================================================================

	OPEN TS_CONTROL.DEV$+"TS_CONTROL.CTR" FOR INPUT AS FILE TS_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP TS_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

