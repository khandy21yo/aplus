	!======================================================================
	! PP_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PP_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('PP_CONTROL',PP_CONTROL.DEV$, STAT%)

	PP_CONTROL.NAME$ = PP_CONTROL.DEV$+"PP_CONTROL.CTR"

	OPEN PP_CONTROL.NAME$ FOR INPUT AS FILE PP_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP PP_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

