	!======================================================================
	! UTL_PROFILE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_PROFILE.CH%, STAT%)
	CALL READ_DEVICE('UTL_PROFILE',UTL_PROFILE.DEV$, STAT%)

	UTL_PROFILE.NAME$ = UTL_PROFILE.DEV$+"UTL_PROFILE.TBL"

	OPEN UTL_PROFILE.NAME$ FOR INPUT AS FILE UTL_PROFILE.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP UTL_PROFILE, &
		ACCESS MODIFY, ALLOW MODIFY

