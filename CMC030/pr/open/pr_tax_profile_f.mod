	!======================================================================
	! PR_TAX_PROFILE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TAX_PROFILE.CH%, STAT%)
	CALL READ_DEVICE('PR_TAX_PROFILE',PR_TAX_PROFILE.DEV$, STAT%)

	PR_TAX_PROFILE.NAME$ = PR_TAX_PROFILE.DEV$+"PR_TAX_PROFILE.TBL"

	OPEN PR_TAX_PROFILE.NAME$ FOR INPUT AS FILE PR_TAX_PROFILE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TAX_PROFILE_F, &
		PRIMARY KEY &
		( &
			PR_TAX_PROFILE_F::AUTH, &
			PR_TAX_PROFILE_F::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

