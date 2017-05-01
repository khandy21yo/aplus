	!======================================================================
	! PR_ERNDED_DEF file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_ERNDED_DEF.CH%, STAT%)
	CALL READ_DEVICE('PR_ERNDED_DEF',PR_ERNDED_DEF.DEV$, STAT%)

	PR_ERNDED_DEF.NAME$ = PR_ERNDED_DEF.DEV$+"PR_ERNDED_DEF.MAS"

	OPEN PR_ERNDED_DEF.NAME$ FOR INPUT AS FILE PR_ERNDED_DEF.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_ERNDED_DEF, &
		PRIMARY KEY &
		( &
			PR_ERNDED_DEF::ETYPE, &
			PR_ERNDED_DEF::CODE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

