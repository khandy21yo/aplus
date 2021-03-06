	!======================================================================
	! PR_UNPN_DEF file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_UNPN_DEF.CH%, STAT%)
	CALL READ_DEVICE('PR_UNPN_DEF',PR_UNPN_DEF.DEV$, STAT%)

	PR_UNPN_DEF.NAME$ = PR_UNPN_DEF.DEV$+"PR_UNPN_DEF.TBL"

	OPEN PR_UNPN_DEF.NAME$ FOR INPUT AS FILE PR_UNPN_DEF.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_UNPN_DEF, &
		PRIMARY KEY &
		( &
			PR_UNPN_DEF::CODE, &
			PR_UNPN_DEF::DTYPE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

