	!======================================================================
	! PR_SKILLS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_SKILLS.CH%, STAT%)
	CALL READ_DEVICE('PR_SKILLS',PR_SKILLS.DEV$, STAT%)

	PR_SKILLS.NAME$ = PR_SKILLS.DEV$+"PR_SKILLS.MAS"

	OPEN PR_SKILLS.NAME$ FOR INPUT AS FILE PR_SKILLS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_SKILLS, &
		PRIMARY KEY &
			PR_SKILLS::SKILL, &
		ALTERNATE KEY &
		( &
			PR_SKILLS::EEOSORT, &
			PR_SKILLS::SKILL &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

