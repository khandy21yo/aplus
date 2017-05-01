	!======================================================================
	! UTL_SPELL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_SPELL.CH%, STAT%)
	CALL READ_DEVICE('UTL_SPELL',UTL_SPELL.DEV$, STAT%)

	UTL_SPELL.NAME$ = UTL_SPELL.DEV$+"UTL_SPELL.TBL"

	OPEN UTL_SPELL.NAME$ FOR INPUT AS FILE UTL_SPELL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_SPELL, &
		PRIMARY KEY &
			UTL_SPELL::AWORD, &
		ACCESS MODIFY, ALLOW MODIFY

