	!======================================================================
	! PP_CARD file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PP_CARD.CH%, STAT%)
	CALL READ_DEVICE('PP_CARD',PP_CARD.DEV$, STAT%)

	PP_CARD.NAME$ = PP_CARD.DEV$+"PP_CARD.MAS"

	OPEN PP_CARD.NAME$ FOR INPUT AS FILE PP_CARD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_CARD, &
		PRIMARY KEY &
		( &
			PP_CARD::CUSNUM, &
			PP_CARD::CARD &
		)	, &
		ALTERNATE KEY &
		( &
			PP_CARD::SYSCUS, &
			PP_CARD::CARD &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			PP_CARD::CARD, &
			PP_CARD::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

