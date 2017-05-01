	!======================================================================
	! TV_AUTH_CHG_TO_ORG file (open read/write)
	!======================================================================

	OPEN TV_AUTH_CHG_TO_ORG.DEV$+"TV_AUTH_CHG_TO_ORG.JRL" FOR INPUT AS FILE TV_AUTH_CHG_TO_ORG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_AUTH_CHG_TO_ORG, &
		PRIMARY KEY &
		( &
			TV_AUTH_CHG_TO_ORG::TANUM, &
			TV_AUTH_CHG_TO_ORG::LIN &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

