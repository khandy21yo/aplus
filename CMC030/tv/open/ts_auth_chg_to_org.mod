	!======================================================================
	! TS_AUTH_CHG_TO_ORG file (open read/write)
	!======================================================================

	OPEN TS_AUTH_CHG_TO_ORG.DEV$+"TS_AUTH_CHG_TO_ORG_"+BATCH_NO$+".JR FOR INPUT AS FILE TS_AUTH_CHG_TO_ORG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TS_AUTH_CHG_TO_ORG, &
		PRIMARY KEY &
		( &
			TS_AUTH_CHG_TO_ORG::TANUM, &
			TS_AUTH_CHG_TO_ORG::LIN &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

