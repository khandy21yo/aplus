	!------------------------------------------------------------------
	! Open up general ledger budget file for year
	!------------------------------------------------------------------

	OPEN GL_BUDGET.DEV$ + "GL_BUD_" + GL_BUDGET.YEAR$ + ".MAS" &
		FOR INPUT AS FILE GL_BUDGET.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_BUDGET, &
		PRIMARY KEY GL_BUDGET::ACCT, &
		ACCESS MODIFY, &
		ALLOW MODIFY

