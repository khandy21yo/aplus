	!======================================================================
	! PR_TAX_PKG file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TAX_PKG.CH%, STAT%)
	CALL READ_DEVICE('PR_TAX_PKG',PR_TAX_PKG.DEV$, STAT%)

	PR_TAX_PKG.NAME$ = PR_TAX_PKG.DEV$+"PR_TAX_PKG.TBL"

	OPEN PR_TAX_PKG.NAME$ FOR INPUT AS FILE PR_TAX_PKG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TAX_PKG, &
		PRIMARY KEY &
		( &
			PR_TAX_PKG::TAX_PKG, &
			PR_TAX_PKG::STTYPE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

