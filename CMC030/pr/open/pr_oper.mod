	!======================================================================
	! PR_OPER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_OPER.CH%, STAT%)
	CALL READ_DEVICE('PR_OPER',PR_OPER.DEV$, STAT%)

	PR_OPER.NAME$ = PR_OPER.DEV$+"PR_OPER.TBL"

	OPEN PR_OPER.NAME$ FOR INPUT AS FILE PR_OPER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_OPER, &
		PRIMARY KEY &
		( &
			PR_OPER::OPER, &
			PR_OPER::EFFDATE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

