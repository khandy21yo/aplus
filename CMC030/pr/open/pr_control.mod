	!======================================================================
	! PR_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('PR_CONTROL',PR_CONTROL.DEV$, STAT%)

	PR_CONTROL.NAME$ = PR_CONTROL.DEV$+"PR_CONTROL.CTR"

	OPEN PR_CONTROL.NAME$ FOR INPUT AS FILE PR_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP PR_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

