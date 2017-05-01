	!======================================================================
	! CK_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(CK_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('CK_CONTROL',CK_CONTROL.DEV$, STAT%)

	CK_CONTROL.NAME$ = CK_CONTROL.DEV$+"CK_CONTROL.CTR"

	OPEN CK_CONTROL.NAME$ FOR INPUT AS FILE CK_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP CK_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

