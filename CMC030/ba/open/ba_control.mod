	!======================================================================
	! BA_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BA_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('BA_CONTROL',BA_CONTROL.DEV$, STAT%)

	BA_CONTROL.NAME$ = BA_CONTROL.DEV$+"BA_CONTROL.CTR"

	OPEN BA_CONTROL.NAME$ FOR INPUT AS FILE BA_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP BA_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

