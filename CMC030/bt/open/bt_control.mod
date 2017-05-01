	!======================================================================
	! BT_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BT_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('BT_CONTROL',BT_CONTROL.DEV$, STAT%)

	BT_CONTROL.NAME$ = BT_CONTROL.DEV$+"BT_CONTROL.CTR"

	OPEN BT_CONTROL.NAME$ FOR INPUT AS FILE BT_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP BT_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

