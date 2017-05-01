	!======================================================================
	! TV_COPY_INSTR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_COPY_INSTR.CH%, STAT%)
	CALL READ_DEVICE('TV_COPY_INSTR',TV_COPY_INSTR.DEV$, STAT%)

	TV_COPY_INSTR.NAME$ = TV_COPY_INSTR.DEV$+"TV_COPY_INSTR.LED"

	OPEN TV_COPY_INSTR.NAME$ FOR INPUT AS FILE TV_COPY_INSTR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_COPY_INSTR, &
		PRIMARY KEY &
		( &
			TV_COPY_INSTR::FRMNUM, &
			TV_COPY_INSTR::SEQNUM &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

