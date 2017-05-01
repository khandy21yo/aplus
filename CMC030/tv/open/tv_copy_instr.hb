	!
	! File Layout for: TV.TV_COPY_INSTR on 21-May-01
	!
	! TV Copy Instructions
	!

	RECORD TV_COPY_INSTR_CDD
		! Element =
		!   Description = Form number
		STRING FRMNUM = 8
		! Element =
		!   Description = Sequence number
		STRING SEQNUM = 2
		! Element =
		!   Description = From date
		STRING FROM_DATE = 8
		! Element =
		!   Description = To date
		STRING TO_DATE = 8
		! Element =
		!   Description = Length
		STRING LENGTH = 6
		! Element =
		!   Description = From time
		STRING FROM_TIME = 6
		! Element =
		!   Description = To time
		STRING TO_TIME = 6
		! Element =
		!   Description = Spot rotations
		STRING SPOT_ROTATION = 30
		! Element =
		!   Description = Current Rotation
		WORD CURRENT_ROTATION
	END RECORD
