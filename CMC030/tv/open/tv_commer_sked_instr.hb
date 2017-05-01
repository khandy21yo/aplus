	!
	! File Layout for: TV.TV_COMMER_SKED_INSTR on 21-May-01
	!
	! Commercial Schedule Instructions
	!

	RECORD TV_COMMER_SKED_INSTR_CDD
		! Element = TV_FRMNUM
		!   Description = Form Number
		STRING FRMNUM = 8
		! Element =
		!   Description = Schedule number
		STRING SKED_NUM = 2
		! Element =
		!   Description = Start date
		STRING START_DATE = 8
		! Element =
		!   Description = End date
		STRING END_DATE = 8
		! Element =
		!   Description = Start time slot
		STRING START_TIME = 6
		! Element =
		!   Description = End time slot
		STRING END_TIME = 6
		! Element =
		!   Description = Number of weeks in
		WORD IN_WEEKS
		! Element =
		!   Description = Number of weeks out
		WORD OUT_WEEKS
		! Element =
		!   Description = Length
		STRING LENGTH = 6
		! Element =
		!   Description = Rate per spot
		GFLOAT RATE_PER_SPOT
		! Element =
		!   Description = Spots per day (mon thur sun)
		WORD SPOTS_PER_DAY(6)
		! Element =
		!   Description = Used for rotating spots
		WORD TOTAL_SPOTS
	END RECORD
