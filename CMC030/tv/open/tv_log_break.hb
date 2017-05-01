	!
	! File Layout for: TV.TV_LOG_BREAK on 21-May-01
	!
	! TV Break Log
	!

	RECORD TV_LOG_BREAK_CDD
		! Element =
		!   Description = Date of break
		STRING DATE = 8
		! Element =
		!   Description = Scheduled Time of break
		STRING SCH_TIME = 6
		! Element =
		!   Description = Actual run time
		STRING RUN_TIME = 6
		! Element =
		!   Description = Break Number
		STRING PRGNUM = 10
		! Element =
		!   Description = Break Description
		STRING DESCR = 30
		! Element =
		!   Description = Type of break
		STRING BRKTYPE = 2
		! Element =
		!   Description = Length of break
		STRING LENGTH = 6
		! Element =
		!   Description = Comment
		STRING COMMENT = 30
		! Element =
		!   Description = Maximum number of commercials
		WORD MAXCOM
		! Element =
		!   Description = Break priority
		WORD PRIORITY
		! Element =
		!   Description = Match code
		STRING MATCH = 6
		! Element =
		!   Description = Fill (usable for anything)
		STRING FILLER = 2
		! Element =
		!   Description = Run (0-yes, 1-no, 2-cancelled)
		STRING RUN = 1
	END RECORD
