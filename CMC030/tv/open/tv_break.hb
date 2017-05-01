	!
	! File Layout for: TV.TV_BREAK on 21-May-01
	!
	! TV Break Master File
	!

	RECORD TV_BREAK_CDD
		! Element =
		!   Description = Program number
		STRING PRGNUM = 10
		! Element =
		!   Description = Relative Run time
		STRING RUN_TIME = 6
		! Element =
		!   Description = Break Description
		STRING DESCR = 30
		! Element =
		!   Description = Break Type
		STRING BRKTYPE = 2
		! Element =
		!   Description = Break Length
		STRING LENGTH = 6
		! Element =
		!   Description = Comment
		STRING COMMENT = 30
		! Element =
		!   Description = Maximum number of commercials
		WORD MAXCOM
		! Element =
		!   Description = Priority
		WORD PRIORITY
		! Element =
		!   Description = Match code
		STRING MATCH = 6
		! Element =
		!   Description = Fill (usable for anything)
		STRING FILLER = 2
	END RECORD
