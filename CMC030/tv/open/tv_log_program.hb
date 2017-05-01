	!
	! File Layout for: TV.TV_LOG_PROGRAM on 21-May-01
	!
	! TV Program Log
	!

	RECORD TV_LOG_PROGRAM_CDD
		! Element =
		!   Description = Run date
		STRING DATE = 8
		! Element =
		!   Description = Run time
		STRING START_TIME = 6
		! Element =
		!   Description = Actual run time
		STRING RUN_TIME = 6
		! Element =
		!   Description = Program number
		STRING PRGNUM = 10
		! Element =
		!   Description = Title
		STRING TITLE = 40
		! Element =
		!   Description = Source
		STRING SOURCE = 4
		! Element =
		!   Description = Type
		STRING PTYPE = 4
		! Element =
		!   Description = Length
		STRING LENGTH = 6
		! Element =
		!   Description = Run (0-yes, 1-no, 2-cancelled)
		STRING RUN = 1
		! Element =
		!   Description = Comment
		STRING COMMENT = 50
		! Element =
		!   Description = Cutaway flag (Y/N)
		STRING CUTAWAY = 10
	END RECORD
