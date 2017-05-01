	!
	! File Layout for: TV.TV_PROGRAM on 21-May-01
	!
	! TV Program Master File
	!

	RECORD TV_PROGRAM_CDD
		! Element =
		!   Description = Program number
		STRING PRGNUM = 10
		! Element =
		!   Description = Run time
		STRING START_TIME(6) = 6
		! Element =
		!   Description = From Date
		STRING FROM_DATE = 8
		! Element =
		!   Description = To date
		STRING TO_DATE = 8
		! Element =
		!   Description = Program title
		STRING TITLE = 50
		! Element =
		!   Description = Program Source
		STRING SOURCE = 04
		! Element =
		!   Description = Program Type
		STRING PTYPE = 4
		! Element =
		!   Description = Program Length
		STRING LENGTH = 6
		! Element =
		!   Description = Comment
		STRING COMMENT = 50
		! Element =
		!   Description = Cutaway flag
		STRING CUTAWAY = 10
	END RECORD
