	!
	! File Layout for: TV.TV_FILL on 21-May-01
	!
	! TV Fill Master File
	!

	RECORD TV_FILL_CDD
		! Element =
		!   Description = Fill number
		STRING FILNUM = 10
		! Element =
		!   Description = Description
		STRING DESCR = 30
		! Element =
		!   Description = Fill class
		STRING FCLASS = 4
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
		!   Description = Number of runs
		WORD RUNS
		! Element =
		!   Description = Number of cuts on tape
		WORD CUTS
		! Element =
		!   Description = Current cut
		WORD CURRENT_CUT
	END RECORD
