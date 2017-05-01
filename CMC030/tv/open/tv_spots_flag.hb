	!
	! File Layout for: TV.TV_SPOTS_FLAG on 21-May-01
	!
	! Spots Status Flag Table
	!

	RECORD TV_SPOTS_FLAG_CDD
		! Element =
		!   Description = S-schedule, N-not run, R-run
		STRING FLAG = 1
		! Element =
		!   Description = Code
		STRING CODE = 2
		! Element =
		!   Description = Scheduled to run code
		STRING SCHD_RUN_CODE = 2
		! Element =
		!   Description = Code description
		STRING DESCR = 30
	END RECORD
