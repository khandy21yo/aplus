	!
	! File Layout for: AD.AD_JOURNAL on 21-May-01
	!
	! Depreciation Units Journal
	!

	RECORD AD_JOURNAL_CDD
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = DATE
		!   Description = Date
		STRING ACTION_DATE = 8
		! Element = STATIONMAN
		!   Description = Station man (operator)
		STRING STATIONMAN = 10
	END RECORD
