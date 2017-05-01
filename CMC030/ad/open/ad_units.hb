	!
	! File Layout for: AD.AD_UNITS on 21-May-01
	!
	! Units of Production Journal
	!

	RECORD AD_UNITS_CDD
		! Element = DEP_OBJECT
		!   Description = Depreciation object
		STRING DEP_OBJECT = 1
		! Element = DATE
		!   Description = Date
		STRING ACTION_DATE = 8
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element =
		!   Description = Units
		LONG QUANTITY
	END RECORD
