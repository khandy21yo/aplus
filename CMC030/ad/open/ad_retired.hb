	!
	! File Layout for: AD.AD_RETIRED on 21-May-01
	!
	! Retired Asset
	!

	RECORD AD_RETIRED_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element = DATE
		!   Description = Date when asset has been retired
		STRING RET_DATE = 8
		! Element =
		!   Description = Amount of disposition
		GFLOAT PROCEEDS
		! Element =
		!   Description = Notes
		STRING NOTES = 40
	END RECORD
