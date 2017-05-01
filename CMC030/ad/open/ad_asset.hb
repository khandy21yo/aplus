	!
	! File Layout for: AD.AD_ASSET on 21-May-01
	!
	! Asset Description File
	!

	RECORD AD_ASSET_CDD
		! Element = ASSET_NUM
		!   Description = Asset number
		STRING ASSET_NUM = 10
		! Element =
		!   Description = Asset description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Asset type
		STRING ASSET_TYPE = 2
		! Element = LOCATION
		!   Description = Location
		STRING LOCATION = 4
		! Element = DEPT_NUM
		!   Description = Department number
		STRING DEPT_NUM = 6
		! Element =
		!   Description = Serial number
		STRING SERIAL_NUM = 20
		! Element = DATE
		!   Description = Service Date
		STRING SERVDATE = 8
		! Element =
		!   Description = Initial cost
		GFLOAT COST
		! Element =
		!   Description = Salvage
		GFLOAT SALVAGE
		! Element =
		!   Description = Bonus, Section 179
		GFLOAT BONUS
		! Element =
		!   Description = Investment tax credit
		GFLOAT ITC
		! Element =
		!   Description = ITC Basis Reduction
		GFLOAT ITCREDUCE
		! Element =
		!   Description = Life in Units
		GFLOAT UNITS
	END RECORD
