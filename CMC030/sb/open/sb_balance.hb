	!
	! File Layout for: SB.SB_BALANCE on 21-May-01
	!
	! Subaccount Balance File
	!

	RECORD SB_BALANCE_CDD
		! Element = SYSTEM
		!   Description = System name
		STRING SYSTEM = 2
		! Element = SUBACCT
		!   Description = Sub Account Number
		STRING SUBACCOUNT = 10
		! Element = OPERATION
		!   Description =
		STRING OPERATION = 8
		! Element = ACCOUNT
		!   Description = GL Account Number
		STRING ACCOUNT = 18
		! Element = PERIOD
		!   Description = YYYYPP
		STRING PERIOD = 6
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
		! Element =
		!   Description = Units
		GFLOAT UNITS
		! Element =
		!   Description = Hours
		GFLOAT HOURS
		! Element =
		!   Description = Beginning Amount
		GFLOAT BEG_AMOUNT
		! Element =
		!   Description = Beginning Units
		GFLOAT BEG_UNITS
		! Element =
		!   Description = Beginning Hours
		GFLOAT BEG_HOURS
	END RECORD
