	!
	! File Layout for: AD.AD_ACCOUNT on 21-May-01
	!
	! Asset and Depreciation Account Table
	!

	RECORD AD_ACCOUNT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Asset type
		STRING ASSET_TYPE = 2
		! Element = ACCOUNT
		!   Description = Asset Account Number
		STRING ASS_ACCT = 18
		! Element = ACCOUNT
		!   Description = Depreciation Account Number
		STRING DEP_ACCT = 18
		! Element = ACCOUNT
		!   Description = Expense Account Number
		STRING EXP_ACCT = 18
	END RECORD
