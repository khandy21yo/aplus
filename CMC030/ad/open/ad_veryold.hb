	!
	! File Layout for: AD.AD_VERYOLD on 21-May-01
	!
	! Old (RSTS) asset format
	!

	RECORD AD_VERYOLD_CDD
		! Element = ASSET
		!   Description = Asset number
		STRING ASSET = 10
		! Element = DESCRIPTION
		!   Description = Description 1
		STRING DESCR1 = 40
		! Element = DESCRIPTION
		!   Description = Description 2
		STRING DESCR2 = 40
		! Element = DESCRIPTION
		!   Description = Description 3
		STRING DESCR3 = 40
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = DATE
		!   Description = Pirchase Date (YYYYMMDD)
		STRING PURCHASE = 8
		! Element = DATE
		!   Description = Disposal Date (YYYYMMDD)
		STRING DISPOSAL = 8
		! Element = METHOD
		!   Description = Method
		STRING METHOD = 2
		! Element =
		!   Description = Lifetime
		LONG LIFE
		! Element =
		!   Description = Cost/Basis
		GFLOAT COSTBASIS
		! Element =
		!   Description = Salvage
		GFLOAT SALVAGE
		! Element =
		!   Description = Invest Tax Credit
		GFLOAT INVEST
		! Element =
		!   Description = First Year Allowance
		GFLOAT FIRSTYEAR
		! Element =
		!   Description = Begining Depreciation
		GFLOAT BEGIN
		! Element =
		!   Description = this Period Depreciation
		GFLOAT THISPER
		! Element =
		!   Description = Accountants Dep.
		GFLOAT ACCOUNTANTS
		! Element =
		!   Description = ACRS Life
		LONG ACRSLIFE
		! Element =
		!   Description = ACRS Method
		STRING ACRSMETHOD = 2
	END RECORD
