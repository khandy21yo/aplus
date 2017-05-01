	!
	! File Layout for: BA.BA_JOURNALL on 21-May-01
	!
	! Agency Journal
	!

	RECORD BA_JOURNALL_CDD
		! Element =
		!   Description = Billing Number
		STRING BILLNUM = 10
		! Element = EMPLOYEE
		!   Description = Client Number
		STRING EMPNUM = 10
		! Element =
		!   Description = Days worked
		GFLOAT DAYS
		! Element =
		!   Description = Hours Worked
		GFLOAT HOURS
		! Element =
		!   Description = Units Worked
		GFLOAT UNITS
		! Element =
		!   Description = Wages Worked
		GFLOAT WAGES
		! Element =
		!   Description = Rate billed
		GFLOAT RATE
		! Element =
		!   Description = Fee Billed
		GFLOAT FEE
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Billing Method
		STRING METHOD = 1
	END RECORD
