	!
	! File Layout for: SB.SB_BUDGET on 21-May-01
	!
	! Subaccount Budget File
	!

	RECORD SB_BUDGET_CDD
		! Element =
		!   Description = System name
		STRING SYSTEM = 2
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCOUNT = 10
		! Element = OPERATION
		!   Description = Operation
		STRING OPERATION = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
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
	END RECORD
