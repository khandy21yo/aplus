	!
	! File Layout for: PC.PC_COST on 21-May-01
	!
	! Product Standard Cost
	!

	RECORD PC_COST_CDD
		! Element = PRODUCT
		!   Description = Product number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Effective date (MMDDYYYY)
		STRING EFFDATE = 8
		! Element = AMOUNT
		!   Description = Product standard cost
		GFLOAT COST
	END RECORD
