	!
	! File Layout for: PR.PR_RATERANGE on 21-May-01
	!
	! Payroll Rate Range
	!

	RECORD PR_RATERANGE_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Age
		STRING AGE = 3
		! Element =
		!   Description = Minimum Rate allowed
		GFLOAT MIN_RATE
		! Element =
		!   Description = Maximum Rate Allowed
		GFLOAT MAX_RATE
	END RECORD
