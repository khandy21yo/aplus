	!
	! File Layout for: PR.PR_EMP_ACCRUAL_RATE on 21-May-01
	!
	! Accrual Definition Rates
	!

	RECORD PR_EMP_ACCRUAL_RATE_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = Accrual (Pay) Type
		STRING ATYPE = 2
		! Element = DATE
		!   Description = Start Date (YYYYMMDD)
		STRING SDATE = 8
		! Element =
		!   Description = Minimum hours worked to get
		GFLOAT MINHOUR
		! Element =
		!   Description = Maximum hours that get accrued for
		GFLOAT MAXHOUR
		! Element =
		!   Description = Rate per hour worked
		GFLOAT HOURRATE
		! Element =
		!   Description = Maximum unused to allow
		GFLOAT MAXACCRUE
		! Element =
		!   Description = Rate Code if we ever need it
		STRING RATECODE = 1
		! Element =
		!   Description = Periods to accrue for (12345S)
		STRING FREQ = 6
	END RECORD
