	!
	! File Layout for: PR.PR_EMP_STD_ERNDED on 21-May-01
	!
	! Payroll Employee Standard ERNDED File
	!

	RECORD PR_EMP_STD_ERNDED_CDD
		! Element =
		!   Description = Employee Number
		STRING EMPNUM = 10
		! Element =
		!   Description = Payment,Deduction,noncompensaTion,Memo
		STRING RTYPE = 1
		! Element =
		!   Description = Earnings/Deduction code
		STRING CODE = 2
		! Element =
		!   Description = Rate or amount of Earn/Ded
		GFLOAT RATE
		! Element =
		!   Description = Limit to pay or deduction
		GFLOAT LIMIT
		! Element =
		!   Description = Amount earn/ded to date
		GFLOAT CTDBAL
		! Element =
		!   Description = Amount accrued
		GFLOAT ACCRUED
		! Element =
		!   Description = Date to stop paying/ded
		STRING ENDDAT = 8
		! Element =
		!   Description = Earn/Ded frequency
		STRING FREQ = 6
		! Element =
		!   Description = 1-hourly,2-mile,3-gross,4-net,5-per pay
		STRING METHOD = 1
		! Element =
		!   Description = User defined
		STRING USERDEF = 30
	END RECORD
