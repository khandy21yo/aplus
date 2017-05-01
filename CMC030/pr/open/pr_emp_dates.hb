	!
	! File Layout for: PR.PR_EMP_DATES on 21-May-01
	!
	! Payroll Employee Date History
	!

	RECORD PR_EMP_DATES_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPLOYEE = 10
		! Element =
		!   Description = Date Code
		STRING DATECD = 2
		! Element = DATE
		!   Description = Beginning Date
		STRING DATEBEGIN = 8
		! Element = DATE
		!   Description = Ending Date
		STRING DATEEND = 8
		! Element =
		!   Description = Description
		STRING DESCR = 30
	END RECORD
