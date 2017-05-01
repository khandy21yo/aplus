	!
	! File Layout for: PR.PR_EMP_ACCRUAL on 21-May-01
	!
	! Accrual Definition Header
	!

	RECORD PR_EMP_ACCRUAL_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = Accrual (Pay) Code
		STRING ATYPE = 2
		! Element =
		!   Description = Hours Unavailable
		GFLOAT HOURSUNA
		! Element =
		!   Description = Hours Available
		GFLOAT HOURSAVA
		! Element =
		!   Description = Dollars Unavailable
		GFLOAT DOLLARUNA
		! Element =
		!   Description = Dollars Available
		GFLOAT DOLLARAVA
		! Element =
		!   Description = When to make available
		STRING AVAILFLAG = 1
		! Element = DATE
		!   Description = Date (YYYYMMDD) just in case
		STRING AVAILDATE = 8
		! Element = YESNO
		!   Description = Post Accrual to GL?
		STRING GLFLAG = 1
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
	END RECORD
