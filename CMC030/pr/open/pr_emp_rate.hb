	!
	! File Layout for: PR.PR_EMP_RATE on 21-May-01
	!
	! Payroll Employee Rate File
	!

	RECORD PR_EMP_RATE_CDD
		! Element =
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = OPERATION
		!   Description = Operation
		STRING OPER = 8
		! Element =
		!   Description = Effective date
		STRING EFFDAT = 8
		! Element =
		!   Description = Hourly,Salary,Piece,Mile
		STRING RATE_TYPE = 1
		! Element =
		!   Description = Rate code
		STRING RATE_CDE = 2
		! Element =
		!   Description = Hourly Rate
		GFLOAT HOUR_RATE
		! Element =
		!   Description = Piece Salary
		GFLOAT PIECE_RATE
		! Element =
		!   Description = Overtime Percentage
		WORD FACTOR
		! Element =
		!   Description = Standard efficiency rating
		GFLOAT STDEFF
		! Element = DATE
		!   Description = Evaluation Date
		STRING EVAL_DATE = 8
	END RECORD
