	!
	! File Layout for: PR.PR_EMP_STATUS on 21-May-01
	!
	! Payroll Employee Status File
	!

	RECORD PR_EMP_STATUS_CDD
		! Element =
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = Datatype. FW=Fed WH, SW=State WH, etc.
		STRING STTYPE = 2
		! Element =
		!   Description = State, city, county, school code
		STRING CODE = 2
		! Element =
		!   Description = Withholding status
		STRING STSTATUS = 1
		! Element = EXEMPT
		!   Description = Number of exemptions
		WORD EXEMPT
		! Element = EXEMPT
		!   Description = Additional Number of exemptions
		WORD ADDEXEMPT
	END RECORD
