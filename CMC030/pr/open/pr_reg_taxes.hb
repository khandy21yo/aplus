	!
	! File Layout for: PR.PR_REG_TAXES on 21-May-01
	!
	! Payroll Tax Register
	!

	RECORD PR_REG_TAXES_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = FW,FI,SW,SX,SU,DW,EW,etc.
		STRING TTYPE = 2
		! Element =
		!   Description = Tax code (Used if type=SW,SX,CW,DW,EX)
		STRING CODE = 2
		! Element =
		!   Description = Quarterly Taxes Witheld
		GFLOAT TAX(3)
		! Element =
		!   Description = Quarterly Reportable Wages
		GFLOAT REPORTABLE(3)
		! Element =
		!   Description = Quarterly Taxable Wages
		GFLOAT TAXABLE(3)
		! Element =
		!   Description = Weeks Worked during Quarter
		WORD WKWRK(3)
		! Element = UPCOUNT
		!   Description = Update counter
		WORD UPDATE_COUNTER
	END RECORD
