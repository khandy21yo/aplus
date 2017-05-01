	!
	! File Layout for: PR.PR_REG_TAXWH on 21-May-01
	!
	! Payroll Tax Register
	!

	RECORD PR_REG_TAXWH_CDD
		! Element =
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = FW, FI, SW, SX, CW, DW, EX
		STRING TTYPE = 2
		! Element =
		!   Description = Used if type = SW,SX,CW,DW,EX
		STRING CODE = 2
		! Element =
		!   Description = Quarterly wages
		GFLOAT QTRWAG(3)
		! Element =
		!   Description = Quarterly taxes witheld
		GFLOAT QTRTAX(3)
		! Element =
		!   Description = Weeks worked during quarter
		WORD WKWRK(3)
		! Element =
		!   Description = Batch number from update
		WORD UPDATE_COUNTER
	END RECORD
