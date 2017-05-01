	!
	! File Layout for: PR.PR_REG_ERNDED on 21-May-01
	!
	! Payroll Earnings and Deduction Register
	!

	RECORD PR_REG_ERNDED_CDD
		! Element =
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = Deduction, noncompensaTion, Memo
		STRING ETYPE = 1
		! Element =
		!   Description = Ernded code
		STRING CODE = 2
		! Element =
		!   Description = Quarter ernded dollars
		GFLOAT QTR_DOLL(3)
		! Element =
		!   Description = Regular hours
		GFLOAT REG_HRS(3)
		! Element =
		!   Description = Premium hours
		GFLOAT PRE_HRS(3)
		! Element =
		!   Description = Units
		GFLOAT UNITS(3)
		! Element =
		!   Description = Update counter
		WORD UPDATE_COUNTER
	END RECORD
