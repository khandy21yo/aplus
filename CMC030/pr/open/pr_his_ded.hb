	!
	! File Layout for: PR.PR_HIS_DED on 21-May-01
	!
	! Payroll Deduction History
	!

	RECORD PR_HIS_DED_CDD
		! Element = EMPNUM
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = DATE
		!   Description = Date
		STRING PR_END_DATE = 8
		! Element =
		!   Description = C-calculated tax, D-deduction
		STRING DTYPE = 1
		! Element =
		!   Description = Tax or deduction code
		STRING CODE = 2
		! Element = AMOUNT
		!   Description = Deduction/Tax amount
		GFLOAT AMOUNT
		! Element =
		!   Description = Tax code
		STRING TAX_CODE = 2
		! Element =
		!   Description = Withholding status
		STRING SSTATUS = 1
		! Element = EXEMPT
		!   Description = Number of exemptions
		WORD EXEMPT
		! Element =
		!   Description = Update flag
		WORD UPDATE_FLAG
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element =
		!   Description = Taxable Basis
		GFLOAT TAXABLE
		! Element =
		!   Description = Reportable basis
		GFLOAT REPORTABLE
		! Element = EXEMPT
		!   Description = Number of exemptions
		WORD ADDEXEMPT
	END RECORD
