	!
	! File Layout for: PR.PR_UNPN_DEF on 21-May-01
	!
	! Payroll Union Pension Definition File
	!

	RECORD PR_UNPN_DEF_CDD
		! Element =
		!   Description = Union Pension Code
		STRING CODE = 2
		! Element =
		!   Description = Union Pension Type
		STRING DTYPE = 2
		! Element =
		!   Description = Description
		STRING DESCR = 30
		! Element =
		!   Description = Paid by Whom
		STRING PAID_BY = 1
		! Element =
		!   Description = Employee Rate
		GFLOAT EMPE_RATE
		! Element =
		!   Description = Employer Rate
		GFLOAT EMPR_RATE
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING LIA_ACCT = 18
		! Element =
		!   Description = Basis
		STRING BASIS = 1
		! Element =
		!   Description = Deduction Code
		STRING DED_CODE = 2
	END RECORD
