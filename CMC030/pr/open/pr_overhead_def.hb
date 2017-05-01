	!
	! File Layout for: PR.PR_OVERHEAD_DEF on 21-May-01
	!
	! Payroll Overhead Subject File
	!

	RECORD PR_OVERHEAD_DEF_CDD
		! Element =
		!   Description = Overhead key
		STRING OVH_KEY = 6
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING SUBJ_ACCT = 18
		! Element = OPERATION
		!   Description = Operation
		STRING OPER = 8
	END RECORD
