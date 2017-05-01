	!
	! File Layout for: WP.WP_BUYOFF on 21-May-01
	!
	! Manufacturing WIP Buyoff Journal Header File
	!

	RECORD WP_BUYOFF_CDD
		! Element =
		!   Description = WIP Job Number
		STRING JOB = 10
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCT = 18
		! Element = LOCATION
		!   Description = From Location number
		STRING TOLOCATION = 4
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
	END RECORD
