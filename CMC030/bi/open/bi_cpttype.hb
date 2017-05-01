	!
	! File Layout for: BI.BI_CPTTYPE on 21-May-01
	!
	! CPT Type and Account Number Table
	!

	RECORD BI_CPTTYPE_CDD
		! Element = CPTTYPE
		!   Description = CPT Type
		STRING CPTTYPE = 2
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
