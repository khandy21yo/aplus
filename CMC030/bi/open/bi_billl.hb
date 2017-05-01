	!
	! File Layout for: BI.BI_BILLL on 21-May-01
	!
	! Insurance Billing Journal Lines
	!

	RECORD BI_BILLL_CDD
		! Element = INSURED
		!   Description = Insured number
		STRING INSURED = 10
		! Element = PATIENT
		!   Description = Patient Number
		STRING PATIENT = 10
		! Element = DATE
		!   Description = Service Date (YYYYMMDD)
		STRING SERVDATE = 8
		! Element = CPT
		!   Description = Current Procedural Terminology Code
		STRING CPT = 5
		! Element =
		!   Description = CPT Description
		STRING DESCRIPTION = 40
		! Element = DIAGNOSIS
		!   Description = Diagnosis Code
		STRING DIAGNOSIS = 6
		! Element =
		!   Description = Service time (in hours)
		GFLOAT LENTH
		! Element =
		!   Description = Time Multiplier
		WORD MULTIPLIER
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
	END RECORD
