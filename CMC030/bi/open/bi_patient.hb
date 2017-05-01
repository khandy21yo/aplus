	!
	! File Layout for: BI.BI_PATIENT on 21-May-01
	!
	! Patient File
	!

	RECORD BI_PATIENT_CDD
		! Element = INSURED
		!   Description = Insured
		STRING INSURED = 10
		! Element = PATIENT
		!   Description = Patient Number
		STRING PATIENT = 10
		! Element = FAMRELAT
		!   Description = Family relation
		STRING FAMRELAT = 1
		! Element =
		!   Description = Insurance carrier
		STRING INSURANCE = 10
		! Element =
		!   Description = Insurance Group Number
		STRING GROUPNO = 10
	END RECORD
