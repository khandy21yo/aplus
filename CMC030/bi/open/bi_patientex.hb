	!
	! File Layout for: BI.BI_PATIENTEX on 21-May-01
	!
	! Extra Information for Patients
	!

	RECORD BI_PATIENTEX_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = DATE
		!   Description = Birth Date (YYYYMMDD)
		STRING BDAY = 8
	END RECORD
