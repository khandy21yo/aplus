	!
	! File Layout for: PR.PR_TAX_PKG on 21-May-01
	!
	! Payroll Tax Package Table
	!

	RECORD PR_TAX_PKG_CDD
		! Element =
		!   Description = Tax package number
		STRING TAX_PKG = 2
		! Element =
		!   Description = Datatype. FW=Fed, SW=state, etc.
		STRING STTYPE = 2
		! Element =
		!   Description = State,city, county, school code
		STRING CODE = 2
	END RECORD
