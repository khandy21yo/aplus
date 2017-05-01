	!
	! File Layout for: WP.WP_REQJOUR on 21-May-01
	!
	! Requisition Journal Header File
	!

	RECORD WP_REQJOUR_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element =
		!   Description = Job Line NUmber
		STRING LLINE = 4
		! Element = DATE
		!   Description = Requisition Date (YYYYMMDD)
		STRING REQDATE = 8
		! Element = NOTES
		!   Description = Notes
		STRING NOTES(1) = 40
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
	END RECORD
