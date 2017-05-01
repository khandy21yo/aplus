	!
	! File Layout for: UTL.UTL_STRING_PRINT on 21-May-01
	!
	! String Print Definitions
	!

	RECORD UTL_STRING_PRINT_CDD
		! Element =
		!   Description = System For Reports (AR,PR,GL...)
		STRING SYSTEM = 6
		! Element =
		!   Description = User defined grouping
		STRING GROUPING = 6
		! Element =
		!   Description = Report sequence number
		STRING REPSEQ = 6
		! Element =
		!   Description = Name to select by
		STRING TITLES = 20
		! Element =
		!   Description = Number of report in report file
		STRING REPNUM = 6
		! Element =
		!   Description = Settings Flag (Ignore,Set,Query)
		STRING FLAGS(9) = 1
		! Element =
		!   Description = For query, code name to ask for
		STRING CODES(9) = 6
		! Element =
		!   Description = For Query, title. For Set, value
		STRING DESCRS(9) = 20
		! Element =
		!   Description = Output Device
		STRING OUTDEV = 20
	END RECORD
