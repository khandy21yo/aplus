	!
	! File Layout for: AP.AP_1099_YYYY on 21-May-01
	!
	! 1099 Annual Work File
	!

	RECORD AP_1099_YYYY_CDD
		! Element =
		!   Description = Vendor Number
		STRING VENNUM = 10
		! Element =
		!   Description = Code
		STRING CODE = 2
		! Element =
		!   Description = Transaction key
		STRING TRANKEY = 6
		! Element =
		!   Description = Invoice number
		STRING INVNUM = 15
		! Element =
		!   Description = Invoice Date
		STRING INVDAT = 8
		! Element =
		!   Description = Check Number
		STRING CKNUM = 6
		! Element =
		!   Description = Check date
		STRING CKDAT = 8
		! Element =
		!   Description = 1099 Amount
		GFLOAT AMT1099
		! Element =
		!   Description = Check Amount
		GFLOAT CKAMT
	END RECORD
