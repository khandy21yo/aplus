	!
	! File Layout for: AP.AP_PJL on 21-May-01
	!
	! Purchase Journal Line File
	!

	RECORD AP_PJL_CDD
		! Element =
		!   Description =
		STRING TRANKEY = 6
		! Element =
		!   Description = Must be reset
		STRING SLINE = 4
		! Element =
		!   Description =
		STRING PONUM = 10
		! Element =
		!   Description =
		STRING PO_LINE = 4
		! Element =
		!   Description =
		STRING ACCT = 18
		! Element =
		!   Description =
		STRING SUBACC = 10
		! Element =
		!   Description =
		STRING OPERATION = 8
		! Element =
		!   Description =
		GFLOAT UNITS
		! Element =
		!   Description =
		GFLOAT AMOUNT
		! Element =
		!   Description =
		GFLOAT DISCAMT
		! Element =
		!   Description = Flag it either (y/N)
		STRING USE_TAX_FLAG = 1
	END RECORD
