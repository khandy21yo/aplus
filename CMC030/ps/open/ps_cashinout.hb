	!
	! File Layout for: PS.PS_CASHINOUT on 21-May-01
	!
	! Cash in and out of the cash Register
	!

	RECORD PS_CASHINOUT_CDD
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING CASHDATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING CASHTIME = 6
		! Element =
		!   Description = Cash Amount
		GFLOAT AMOUNT
		! Element = NOTES
		!   Description = Notes
		STRING NOTES = 40
		! Element = OPERATOR
		!   Description = Written By
		STRING OPERATOR = 10
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
