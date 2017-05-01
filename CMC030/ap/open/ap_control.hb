	!
	! File Layout for: AP.AP_CONTROL on 21-May-01
	!
	! Accounts Payable Control File
	!

	RECORD AP_CONTROL_CDD
		! Element =
		!   Description =
		STRING AP_ACCT = 18
		! Element =
		!   Description =
		STRING DISCLOST_ACCT = 18
		! Element =
		!   Description =
		STRING CASH_ACCT = 18
		! Element =
		!   Description =
		STRING LAST_TRANKEY = 6
		! Element =
		!   Description =
		STRING LAST_CKNUM = 6
		! Element =
		!   Description = Retention cycle
		WORD RETAIN
		! Element =
		!   Description = Last period closed
		WORD LASTPERCLOSE
		! Element =
		!   Description = Retain 1099 history only (Y/N)
		STRING RETAIN_1099_ONLY = 1
		! Element =
		!   Description = Year
		STRING YEAR = 4
		! Element =
		!   Description = Closing flag 0-nostate,1-close,2-reset
		STRING CLOSEFLAG = 1
	END RECORD
