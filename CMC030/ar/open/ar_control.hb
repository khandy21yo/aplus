	!
	! File Layout for: AR.AR_CONTROL on 21-May-01
	!
	! Accounts Receivable Control File
	!

	RECORD AR_CONTROL_CDD
		! Element = ACCOUNT
		!   Description = AR account
		STRING AR_ACCT = 18
		! Element =
		!   Description = Retention cycle
		WORD RETAIN
		! Element =
		!   Description = Last Period Closed
		WORD LASTPERCLOSE
		! Element =
		!   Description = Current Year
		STRING YEAR = 4
		! Element =
		!   Description = Closing flag 0-nostate,1-close,2-reset
		STRING CLOSEFLAG = 1
		! Element =
		!   Description = Number of days in a period
		WORD AGEPER(4)
		! Element =
		!   Description = Names of periods
		STRING AGENAM(4) = 16
		! Element =
		!   Description = Customer Title (Cust, client, patient)
		STRING CTITLE = 16
		! Element =
		!   Description = Default Method for AR
		STRING METHOD = 1
	END RECORD
