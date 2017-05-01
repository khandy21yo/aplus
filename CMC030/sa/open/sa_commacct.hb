	!
	! File Layout for: SA.SA_COMMACCT on 21-May-01
	!
	! Commision GL Accounts Table
	!

	RECORD SA_COMMACCT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = SALTYPE
		!   Description = Salesman Type
		STRING SALTYPE = 2
		! Element = ACCOUNT
		!   Description = Commision Expanse GL Account
		STRING EXPACCT = 18
		! Element = ACCOUNT
		!   Description = Commision Payable GL Account
		STRING PAYACCT = 18
	END RECORD
