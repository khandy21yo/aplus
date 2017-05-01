	!
	! File Layout for: AP.AP_CDJ on 21-May-01
	!
	! Cash Disbursements Journal
	!

	RECORD AP_CDJ_CDD
		! Element =
		!   Description = Vendor number
		STRING VENNUM = 10
		! Element =
		!   Description = Transaction key
		STRING TRANKEY = 6
		! Element =
		!   Description = Invoice number
		STRING INVNUM = 15
		! Element =
		!   Description = Invoice date
		STRING INVDAT = 8
		! Element =
		!   Description = Invoice amount
		GFLOAT INVAMT
		! Element =
		!   Description = Check amount
		GFLOAT CKAMT
		! Element =
		!   Description = Check number
		STRING CKNUM = 6
		! Element =
		!   Description = Check date
		STRING CKDAT = 8
		! Element =
		!   Description = Check description
		STRING CKDESC = 20
		! Element =
		!   Description = Discount date
		STRING DISCDAT = 8
		! Element =
		!   Description = Discount amount
		GFLOAT DISAMT
		! Element =
		!   Description = Amount discount lost
		GFLOAT DISC_LOST_AMT
		! Element =
		!   Description = Discount lost to account
		STRING DISCLOST_ACCT = 18
		! Element =
		!   Description = Due date
		STRING DUEDAT = 8
		! Element =
		!   Description = Purchase order number
		STRING PONUM = 10
		! Element =
		!   Description = Accounts Payable Account
		STRING AP_ACCT = 18
		! Element =
		!   Description = Cash Account
		STRING CASH_ACCT = 18
	END RECORD
