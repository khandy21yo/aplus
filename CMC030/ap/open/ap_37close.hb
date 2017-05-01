	!
	! File Layout for: AP.AP_37CLOSE on 30-Jun-00
	!
	! AP Closed Information
	!

	RECORD AP_37CLOSE_CDD
		! Element =
		!   Description =
		STRING VENNUM = 10
		! Element =
		!   Description =
		STRING TRANKEY = 6
		! Element =
		!   Description =
		STRING TRANKEY_DATE = 8
		! Element =
		!   Description =
		STRING INVNUM = 15
		! Element =
		!   Description =
		STRING INVDAT = 8
		! Element =
		!   Description =
		GFLOAT INVAMT
		! Element =
		!   Description =
		STRING CODE_1099 = 2
		! Element =
		!   Description =
		GFLOAT AMT_1099
		! Element =
		!   Description =
		STRING USE_JOB_NUM = 10
		! Element =
		!   Description =
		GFLOAT USE_AMT
		! Element =
		!   Description =
		STRING DISCDAT = 8
		! Element =
		!   Description =
		GFLOAT DISAMT
		! Element =
		!   Description =
		STRING DUEDAT = 8
		! Element =
		!   Description =
		STRING PONUM = 10
		! Element =
		!   Description =
		STRING AP_ACCT = 18
		! Element =
		!   Description =
		STRING CASH_ACCT = 18
		! Element =
		!   Description =
		STRING CKNUM = 6
		! Element =
		!   Description =
		STRING CKDAT = 8
		! Element =
		!   Description =
		STRING CKDESC = 20
		! Element =
		!   Description =
		GFLOAT CKAMT
		! Element =
		!   Description = (MMYYYY)
		STRING UPDATED = 8
		! Element =
		!   Description = (MMYYYY)
		STRING CLOSEDATE = 6
		! Element =
		!   Description = Selected for payment (Y/N)
		STRING SELECTED = 1
		! Element =
		!   Description =
		STRING BATCH = 6
	END RECORD
