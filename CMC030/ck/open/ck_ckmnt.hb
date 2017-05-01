	!
	! File Layout for: CK.CK_CKMNT on 21-May-01
	!
	! Check Maintainance
	!

	RECORD CK_CKMNT_CDD
		! Element =
		!   Description = Bank Account
		STRING BANK_ACCT = 6
		! Element = CHECK
		!   Description = Check number
		STRING CKNUM = 6
		! Element =
		!   Description = Entry Flag
		STRING ETYPE = 1
		! Element =
		!   Description = From GL (G), or Bank (B)
		STRING STYPE = 1
		! Element = DATE
		!   Description = Check Date
		STRING CKDAT = 8
		! Element =
		!   Description = Check Amount
		GFLOAT CKAMT
		! Element = PERIOD
		!   Description = GL Fiscal year (YYYY) and Cycle (PP)
		STRING GLDATE = 6
	END RECORD
