	!
	! File Layout for: CK.CK_CONTROLACC on 21-May-01
	!
	! Check Account Control File
	!

	RECORD CK_CONTROLACC_CDD
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Bank Account
		STRING BANK_ACCT = 6
		! Element =
		!   Description = Start Check Number
		STRING STARTCK = 6
		! Element =
		!   Description = End Check Number
		STRING ENDCK = 6
		! Element =
		!   Description = Bank Number
		STRING BANK_NUM = 20
	END RECORD
