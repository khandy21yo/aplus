	!
	! File Layout for: AR.AR_SJH on 21-May-01
	!
	! Sales Journal Header
	!

	RECORD AR_SJH_CDD
		! Element = INVNUM
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Transaction type 01 - Invoice 02 - Cash
		STRING TRATYP = 2
		! Element =
		!   Description = Transaction date
		STRING TRADAT = 8
		! Element =
		!   Description = Transaction amount
		GFLOAT AMOUNT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ARACCT = 18
		! Element =
		!   Description = Receipt number
		STRING RECNUM = 8
		! Element =
		!   Description = Check number
		STRING CHECK = 6
		! Element =
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element =
		!   Description = Description
		STRING DESCR = 26
		! Element = DATE
		!   Description = Due Date (YYYYMMDD)
		STRING DUEDATE = 8
		! Element = DATE
		!   Description = Discount Date (YYYYMMDD)
		STRING DISCOUNTDATE = 8
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
	END RECORD
