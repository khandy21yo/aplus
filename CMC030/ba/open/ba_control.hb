	!
	! File Layout for: BA.BA_CONTROL on 21-May-01
	!
	! Billing to Agency Control File
	!

	RECORD BA_CONTROL_CDD
		! Element =
		!   Description = Last Period Closed
		WORD LASTPERCLOSE
		! Element =
		!   Description = Year closed
		STRING YEAR = 4
		! Element = INVOICE
		!   Description = Invoice number
		STRING INV_NUM = 8
		! Element =
		!   Description = Billing Number
		STRING BILLNUM = 10
	END RECORD
