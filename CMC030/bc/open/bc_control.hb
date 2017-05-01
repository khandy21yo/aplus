	!
	! File Layout for: BC.BC_CONTROL on 21-May-01
	!
	! Billing to Customer Control File
	!

	RECORD BC_CONTROL_CDD
		! Element =
		!   Description = Last period closed
		WORD LASTPERCLOSE
		! Element =
		!   Description = Year closed
		STRING YEAR = 4
		! Element = INVOICE
		!   Description = Invoice number
		STRING INV_NUM = 8
	END RECORD
