	!
	! File Layout for: BT.BT_CONTROL on 21-May-01
	!
	! Billing Tuition Control File
	!

	RECORD BT_CONTROL_CDD
		! Element =
		!   Description = Last period closed
		WORD LASTPERCLOSE
		! Element = YEAR
		!   Description = Physical year (YYYY)
		STRING YEAR = 4
		! Element = INVOICE
		!   Description = Invoice number
		STRING INV_NUM = 8
		! Element = FLAG
		!   Description = Flag
		STRING FLAG = 1
	END RECORD
