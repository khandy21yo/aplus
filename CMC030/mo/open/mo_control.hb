	!
	! File Layout for: MO.MO_CONTROL on 21-May-01
	!
	! Manufacture Order Controling File
	!

	RECORD MO_CONTROL_CDD
		! Element = ORDNUM
		!   Description = Control Order Number
		STRING ORDNUM = 10
		! Element = DATE
		!   Description = Last Close/Purge Date (YYYYMMDD)
		STRING PURGDATE = 8
		! Element =
		!   Description = Activity status
		STRING STATUS_FLAG = 1
		! Element = INVOICE
		!   Description = Invoice number
		STRING LAST_INV = 8
	END RECORD
