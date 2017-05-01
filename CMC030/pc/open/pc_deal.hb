	!
	! File Layout for: PC.PC_DEAL on 02-Oct-01
	!
	! Special deals to customers
	!

	RECORD PC_DEAL_CDD
		! Element =
		!   Description = Deal number
		STRING DEAL =   20
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSTOMER = 10
		! Element = DATE
		!   Description = Start Date (YYYYMMDD)
		STRING STARTD = 8
		! Element = DATE
		!   Description = End Date (YYYYMMDD)
		STRING ENDD = 8
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCR =   40
	END RECORD
