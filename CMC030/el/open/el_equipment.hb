	!
	! File Layout for: EL.EL_EQUIPMENT on 21-May-01
	!
	! Equipment Ledger Descriptons Master File
	!

	RECORD EL_EQUIPMENT_CDD
		! Element =
		!   Description = Subject type for equipment "E"
		STRING SUBJECT = 1
		! Element = SUBACCT
		!   Description = Equipment Number
		STRING EQNUM = 10
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Equipment Type
		STRING TTYPE = 2
		! Element = CLASS
		!   Description = Equipment Class
		STRING CLASS = 4
		! Element = DATE
		!   Description = Creation Date
		STRING BDATE = 8
		! Element =
		!   Description = Equipment Status
		STRING SSTATUS = 1
		! Element = DATE
		!   Description = Closed Date
		STRING EDATE = 8
		! Element = LOCATION
		!   Description = Equipment Location
		STRING LOCATION = 4
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element = REFNO
		!   Description = Reference Number
		STRING REFNO = 16
	END RECORD
