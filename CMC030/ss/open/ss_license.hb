	!
	! File Layout for: SS.SS_LICENSE on 21-May-01
	!
	! Support System Customer Licensing File
	!

	RECORD SS_LICENSE_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = System License Number
		STRING LICENSE_NUM = 10
		! Element = DATE
		!   Description = License Expiration Date (YYYYMMDD)
		STRING EXPIR_DATE = 8
		! Element =
		!   Description = Product (System) Make Number
		STRING MAKE = 10
		! Element =
		!   Description = Product (System) Model Number
		STRING MODEL = 10
		! Element = SERIAL
		!   Description = Product (System) Serial number
		STRING SERIAL_NUM = 20
		! Element =
		!   Description = Operating System
		STRING OPER_SYS = 6
		! Element =
		!   Description = Version Number
		STRING VERSION = 6
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE_NUM = 10
		! Element = PHONE_EXTENSION
		!   Description = Phone Extension
		STRING PHONE_EXT = 4
	END RECORD
