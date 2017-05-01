	!
	! File Layout for: SB.SB_SUBACCOUNT on 21-May-01
	!
	! Subaccount Description File
	!

	RECORD SB_SUBACCOUNT_CDD
		! Element =
		!   Description = Subject type
		STRING SUBJECT = 1
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCOUNT = 10
		! Element = DESCRIPTION6
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Type
		STRING TTYPE = 2
		! Element = CLASS
		!   Description = Class
		STRING CLASS = 4
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING BDATE = 8
		! Element =
		!   Description = Status
		STRING SSTATUS = 1
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING EDATE = 8
		! Element =
		!   Description = Extra Fields for a Subaccount
		STRING EXTRAFIELDS = 110
	END RECORD
