	!
	! File Layout for: MO.MO_MAKE on 21-May-01
	!
	! Make Master Table File
	!

	RECORD MO_MAKE_CDD
		! Element =
		!   Description = Dealer Model of Make
		STRING MAKE = 10
		! Element =
		!   Description = Make Description
		STRING DESCR = 40
		! Element =
		!   Description = Year for Make YYYY
		STRING YEAR = 4
		! Element = MTYPE
		!   Description = Type of Make
		STRING MTYPE = 2
		! Element =
		!   Description = Size of the Make
		STRING MSIZE = 4
		! Element = CLASS
		!   Description = Class
		STRING CLASS = 4
		! Element =
		!   Description = Cut Tubing in inches ie: xx x/x
		STRING TUBING = 8
		! Element =
		!   Description = Front Slant in Degrees ie: 3.5
		STRING SLANT = 8
		! Element =
		!   Description = Overall Length of Cab in inches
		STRING OVERALL = 8
		! Element =
		!   Description = Indicator for Narrow Front (Y/N)
		STRING NFRONT = 1
		! Element =
		!   Description = Indicator for Narrow Back (Y/N)
		STRING NBACK = 1
	END RECORD
