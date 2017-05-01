	!
	! File Layout for: UTL.UTL_REPORT on 21-May-01
	!
	! Utility Report File
	!

	RECORD UTL_REPORT_CDD
		! Element =
		!   Description = System name
		STRING SYSTEM = 6
		! Element =
		!   Description = Subsystem name
		STRING SUBSYS = 6
		! Element =
		!   Description = report number
		STRING REPNUM = 6
		! Element =
		!   Description = Description
		STRING REPDES = 30
		! Element =
		!   Description = Program device
		STRING PRODEV = 32
		! Element =
		!   Description = Program name
		STRING PRONAM = 40
		! Element =
		!   Description = Can report be spooled
		STRING CANSPOOL = 1
		! Element =
		!   Description = Can it be displayed
		STRING CANDISP = 1
		! Element =
		!   Description = Can it go to a device
		STRING CANDEV = 1
		! Element =
		!   Description = Can it go to a file
		STRING CANFILE = 1
		! Element =
		!   Description = Can it run detached
		STRING CANDET = 1
		! Element =
		!   Description = Report Date used (Y/N)
		STRING REPYN = 1
		! Element =
		!   Description = Description of options
		STRING DESCR(9) = 20
		! Element =
		!   Description = Type of option
		STRING OPTTYPE(9) = 1
		! Element =
		!   Description = Length of option
		WORD OPTLEN(9)
		! Element =
		!   Description = Valid items in options
		STRING VALID(9) = 20
		! Element =
		!   Description = Option data required?
		STRING REQUIRE(9) = 1
		! Element =
		!   Description = Spooler name
		STRING SPOOL = 32
		! Element =
		!   Description = Option data
		STRING OPTDEF(9) = 20
		! Element =
		!   Description = Default output file/device name
		STRING DEFOUT = 20
		! Element =
		!   Description = Printer type groups
		STRING ITEMGROUP(9) = 2
		! Element =
		!   Description = Defaults for printer groups
		STRING ITEM(9) = 6
		! Element =
		!   Description = Program to chain to
		STRING CHAINTO = 20
		! Element =
		!   Description = Printer type
		STRING PRINTTYPE = 8
		! Element =
		!   Description = Last run date
		STRING LASTRUNDATE = 8
		! Element =
		!   Description = Last run time
		STRING LASTRUNTIME = 6
		! Element =
		!   Description = Base run date
		STRING BASERUNDATE = 8
		! Element =
		!   Description = Run frequency
		STRING RUNFREQ = 2
		! Element =
		!   Description = Report Width in Characters
		WORD REPWID
		! Element =
		!   Description = Spooler From Name
		STRING SPOOLFORM = 20
	END RECORD
