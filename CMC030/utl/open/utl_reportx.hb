	!
	! File Layout for: UTL.UTL_REPORTX on 21-May-01
	!
	! Report Settings Structure
	!

	RECORD UTL_REPORTX_CDD
		! Element =
		!   Description = report number
		STRING REPNUM = 6
		! Element =
		!   Description = Program device
		STRING PRODEV = 32
		! Element =
		!   Description = Program name
		STRING PRONAM = 40
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
		!   Description = Starting page
		LONG STARTP
		! Element =
		!   Description = Ending page
		LONG ENDP
		! Element =
		!   Description = Copies
		LONG COPIES
		! Element =
		!   Description = Page length
		LONG PAGELEN
		! Element =
		!   Description = Report width
		LONG REPWIDTH
		! Element =
		!   Description = Report date
		STRING REPDATE = 32
		! Element =
		!   Description = Print report date on report (yn)
		STRING REPYN = 1
		! Element =
		!   Description = Output flag 1 - Display 2 - Spool 3 - Ou
		LONG PRINTTO
		! Element =
		!   Description = Autoscroll flag
		LONG AUTOSCROLL
		! Element =
		!   Description = Printer type
		STRING PRINTTYPE = 8
		! Element =
		!   Description = Printer initilization string
		STRING PRINTINIT = 64
		! Element =
		!   Description = Printer finilization string
		STRING PRINTFINISH = 64
		! Element =
		!   Description = Control string to go to next page
		STRING NEXTPAGE = 64
		! Element =
		!   Description = Output to local printer
		STRING TOLOCAL = 32
		! Element =
		!   Description = Output to screen again
		STRING TOSCREEN = 32
		! Element =
		!   Description = Next program to run
		STRING NEXTRUN = 64
		! Element =
		!   Description = Channel for report output
		LONG CHAN
		! Element =
		!   Description = Exit status
		LONG STAT
		! Element =
		!   Description = Page number
		LONG PAGENO
		! Element =
		!   Description = Line number
		LONG LINENO
		! Element =
		!   Description = Starting date
		STRING SDATE = 10
		! Element =
		!   Description = Starting time
		STRING STIME = 10
		! Element =
		!   Description = Window for selection menu
		LONG WINDOW
		! Element =
		!   Description = Cannot detach flag
		LONG DETACH
		! Element =
		!   Description = Spooler From Name
		STRING SPOOLFORM = 20
		! Element =
		!   Description = Spaces to add to left margin
		LONG OFFSET
		! Element = TIME
		!   Description = Run After Time (HHMMSS)
		STRING AFTERTIME = 6
		! Element = YESNO
		!   Description = Run Background?
		STRING BACKGROUND = 1
	END RECORD
