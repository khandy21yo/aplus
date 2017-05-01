1	!
	! Dump chart of accounts in to an ascii file
	!
	!++
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_ASCI_CUSTOM/LINE
	!	$ LINK/EXEC:AR_EXE AR_ASCI_CUSTOM,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_ASCI_CUSTOM.OBJ;*
	!
	! History:
	!
	!	07/26/2000 - Kevin Handy
	!		Make it available for comma seperated files
	!
	!	07/19/2001 - Kevin Handy
	!		Added compile lines.
	!		Switch to tabbed file
	!		Use "ar_35custom.hb" instead of CDD version.
	!		Turn off "only location 007"
	!--

 !	%include %from %cdd "cdd$top.ar.ar_35custom"
	%include "source:[ar.open]ar_35custom.hb"
	map (ar_35custom) ar_35custom_cdd ar_35custom

 !	fmt% = 132%
	fmt% = 0%

	def fnformat$(a$, fmtflg%)
		select fmtflg%
		case 0%
			fnformat$ = EDIT$(a$, 132%)
		case 132%
			fnformat$ = '"' + EDIT$(a$, 132%) + '"'
		end select
	fnend

	select fmt%
	case 132%
		fmtflag$ = ", "
	case else
		fmtflag$ = "	"
	end select

1000	%include "source:[ar.open]ar_35custom.opn"

1010	open "arcustom.txt" for output as file 1%, &
		recordsize 511%

	text$ = "CUSNUM" + fmtflag$ + &
		"CUSNAM" + fmtflag$ + &
		"TTYPE" + fmtflag$ + &
		"CATEGORY" + fmtflag$ + &
		"BDATE" + fmtflag$ + &
		"SSTATUS" + fmtflag$ + &
		"EDATE" + fmtflag$ + &
		"ADD1" + fmtflag$ + &
		"ADD2" + fmtflag$ + &
		"ADD3" + fmtflag$ + &
		"CITY" + fmtflag$ + &
		"STATE" + fmtflag$ + &
		"ZIP" + fmtflag$ + &
		"COUNTRY" + fmtflag$ + &
		"COUNTY" + fmtflag$ + &
		"PHONE" + fmtflag$ + &
		"METHOD" + fmtflag$ + &
		"STMTFLG" + fmtflag$ + &
		"ALPSRT" + fmtflag$ + &
		"SERCHRG" + fmtflag$ + &
		"TAXCODE" + fmtflag$ + &
		"TAXEXEMP" + fmtflag$ + &
		"LOCATION" + fmtflag$ + &
		"TERMS" + fmtflag$ + &
		"CARRIER" + fmtflag$ + &
		"SALESMAN" + fmtflag$ + &
		"CREDITLIM" + fmtflag$ + &
		"DISCOUNT" + fmtflag$ + &
		"BACKORDER" + fmtflag$ + &
		"TAXFLAG"

	print #1%, text$

2000	reset #ar_35custom.ch%

 !	PRINT "WARNING: LOCATION 007 ONLY"

2100	WHEN ERROR IN
		get #ar_35custom.ch%, regardless
	use
		CONTINUE 2900 if err = 11%
		print "ERROR: "; ERR
		STOP
	end when

 !	GOTO 2100 IF AR_35CUSTOM::LOCATION <> "007"

	text$ = fnformat$(AR_35CUSTOM::CUSNUM, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CUSNAM, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TTYPE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CATEGORY, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::BDATE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::SSTATUS, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::EDATE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD1, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD2, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD3, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CITY, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::STATE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ZIP, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::COUNTRY, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::COUNTY, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::PHONE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::METHOD, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::STMTFLG, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ALPSRT, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::SERCHRG, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TAXCODE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TAXEXEMP, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::LOCATION, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TERMS, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CARRIER, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::SALESMAN, fmt%) + fmtflag$ + &
		NUM1$(AR_35CUSTOM::CREDITLIM) + fmtflag$ + &
		NUM1$(AR_35CUSTOM::DISCOUNT) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::BACKORDER, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TAXFLAG, fmt%)

	print #1%, text$

	goto 2100

2900	!

32767	end
