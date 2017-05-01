1	!
	! Dump chart of accounts in to an ascii file
	!
	!++
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_ASCI_CUSTOM_FJ/LINE
	!	$ LINK/EXEC:AR_EXE AR_ASCI_CUSTOM_FJ,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_ASCI_CUSTOM_FJ.OBJ;*
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

	%include "source:[ar.open]ar_contact.hb"
	map (ar_contact) ar_contact_cdd ar_contact


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

1005	%include "source:[Ar.open]Ar_contact.opn"

1010	open "arcustom.txt" for output as file 1%, &
		recordsize 511%

	text$ = "CUSNUM" + fmtflag$ + &
		"CUSNAM" + fmtflag$ + &
		"ADD1" + fmtflag$ + &
		"ADD2" + fmtflag$ + &
		"ADD3" + fmtflag$ + &
		"CITY" + fmtflag$ + &
		"STATE" + fmtflag$ + &
		"ZIP" + fmtflag$ + &
		"CONTACT" + fmtflag$ + &
		"PHONE" + fmtflag$ + &
		"FAX" + fmtflag$ + &
		"CRLIMIT" + fmtflag$ + &
		"TERMS"

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

2110	contact$ = ""
	memo$ = ""
	terms$ = ""

	when error in
		get #ar_contact.ch%, &
			key #0% eq AR_35custom::cusNUM, &
			regardless
	use
		continue 2300
	end when

2120	when error in
		get #ar_contact.ch%, &
			regardless
	use
		continue 2300
	end when

	if ar_contact::cusnum = Ar_35custom::cusNUM
	then
		if edit$(ar_contact::CONTACT_NAME, -1) = "FAX"
		then
			fax$ = ar_contact::PHONE
		else
			contact$ = ar_contact::CONTACT_NAME
		end if

		goto 2120
	end if

2300	text$ = fnformat$(AR_35CUSTOM::CUSNUM, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CUSNAM, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD1, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD2, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ADD3, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::CITY, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::STATE, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::ZIP, fmt%) + fmtflag$ + &
		fnformat$(CONTACT$, fmt%) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::PHONE, fmt%) + fmtflag$ + &
		fnformat$(FAX$, fmt%) + fmtflag$ + &
		NUM1$(AR_35CUSTOM::CREDITLIM) + fmtflag$ + &
		fnformat$(AR_35CUSTOM::TERMS, fmt%)

	print #1%, text$

	goto 2100

2900	!

32767	end
