1	!
	! Dump AP vendor file in to an ascii file
	!
	!	07/07/2003 - Kevin Handy
	!		Forde Johnson Version
	!

 !	%include %from %cdd "cdd$top.AP.AP_VENDOR"
	%include "source:[ap.open]ap_vendor.hb"
	map (AP_VENDOR) AP_VENDOR_cdd AP_VENDOR

	%include "source:[ap.open]ap_contact.hb"
	map (ap_contact) ap_contact_cdd ap_contact

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

1000	%include "source:[AP.open]AP_VENDOR.opn"

1005	%include "source:[AP.open]AP_contact.opn"

1010	open "APvendor.txt" for output as file 1%, &
		recordsize 511%

	text$ = fnformat$("VENNUM", fmt%) + fmtflag$ + &
		fnformat$("VENNAM", fmt%) + fmtflag$ + &
		fnformat$("ADD1", fmt%) + fmtflag$ + &
		fnformat$("ADD2", fmt%) + fmtflag$ + &
		fnformat$("CITY", fmt%) + fmtflag$ + &
		fnformat$("STATE", fmt%) + fmtflag$ + &
		fnformat$("ZIP", fmt%) + fmtflag$ + &
		fnformat$("CONTACT", fmt%) + fmtflag$ + &
		fnformat$("PHONE", fmt%) + fmtflag$ + &
		fnformat$("FAX", fmt%) + fmtflag$ + &
		fnformat$("MEMO", fmt%) + fmtflag$ + &
		fnformat$("TERMS", fmt%)

	print #1%, text$

2000	reset #AP_VENDOR.ch%

2100	get #AP_VENDOR.ch%, regardless


2110	contact$ = ""
	memo$ = ""
	terms$ = ""

	when error in
		get #ap_contact.ch%, &
			key #0% eq AP_VENDOR::VENNUM, &
			regardless
	use
		continue 2300
	end when

2120	when error in
		get #ap_contact.ch%, &
			regardless
	use
		continue 2300
	end when

	if ap_contact::cusnum = AP_VENDOR::VENNUM
	then
		if edit$(ap_contact::CONTACT_NAME, -1) = "FAX"
		then
			fax$ = ap_contact::PHONE
		else
			contact$ = ap_contact::CONTACT_NAME
		end if

		goto 2120
	end if

2300	text$ = fnformat$(AP_VENDOR::VENNUM, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::VENNAM, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ADD1, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ADD2, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::CITY, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::STATE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ZIP, fmt%) + fmtflag$ + &
		fnformat$(CONTACT$, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::PHONE, fmt%) + fmtflag$ + &
		fnformat$(FAX$, fmt%) + fmtflag$ + &
		fnformat$(MEMO$, fmt%) + fmtflag$ + &
		fnformat$(TERMS$, fmt%)

	print #1%, text$

	goto 2100

32767	end
