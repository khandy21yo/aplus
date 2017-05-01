1	!
	! Dump AP vendor file in to an ascii file
	!
 !	%include %from %cdd "cdd$top.AP.AP_VENDOR"
	%include "source:[ap.open]ap_vendor.hb"
	map (AP_VENDOR) AP_VENDOR_cdd AP_VENDOR

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

1010	open "APvendor.txt" for output as file 1%, &
		recordsize 511%

	text$ = fnformat$("VENNUM", fmt%) + fmtflag$ + &
		fnformat$("VENNAM", fmt%) + fmtflag$ + &
		fnformat$("ADD1", fmt%) + fmtflag$ + &
		fnformat$("ADD2", fmt%) + fmtflag$ + &
		fnformat$("CITY", fmt%) + fmtflag$ + &
		fnformat$("STATE", fmt%) + fmtflag$ + &
		fnformat$("ZIP", fmt%) + fmtflag$ + &
		fnformat$("COUNTRY", fmt%) + fmtflag$ + &
		fnformat$("PHONE", fmt%) + fmtflag$ + &
		fnformat$("POADD1", fmt%) + fmtflag$ + &
		fnformat$("POADD2", fmt%) + fmtflag$ + &
		fnformat$("POCITY", fmt%) + fmtflag$ + &
		fnformat$("POSTATE", fmt%) + fmtflag$ + &
		fnformat$("POZIP", fmt%) + fmtflag$ + &
		fnformat$("POCOUNTRY", fmt%) + fmtflag$ + &
		fnformat$("POPHONE", fmt%) + fmtflag$ + &
		fnformat$("PURGE", fmt%) + fmtflag$ + &
		fnformat$("FEDID", fmt%) + fmtflag$ + &
		fnformat$("FLG1099", fmt%) + fmtflag$ + &
		fnformat$("DUEDAYS", fmt%) + fmtflag$ + &
		fnformat$("DUEDATE", fmt%) + fmtflag$ + &
		fnformat$("DISDAYS", fmt%) + fmtflag$ + &
		fnformat$("DISDATE", fmt%) + fmtflag$ + &
		fnformat$("DISCPER", fmt%) + fmtflag$ + &
		fnformat$("ALPSRT", fmt%)

	print #1%, text$

2000	reset #AP_VENDOR.ch%

2100	get #AP_VENDOR.ch%, regardless

	text$ = fnformat$(AP_VENDOR::VENNUM, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::VENNAM, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ADD1, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ADD2, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::CITY, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::STATE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::ZIP, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::COUNTRY, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::PHONE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POADD1, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POADD2, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POCITY, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POSTATE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POZIP, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POCOUNTRY, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::POPHONE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::PURGE, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::FEDID, fmt%) + fmtflag$ + &
		fnformat$(AP_VENDOR::FLG1099, fmt%) + fmtflag$ + &
		NUM1$(AP_VENDOR::DUEDAYS) + fmtflag$ + &
		fnformat$(AP_VENDOR::DUEDATE, fmt%) + fmtflag$ + &
		NUM1$(AP_VENDOR::DISDAYS) + fmtflag$ + &
		fnformat$(AP_VENDOR::DISDATE, fmt%) + fmtflag$ + &
		NUM1$(AP_VENDOR::DISCPER) + fmtflag$ + &
		fnformat$(AP_VENDOR::ALPSRT, fmt%)

	print #1%, text$

	goto 2100

32767	end
