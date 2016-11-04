1	OPEN "SS0:W2.FIL" FOR INPUT AS FILE 11% &
	\ DIM #11%, A0$(15%)=32%, S%(0%), S$(10%)=4%, S1$(10%)=16% &
	\ PRINT "Control number ";A0$(1%) &
	\ PRINT "Federal number ";A0$(2%) &
	\ PRINT "Pension Plan   ";A0$(4%) &
	\ PRINT "Include non-taxed earning in wages (Box 10 & 18) ";A0$(5%) &
	\ PRINT "Employer's name  ";A0$(6%) &
	\ PRINT "Address line 1   ";A0$(7%) &
	\ PRINT "Address line 2   ";A0$(8%) &
	\ PRINT "Address line 3   ";A0$(9%) &
	\ PRINT "Address line 4   ";A0$(10%) &
	\ PRINT &
	\ PRINT "State ";S$(I%);"  Id code ";S1$(I%) FOR I%=1% TO S%(0%) &
	\ CLOSE 11% &

32767	END
