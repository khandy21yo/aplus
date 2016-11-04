1	EXTEND
2	  !	DSKLAB.BAS - Read the volume label of BACKUP disk	&
	  !								&
	  !	(C) Copyright 1983 by Computer Management Center, Inc.	&
	  !								&
	  !			All Rights Reserved			&
	  !								&
	  !-------------------------------------------------------------&
	  !								&
	  !	V1.0-00		Pete Slater		04-Nov-83	&
	  !								&
	  !-------------------------------------------------------------&

100	  PRINT "DSKLAB V1.0-00 ";RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%) &
	\ PRINT &

110	  INPUT "Device ";DEV$ &
	\ DEV$=CVT$$(DEV$,-1%) &
	\ I$=SYS(CHR$(6%)+CHR$(-10%)+DEV$) &
	\ UNLESS SWAP%(CVT$%(MID(I$,29%,2%))) AND 8192% &
	  THEN	  PRINT "A device name must be specified." &
		\ GOTO 10000 &

120	  ON ERROR GOTO 19000 &
	\ MAG.TAPE%=-1% IF MID(I$,23%,1%)="M" &
	\ MDE%=0% &
	\ MDE%=800% IF MAG.TAPE% &
	\ MDE%=1600% IF MAG.TAPE% AND INSTR(1%,DEV$,"/DEN:1600") &
	\ RCD%=512% &
	\ RCD%=516% IF MAG.TAPE% &
		! Set-up error-trapping &
		! Set MODE and RECORDSIZE values depending on whether it's &
		!	a disk or a tape &

130	  DEV$=MID(I$,23%,2%)+NUM1$(ASCII(MID(I$,25%,1%)))+":" &
	\ OPEN DEV$ AS FILE #6%, RECORDSIZE RCD%, MODE MDE% &

140	  LNGTH.OF.DISK%=PEEK(PEEK(PEEK(PEEK(520%))+12%)+4%)-1% &
		UNLESS MAG.TAPE% &
			! Find the length of the disk &

1000	  ! &
	  !		M A I N   P R O G R A M &
	  ! &

1010	  TRY%=1% &
	\ BLK%=1% &

1020	  GET #6%, RECORD BLK% UNLESS MAG.TAPE% &
	\ I%=MAGTAPE(3%,0%,6%) IF MAG.TAPE% &
	\ FIELD #6%, 84% AS I$ &

1030	  IF LEFT(I$,12%)<>"BACKUP543210" &
	  THEN	  PRINT "This is not a BACKUP volume!" &
		\ GOTO 10000 &

1040	  PRINT "The backup set name is ";MID(I$,13%,6%);" Sequence #"; &
		CVT$%(MID(I$,25%,2%)) &
	\ PRINT &

10000	 CLOSE #6% &
	\ GOTO 32767 &

19000	  !------------------------------------------------------------- &
	  ! &
	  !		E R R O R   T R A P P I N G &
	  ! &
	  !------------------------------------------------------------- &

19010	  IF ERR=14% &
	  THEN	  PRINT DEV$;" is hung or write locked." &
		\ RESUME 10000 &

19015	  IF MAG.TAPE% &
	  THEN	  PRINT "Error on ";DEV$;" #";ERR &
		\ RESUME 10000 &

19020	  BLK%=13% IF TRY%=1% &
	\ BLK%=LNGTH.OF.DISK% IF TRY%=2% &
	\ BLK%=BLK%-13% IF TRY%=3% &
	\ TRY%=TRY%+1% &
	\ RESUME 1020 UNLESS TRY%=4% &
	\ PRINT "This disk is unreadable!" &
	\ RESUME 10000 &

32767	  END &

