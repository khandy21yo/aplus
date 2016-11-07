100	! &
	! Read 8" CP/M disks on the MDB Micro-11 &
	! &

110	OPEN "_kb:" AS file 12% &
\	false% = 0% &
\	true% = NOT false% &
\	unit$( 1% ) = "A" &
\	unit$( 2% ) = "B" &
\	unit$( 3% ) = "C" &
\	unit$( 4% ) = "D" &
\	DIM command$( 20% ), density%( 4% ), sector.size%( 4% ), &
		logged.in%( 4% ), entry$( 64% ), sector$( 16% ), &
		alloc.vec$( 16% ), buffer.rx02$( 4% ), sector.skew%( 26% ) &
\	num.command% = 0% &
\	UNTIL command$( num.command% ) = "NOMORE" &
\		num.command% = num.command% + 1% &
\		READ command$( num.command% ) &
\	NEXT &
\	num.command% = num.command% - 1% &
\	DATA	A:, B:, C:, D:, DIR, TYPE, SCAN, END, USER, NOMORE &

120	READ sector.skew%( sector% ) FOR sector% = 1% TO 26% &
\	DATA	1,  7, 13, 19, 25,  5, 11, 17, 23,  3,  9, 15, 21, &
		2,  8, 14, 20, 26,  6, 12, 18, 24,  4, 10, 16, 22, &

130	OPEN "_nl:" AS FILE 11%, RECORDSIZE 32% + 16%*128% &
\	FIELD #11%, dummy% *  32% AS dummy$,  32% AS entry$( dummy% ) &
		FOR dummy% = 0% TO 64% &
\	FIELD #11%, 32%+(dummy% - 1%)*128% AS dummy$, 128% AS sector$(dummy%) &
		FOR dummy% = 1% to 16% &
\	FIELD #11%, 1% AS user.num$, 8% AS file.name$, 3% AS file.ext$, &
		1% AS extent.num$, 2% AS unknown$, 1% AS file.size$ &
\	FIELD #11%, 16% - 1% + dummy% AS dummy$, 1% AS alloc.vec$( dummy% ) &
		FOR dummy% = 1% TO 16% &

210	! &
	! Start it all off with the right "Boot", ah, foot... &
	! &
	INPUT #12%, "RX unit to use:"; unit.num% &
\	unit% = unit.num% + 1% &
\	user% = 0% &
\	GOSUB 11000 &
300	! &
	! How about a menu of choices? &
	! No, let's give the CP/M prompt:  A> &
	! (after he has told us that he has put a disk in drive A, anyway. &
	! &

320	! &
	! Print the prompt and parse the command line &
	! &
	PRINT unit$( unit% ); ">"; &
\	INPUT LINE #12%, command.in$ &
\	PRINT if CCPOS( 12% ) &
\	file.found% = false% &
	! &
	! Make the command line easier to parse, like CP/M does by stripping &
	! parity, leading and trailing spaces, reducing tabs and spaces to &
	! one, and converting to uppercase &
	! &
\	command.in$ = cvt$$( command.in$, 1%+4%+8%+16%+32%+128%+256% ) &
\	GOTO 300 UNLESS LEN( command.in$ ) &
\	param.ptr% = instr( 1%, command.in$ + " ", " " ) &
\	command.tail$ = right( command.in$, param.ptr% + 1% ) &
\	command$ = left( command.in$, param.ptr% - 1%) &
\	found.command% = 0% &
\	found.command% = item% IF command$ = command$( item% ) &
		FOR item% = 1% TO num.command% &
\	UNLESS found.command% &
	THEN	PRINT command$; "?" &
\		GOTO 320 &

330	! &
	! Well, now we have a command that we like, let's do something: &
	! &
	ON found.command% GOTO 1000, 1000, 1000, 1000, 2000, 3000, 13000, &
		32000, 4000 &
1000	! &
	! "Log in" a new drive &
	! &
	unit.num% = ASCII(command$) - ASCII("A") &
\	unit% = unit.num% + 1% &
\	GOSUB 11000 unless logged.in%( unit% ) &
\	GOTO 300 &
2000	! &
	! DIR command &
	! &

2010	! &
	! For now, we'll assume that he meant "d:*.*" &
	! &
	track% = 2% &
\	FOR sector% = 1% TO 16% &
\		GET #unit%, RECORD fnrecord.num%(track%, sector.skew%(sector%)) &
\		LSET sector$( sector% ) = buffer.rx02$( unit% ) &
\	NEXT sector% &
	! &
	! Now look at each directory entry and see if it is worth &
	! talking about... &
	!&
\	FOR entry% = 1% TO 64% &
\		LSET entry$( 0% ) = entry$( entry% ) &
\		IF ASCII( user.num$ ) = user% AND ASCII(extent.num$) = 0% &
		THEN	PRINT #12%, file.name$; "."; file.ext$; "   "; &
\			PRINT #12% IF CCPOS( 12% ) > 70% &

2020	NEXT entry% &
\	PRINT #12% IF CCPOS( 12% ) > 70% &
\	PRINT #12% &

2999	GOTO 300 &
3000	! &
	! TYPE &
	! &
	param.ptr% = INSTR( 1%, command.tail$ + " ", " " ) &
\	file.req$ = LEFT( command.tail$, param.ptr% - 1% ) &
\	file.req$ = file.req$ + "." UNLESS instr( 1%, file.req$, "." ) &
\	dot.ptr% = instr( 1%, file.req$, "." ) &
\	file.req$ = LEFT(LEFT(LEFT(file.req$, dot.ptr%-1%) + SPACE$(8%), 8%) &
		+ RIGHT(file.req$, dot.ptr%) + "   ", 12%) &
	! &
	! Finally.  We have a filename we can trust? &
	! Well, let's try it anyway. &
	! &
	! Pull in the entire directory on this disk &
	! &
\	track% = 2% &
\	FOR sector% = 1% TO 16% &
\		GET #unit%, RECORD fnrecord.num%(track%, sector.skew%(sector%)) &
\		LSET sector$( sector% ) = buffer.rx02$( unit% ) &
\	NEXT sector% &
	! &
	! Now look for the right entry, and get set to "open" the file. &
	! Start out by looking for extent #0 &
	! &
\	curr.ext% = 0% &

3020	FOR entry% = 1% TO 64% &
\		LSET entry$( 0% ) = entry$( entry% ) &
\		IF ASCII(user.num$) = user% AND ASCII(extent.num$) = curr.ext% &
		THEN	IF file.req$ = file.name$ + "." + file.ext$ &
			THEN	file.found% = true% &

3030	NEXT entry% &
	! &
	! Well, we must be all done by now, finish up and go home... &
	! &
\	UNLESS file.found% &
	THEN	PRINT file.req$; "?" &
\		PRINT &
\		GOTO 300 &

3100	! &
	! We found it, now what do we do with it? &
	! &
	print "WE found it!~~/\~~" &

3999	GOTO 300 &
4000	! &
	! USER &
	! &
	param.ptr% = INSTR( 1%, command.tail$ + " ", " " ) - 1% &
\	UNLESS param.ptr% &
	THEN	PRINT #12%, "USER"; user% &
	ELSE	dummy% = VAL( LEFT( command.tail$, param.ptr% ) ) &
\		IF dummy% > 31% &
		THEN	PRINT NUM1$( dummy% ); "?"; CHR$(10%) &
		ELSE	user% = dummy% &

4999	GOTO 300 &
11000	! &
	! Make sure that we open the floppy in "Sector" mode, as opposed to &
	! "Block" mode, so that we work only with one sector at a time, rather &
	! than 512 bytes at a time. &
	! &

11010	CLOSE unit% &
\	OPEN "DX" + NUM1$( unit.num% ) + ":" AS FILE unit%, MODE 16384% &
\	density%( unit% ) = fndensity%( unit% ) &
\	sector.size%( unit% ) = 128% * density%( unit% ) &
\	logged.in%( unit% ) = true% &
\	FIELD #unit%, sector.size%( unit% ) AS buffer.rx02$( unit% ) &
\	RETURN
13000	! &
	! SCAN function &
	! For now, let's just read the whole bloody thing... &
	! &

13100	PRINT "Sector size ="; sector.size%(unit%) &
\	FOR track% = 0% TO 76% &
\		FOR sector% = 1% TO 26% &
\			record.num% = fnrecord.num%( track%, &
					sector.skew%( sector% ) ) &
\			GET #unit%, RECORD record.num% &
\			GOSUB 16000 		! Process that record... &
\		NEXT sector% &
\	NEXT track% &

13900	GOTO 300 &
16000	! &
	! Print the buffer... &
	! &

16010	PRINT &
\	PRINT "Track "; NUM1$ (track%); ", sector "; NUM1$ (sector%) &
\	FOR line.num% = 1% TO sector.size%( unit% ) STEP 16% &
\	PRINT fnhex$( line.num%/256% ); fnhex$( line.num% - 1% ); "  "; &
\		PRINT fnhex$(ASCII(MID(buffer.rx02$(unit%), char%, 1%))); " "; &
			FOR char% = line.num% TO line.num% + 15% &
\		PRINT fnprintable$(ASCII(MID(buffer.rx02$(unit%),char%,1%))); &
			FOR char% = line.num% TO line.num% + 15% &
\		PRINT &
\	NEXT line.num% &

16090	RETURN &
18000	! &
	! Some function definitions may be useful here... &
	! &

18100	! &
	! Convert track and sector number to RSTS/E RECORD number &
	! &
	! According to the RSTS/E Programming Manual for V8.0 dated &
	! March 1983, the TRACK and SECTOR number are defined as follows: &
	! &
	!	TRACK  = INT(n/26) &
	!	SECTOR = n - INT(n/26)*26 + 1 &
	&
	DEF* fnrecord.num% (track%, sector%) = (track% * 26%) + sector% - 1% &
	&
	! And, by the same token, we can unmash the RSTS/E RECORD number: &
	&
\	DEF* fntrack%(  record.num% ) = INT( record.num%/26% ) &
\	DEF* fnsector%( record.num% ) &
		= record.num% - INT( record.num%/26% )*26% + 1% &
	&
	! Also useful would be a function to return the density of a diskette: &
	&
\	DEF* fndensity%( channel% ) = SPEC%( 0%, 0, channel%, 18% ) AND 255% &

18200	! &
	! How about one to determine the printability of a given character: &
	! &
	DEF* fnprintable$( char% ) &
\		dummy% = char% AND 127% &
\		dummy% = 46% IF dummy% < 32% &
\		dummy% = 46% IF dummy% = 127% &
\		fnprintable$ = CHR$( dummy% ) &
\	FNEND &
	! &
	! While we are at it, let's print a hexadecimal value: &
	! &
\	DEF* fnhex$( byte% ) &
		= mid( "0123456789ABCDEF", ((byte%/16%) AND 15%) + 1%, 1%) &
		+ mid( "0123456789ABCDEF", (byte% AND 15%) + 1%, 1%) &
32000	! &
	! Close up shop, and go home to DCL or whoever... &
	! &
	CLOSE 1%, 2%, 3%, 4%, 11%, 12% &

32767	END
