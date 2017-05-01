/*
 * %TITLE "Read Data From Percon Portable"
 * %SBTTL "UT_SPEC_UPDTFER"
 *
 * This version maintained by:
 *	Software Solutions, Inc.
 *	Email: <kth@srv.net>
 *	Web:   <http://srv.net/~kth>
 *	Idaho Falls, Idaho
 *
 *++
 * Abstract:
 *	.b
 *	.lm +5
 *	updtfer.c - percon pocketreader program for Unix
 *	.b
 *	This program is used to read information in from a Percon
 *	portable data reader into a text file.
 *	.b
 *	Note this version uses fixed parameter positions in the arguments
 *	device must be first, filename second, options 3rd.
 *	.b
 *	Timeouts & error handling (errcnt) needs refinement
 *	.lm -5
 *
 * Compile:
 *
 *	$ CC UTL_SOURCE:UT_SPEC_UPDTFER
 *	$ LINK/EXE=UTL_EXE: UT_SPEC_UPDTFER, FUNC_LIB:CMCLINK/OPTION
 *	$ DELETE UT_SPEC_UPDTFER.OBJ;*
 *
 * Author:
 *
 *	??/??/?? - Darren Goddard.
 *		Original author?
 *
 *	??/??/?? - Robert Meyers Consulting, Inc.
 *		adapted for Unix from DOS version "pdtfer"
 *
 *	09/30/97 - Kevin Handy
 *		Created from UPDTFER from Pecon's bulliten board, with
 *		many changes to make it work with VMS and Linux.
 *
 * Modification History:
 *
 *	10/07/97 - Kevin Handy
 *		And checksum with '0x7f' instead of '0xff' to fix
 *		checksum read error problems.
 *--
 */

/*
 * unix includes
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#ifdef VMS
#include <iodef.h>
#include <descrip.h>
#include <ssdef.h>
#include <unixio.h>

typedef struct	/* template for QIO read iosb. refer to section 8.5 in */
{		/* the IO users guide (part 1) for detailed info. */
	short cond_value;
	short offset;
	short term;
	short term_size;
} status_block;

int SYS$ASSIGN(void *, ...);
int LIB$STOP(long x);
int SYS$QIOW(long x, ...);

#else
#include <signal.h>
#include <sys/ioctl.h>
#endif

#define SilentPrint(x) if (!Silent) fprintf(stderr, "%s\n", x)
#define DebugPrint(x) if (debug) fprintf(stderr, "%s\n", x)

#define DATA_READY	0x100
#define MAXLINEBUF	256		/* max line buffer size */
#define bitmask	 	0xff

/* internal protocol states... */
#define RXDATA		1		/* ...expecting data */
#define RXCHECK		2		/* ...expecting checksum */
#define TXNEWLINE	3		/* tx packet */
#define TXSAMELINE	4
#define WAITRESP	5
#define END		6

/* protocol controls */
#define BEL		0x07		/* abort action */
#define ACK		0x06		/* affimative */
#define NAK		0x15		/* negative */
#define EOM		0x0d		/* cr is end of message */
#define OUREOF		0x1a		/* ctrl Z is end of file */

#define TRUE		1
#define FALSE		0

#define TRANSMIT	0x0001
#define RECEIVE		0x0002
#define APPEND		0x0004
#define NOINIT		0x0008
#define HELP		0x0010

extern int errno;


/***
//
//  Function declarations
//
***/
int    getline(FILE * hfile);
void   txline(char * linebuffer, int count, char check);
void   usage(int mode);
int    line_set();
int    readbyte(int seconds);
void   sendbyte(int ch);

int    debug = 0;
int    Silent = 0;	/* show diagnostics */
int    Errcnt = 0;

/*
 * alarm clock timeout function - do nothing, just return
 */
#ifndef VMS
static void alrm(int param)
{
}
#endif

/*
//  Global variable declarations
*/
char    LineBuff[MAXLINEBUF];
char    DeviceName[128];
char    LF[]= {"\x0a"};
char    EOFmes[] = {"\x1a\x0d\x00"};

#ifdef VMS
struct dsc$descriptor_s terminal;
/* $DESCRIPTOR(terminal, "TT:"); */
short tt_chan=0;
#else
int pfd;  /* port file descrip*/
tcflag_t Imodes = 0;
#endif


/***
//
//  Main (entry) function
//
***/

int main(int argc, char *argv[])
{
	FILE    * fhandle=NULL;    /* file handle */
	int     biostat;           /* value from com port */
	unsigned char RxChar;      /* Comm port received char */
	int     ProtoMode = END;   /* receiving state/mode */
	int     EndOfJob = FALSE;  /* false while not end of job */
	int     rxcount = 0;       /* char count for line */
	int     ourcheck=0;        /* our calculated check (all in mod 8) */
	int     lencheck;          /* hi byte == len, low == check */
	int     EndInSight=0;      /* flag indicates near completion */
	char    FileName[64];
	unsigned int mode=0;
	long status;

	LineBuff[0] = '\0';
	FileName[0] = '\0';
	DeviceName[0] = '\0';

	if (argc < 4)
	{
		fprintf(stderr,"%s: not enough arguments.\n", argv[0]);
		usage(mode);
	}

	strcpy(DeviceName, argv[1]);
	strcpy(FileName, argv[2]);
	strcpy(LineBuff, argv[3]);

	/*  parse arguments */
	argc=strlen(LineBuff);
	while(argc--)
	{
		switch(LineBuff[argc])
		{
		case '-':
		case '/':
			break;
		case 'T':
		case 't':
			mode |= TRANSMIT;
			break;
		case 'R':
		case 'r':
			mode |= RECEIVE;
			break;
		case 'A':
			case 'a':
			mode |= APPEND;
			break;
		case 'N':
		case 'n':
			mode |= NOINIT;
			break;
		case 'D':
		case 'd':
			++debug;
			break;
		case '?':
		case 'h':
		case 'H':
			mode |= HELP;
			break;
		case 'S':
		case 's':
			++Silent;
			break;
		default:
			fprintf(stderr,"%s: invalid option\n", argv[0]);
			usage(mode);
		}
	}

	if (mode & HELP)
	{
		usage(mode);
	}

	if ((mode & TRANSMIT) && ((mode & RECEIVE)||(mode & APPEND)))
	{
		fprintf(stderr, "%s: cannot receive and transmit", argv[0]);
		usage(mode);
	}

	if (mode & TRANSMIT)
	{
#ifndef VMS
		Imodes |= IXON;
#endif
		if ((fhandle = fopen(FileName , "r")) == NULL)
		{
			fprintf(stderr, "%s: error opening file %s\n",argv[0],FileName);
			exit(1);
		}
		ProtoMode = TXNEWLINE;
		sprintf(LineBuff,"Transmiting %s on port %s.\n",FileName,DeviceName);
	}

	if (mode & APPEND)
	{
		if ((fhandle = fopen(FileName , "a")) == NULL)
		{
			fprintf(stderr,"%s: error opening %s for append.\n", argv[0],FileName);
			exit(1);
		}
#ifndef VMS
		Imodes |= IXOFF;
#endif
		ProtoMode = RXDATA;
		sprintf(LineBuff,"Appending to %s from port %s.\n",FileName,DeviceName);
	}
	else
	{
		if (mode & RECEIVE)
		{
			if ((fhandle = fopen(FileName , "w")) == NULL)
			{
				fprintf(stderr, "%s: error creating file %s\n", argv[0], FileName);
				exit(1);
			}
#ifndef VMS
			Imodes |= IXOFF;
#endif
			ProtoMode = RXDATA;
			sprintf(LineBuff,"Receiving %s from port %s.\n",FileName,DeviceName);
		}
	}

	/*
	 *open port
	 */
#ifdef VMS
	/* Assign an I/O channel to device. */
	terminal.dsc$a_pointer = DeviceName;
	terminal.dsc$w_length = strlen(DeviceName);
	terminal.dsc$b_class = DSC$K_CLASS_S;
	terminal.dsc$b_dtype = DSC$K_DTYPE_T;

	if (((status = SYS$ASSIGN(&terminal, &tt_chan, 0, 0)) & 1) != 1)
	{
		fprintf(stderr, "Error %d in assign\n", status);
		exit(EXIT_FAILURE);
	}
#else
	if ((pfd = open(DeviceName, O_RDWR)) < 0)
	{
		fprintf(stderr,"%s: error opening port %s\n",argv[0],DeviceName);
		exit(1);
	}

	/*
	 * check and make sure 1st argument is truely a tty device type
	 */
	if (!isatty(pfd))
	{
		fprintf(stderr,"%s: invalid serial port %s\n", argv[0],DeviceName);
		exit(1);
	}
#endif

	if (!(mode & NOINIT))
	{
		biostat = line_set();
		if (biostat < 0)
		{
			fprintf(stderr,"%s: error setting device %s\n", argv[0],DeviceName);
			exit(1);
		}
	}

	SilentPrint(LineBuff);

	/*
	 *  Do until End Of File
	 */
	while(!EndOfJob)
	{
		switch (ProtoMode)
		{
		case TXNEWLINE:
			lencheck = getline(fhandle);
			/* falls through */
		case TXSAMELINE:
			if ((lencheck & 0xffff) == 0xff)
			{
				DebugPrint("End In Sight");
				EndInSight = TRUE;
				txline(EOFmes, 2, '\x27'); /* from d9 */
				ProtoMode = WAITRESP;
			}
			else
			{
				txline(LineBuff, (lencheck >> 8) & 0xff,
					lencheck & 0xff);
				SilentPrint(LineBuff);
				ProtoMode = WAITRESP;
			}
			break;

		case RXCHECK:
			if (debug)
			{
				fprintf(stderr, "Expected checksum: %d\n",
					ourcheck);
			}
			break;
		}

		biostat = readbyte(10);
		RxChar  = biostat & 0xff;

		if (biostat >= 0)
		{
			switch(ProtoMode)
			{
			case RXDATA:
				if (RxChar != '\r')
				{
					LineBuff[rxcount++] = RxChar;
				}
				ourcheck = (RxChar + ourcheck) & 0x7f;
#if 0
if (debug)
{
	printf("--Checksum Calc +%d = %d\n", RxChar, ourcheck);
}
#endif
				if (RxChar == EOM)
				{
					ProtoMode = RXCHECK;
				}
				break;

			case RXCHECK:
				if (ourcheck != RxChar)
				{
					if (debug)
					{
						fprintf(stderr,"  Bad checksum %d -> %d\n",RxChar,ourcheck);
						fprintf(stderr,"  data>%s<\n",LineBuff);
					}
					sendbyte(NAK);
					if (++Errcnt > 10)
					{
						SilentPrint("abort, to many errors");
						exit(1);
					}
				}
				else
				{
					if (LineBuff[rxcount-1] == OUREOF)
					{
						EndOfJob = TRUE;
						DebugPrint("* End of Job *");
					}
					else
					{
						fwrite(LineBuff,rxcount,1,fhandle);
						fwrite(LF,1,1, fhandle);
						LineBuff[rxcount] = '\0';
						SilentPrint(LineBuff);
					}
					sendbyte(ACK);
				}
				ProtoMode = RXDATA;
				rxcount = 0;
				ourcheck = 0;
				break;

			case WAITRESP:
				if (RxChar == ACK)
				{
					ProtoMode = TXNEWLINE;
					if (EndInSight)
					{
						EndOfJob  = TRUE;
					}
				}
				else if (RxChar == NAK)
				{
					DebugPrint("  Got a NAK");
					ProtoMode = TXSAMELINE;
					if (++Errcnt > 10)
					{
						SilentPrint("abort, to many errors");
						exit(1);
					}
				}
				else if (RxChar == BEL)
				{
					ProtoMode = END;
					EndOfJob  = TRUE;
				}
				break;
			}   /* switch ProtoMode */
		}
		else
		{
			if (debug)
			{
				fprintf(stderr,"* Timeout ");
				switch(ProtoMode)
				{
				case RXDATA:
					fprintf(stderr," Receiving Data *\n");
					break;
				case RXCHECK:
					fprintf(stderr," Waiting For Checksum %d*\n",
						ourcheck);
					break;
				case TXNEWLINE:
					fprintf(stderr," TXNEWLINE *\n");
					break;
				case TXSAMELINE:
					fprintf(stderr," TXSAMELINE *\n");
					break;
				case WAITRESP:
					fprintf(stderr," Waiting for ACK/NACK *\n");
				}
			}
			/* ioctl(pfd,TCXONC,1);  */
			if (ProtoMode == RXDATA || ProtoMode == RXCHECK)
			{
				ProtoMode = RXDATA;
				rxcount = 0;
				ourcheck = 0;
				DebugPrint("Sending Nak to restart");
				sendbyte(NAK);
			}
			if (++Errcnt > 10)
			{
				SilentPrint("abort, too many errors");
				exit(1);
			}
		} /* biostat >= 0 */
	}  /* end of while(!EndOfJob) */
Terminate:
	fclose(fhandle);
}

/***************************************************************
//
//  Read a line from file, return an int (hi = len, low = check)
//  len 0 with check 0xff indicates EOF
//
***/

int getline(FILE* hfile)
{
	int     check = 0;
	char    charbuf;
	int     Stat =0;
	int     ccount = 0;

	while(!Stat)
	{
		if (fread(&charbuf,1,1, hfile) == 0)
		{
			DebugPrint("fread returned 0");
			return(0x00ff);
		}
		if (charbuf == '\n')
		{
			Stat    = EOM;
			charbuf = '\r';
		}
		check = (charbuf+check) & 0xff;
		LineBuff[ccount++] = charbuf;
	}
	LineBuff[ccount] = '\0';
	return((ccount<<8) | check);
}

void txline(char* linebuffer, int count, char check)
{
	while(count--)
	{
		sendbyte(*linebuffer++); /* tx line data */
	}
	sendbyte(check); /* Checksum */
}

/*
 * Change the communication line settings to the new values.
 */
int line_set()
{
#ifdef VMS
	return 0;
#else
	struct termio tbuf;

	ioctl(pfd, TCGETA, &tbuf);

	tbuf.c_cc[4] = 1;	/* VMIN - minimum # of chars to satisfy a read */
	tbuf.c_cc[5] = 0;	/* VTIME - wait forever */
	tbuf.c_oflag = 0;
	tbuf.c_cflag = (CREAD | HUPCL | CLOCAL);
	tbuf.c_lflag = 0;
	tbuf.c_iflag = 0;	/* input flow control */
	tbuf.c_cflag |= B9600;
	tbuf.c_cflag |= CS8;
	return(ioctl(pfd, TCSETAF, &tbuf));
#endif
}

void usage(int mode)
{
	printf("\n    Percon PocketReader Unix transfer utility v1.3  (09/30/97)\n");
	printf("    Copyright (c) 1992 Percon, Inc.\n\n");
	printf("    USAGE:    updtfer device filename -options \n");
	printf("    WHERE:\n    device = serial port name, such as /dev/tty1a\n");
	printf("            filename = filename for input (output) data source (destination) \n");
	printf("            options = letters, preceded by \"-\" character.\n");
	printf("            r   Receive, replace.     t   Transmit\n");
	printf("            a   Append receive.       d   Debug info\n");
	printf("            h   This help             s   Silent\n\n");
	printf("    EXAMPLES:\n");
	printf("        updtfer /dev/tty1a filename /r\n     (Receive file filename on serial port /dev/tty1a)\n");
	printf("        updtfer /dev/tty1a filename /t\n     (Transmit file filename on serial port /dev/tty1a)\n");
	printf("    Written by Darren Goddard.\n");
	printf("    Unix adaptation by Robert Meyers\n\n");
	exit(1);
}
	

int readbyte(int seconds)
{
#ifdef VMS
	short func_code=0;
	int term_mask[2];
	register long status;
	char inchar[80];
	status_block iosb;

	/*
	 * No terminaters
	 */
	term_mask[0] = 0;
	term_mask[1] = 0;

	/*
	 * Read in info, dont lose anything, and respect timeout value
	 */
	if (seconds > 0)
	{
		func_code = IO$_READVBLK | IO$M_NOFILTR | IO$M_TIMED;
	}
	else
	{
		func_code = IO$_READVBLK | IO$M_NOFILTR;
	}
	status = SYS$QIOW(0, tt_chan, func_code, &iosb, 0, 0,
		&inchar, 1, seconds, &term_mask, 0, 0);

	if ((status & 1) != 1)
	{
		fprintf(stderr, "Error in read %d %d\n", status, iosb.cond_value);
		exit(EXIT_FAILURE);
	}

	if (iosb.cond_value == SS$_TIMEOUT)
	{
		return -1;
	}
	return (inchar[0] & bitmask);
#else
	static char rxbuf[BUFSIZ], *p;	/* BUFSIZ is defined in stdio.h */
	static int count = 0;

	if (count > 0)
	{
		count--;
		return(*p++ & bitmask);
	}
	if (seconds > 0)
	{
		signal(SIGALRM, alrm);
		alarm((unsigned)seconds);
	}
	if ((count = read(pfd, p = rxbuf, BUFSIZ)) < 1)
	{
		return(-1);
	}
	if (seconds > 0)
	{
		alarm(0);
	}
	count--;
	return(*p++ & bitmask);
#endif
}

/* Output a byte */

void sendbyte(int ch)
{
#ifdef VMS
	short func_code;
	register long status;
	char outchar[4];
	status_block iosb;

	outchar[0] = ch & bitmask;
	func_code = IO$_WRITEVBLK | IO$M_NOFORMAT | IO$M_REFRESH;
	status = SYS$QIOW(0, tt_chan, func_code, &iosb, 0, 0,
		&outchar, 1, 0, 0, 0, 0);

#else
	char c;

	c = ch & bitmask;
	write(pfd, &c, 1);
#endif
}

