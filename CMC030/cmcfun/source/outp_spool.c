/*
 *	%TITLE "OUTP_SPOOL -- Print Spooler Routing Function"
 */
#pragma module outp_spool "V3.6 Calico"

/*
 * COPYRIGHT (C) 1988 BY
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * ABSTRACT:
 *
 *	This function is used to send a report to the
 *	spooler when the processing is finished.
 *
 *	This function will try to mount a form if it is
 *	given a form name.
 *
 *	NOTE: No errors are returned back.
 *
 * Parameters:
 *
 *	UTL_REPORTX
 *		Passed report structure containing all necessary
 *		information to spool the report.
 *
 *
 *	Returned value
 *		This function sends a report on to the
 *		spooler when the print processing has finished.
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:OUTP_SPOOL/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_SPOOL
 *	$ DELETE OUTP_SPOOL.OBJ;*
 *
 * AUTHOR:
 *
 *	11/24/86 - B. Craig Larsen
 *		VAX Version
 *
 *	05/17/88 - Kevin Handy
 *		Conversion to a function.
 *
 * MODIFICATION HISTORY:
 *
 *	05/17/88 - Kevin Handy
 *		Converted into a function.
 * 
 *	08/02/88 - Kevin Handy
 *		Added a /NOFEED to the spooling, so it
 *		won't matter what the spooler is set up as, it
 *		will still accept our paging info and not it's
 *		own.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	03/04/99 - Kevin Handy
 *		Use #include instead of #dictionary
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, sys$routines.h, descriptors, sjcdef.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <ctype.h>
/* #include <sys$routines.h> */

#ifdef DEBUG
#include <descrip>
#include "func_include:cmcfun.h"

extern struct scope_struct scope;
#endif

/* #include "func_include:sndjbc.h" */
#include <sjcdef.h>
extern long sys$sndjbcw();

#include "source:[utl.open]utl_reportx.h"

struct iosb
{
	long	stat;
	long	zero;
};

struct itemlst
{
	short int buflen;
	short int code;
	long	bufadr;
	long	retlenadr;
};


void outp_spool(struct utl_reportx_cdd *utl_reportx)
{
	/*
	 * Declare vars
	 */
	long sys_stat;
	int elem;
	int len;

	struct itemlst spool[10];
	struct iosb iosstr;
#ifdef DEBUG
	struct dsc$descriptor_s dsc_mesg;
#endif

	char errortext[80];
	int flag;

	/*
	 * Start up for first element
	 */
	elem = 0;

	/*
	 * Queue name
	 */
	spool[elem].buflen 	= sizeof(utl_reportx->spool);
	spool[elem].code	= SJC$_QUEUE;
	spool[elem].bufadr 	= (long)utl_reportx->spool;
	spool[elem].retlenadr	= 0;
	elem++;

	/*
	 * File name
	 */
	spool[elem].buflen	= sizeof(utl_reportx->defout);
	spool[elem].code	= SJC$_FILE_SPECIFICATION;
	spool[elem].bufadr	= (long)utl_reportx->defout;
	spool[elem].retlenadr	= 0;
	elem++;
               
	/*
	 * Delete the file on completion
	 */
	spool[elem].buflen	= 0;
	spool[elem].code	= SJC$_DELETE_FILE;
	spool[elem].bufadr	= 0;
	spool[elem].retlenadr	= 0;
	elem++;

	/*
	 * No pagination
	 */
	spool[elem].buflen	= 0;
	spool[elem].code	= SJC$_NO_PAGINATE;
	spool[elem].bufadr	= 0;
	spool[elem].retlenadr	= 0;
	elem++;

	/*
	 * Form name
	 */
	if (isspace(utl_reportx->spoolform[1]) == 0)
	{
		for (len = 0;
			(utl_reportx->spoolform[len] != ' ') && 
			(len < sizeof(utl_reportx->spoolform)); len++);
		spool[elem].buflen	= len;
		spool[elem].code	= SJC$_FORM_NAME;
		spool[elem].bufadr	= (long)utl_reportx->spoolform;
		spool[elem].retlenadr	= 0;
		elem++;
	}

	/*
	 * Number of copies
	 */
	if (utl_reportx->copies != 0)
	{
		spool[elem].buflen	= 4;
		spool[elem].code	= SJC$_FILE_COPIES;
		spool[elem].bufadr	= (long)utl_reportx->copies;
		spool[elem].retlenadr	= 0;
		elem++;
	}

	/*
	 * End of list
	 */
	spool[elem].buflen	= 0;
	spool[elem].code	= 0;
	spool[elem].bufadr	= 0;
	spool[elem].retlenadr	= 0;

	iosstr.stat = iosstr.zero = 0;

	sys_stat = sys$sndjbcw(0L, SJC$_ENTER_FILE, 0L, spool,
		&iosstr, 0L, 0L, 0L);

#ifdef DEBUG
	if (((sys_stat & 1) == 0) || ((iosstr.stat & 1) == 0))
	{

		sprintf(&errortext, "Error from SYS$SNDJBC = %ld, ios = %ld",
			sys_stat, iosstr.stat);

		dsc_mesg.dsc$w_length = strlen(errortext);
		dsc_mesg.dsc$a_pointer = errortext;
		dsc_mesg.dsc$b_class = DSC$K_CLASS_S;
		dsc_mesg.dsc$b_dtype = DSC$K_DTYPE_T;
		flag = 0;

		entr_3message(&scope, &dsc_mesg, &flag);
	}
#endif
}
