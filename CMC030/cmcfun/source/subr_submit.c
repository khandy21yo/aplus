/*	%TITLE "SUBR_SUBMIT -- Submit a File to SYS$BATCH"
 */
#pragma module subr_submit "V3.6 Calico"

/*
 * COPYRIGHT (C) 1988 BY
 * Computer Management Center
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies therof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer management Center.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:
 *
 *	This function submits a job to be executen on the
 *	batch processor.
 *
 *	Assumes the use of SYS$BATCH.
 *
 * Index:
 *
 * Parameters:
 *
 *	DDATA$ 
 *		The name of the command file to submit, and any
 *		additional options.
 *
 *		/A<time> - Submit after a specified time
 *		
 *
 *	STATUS% 
 *		The returned status value and may be:
 *	.table
 *			1% - Normal
 *
 *			?  - JBC errors
 *	.endtable
 *
 * Example:
 *
 *	CALL SUBR_SUBMIT(THEDATA$, STATUS%)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:SUBR_SUBMIT/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP SUBR_SUBMIT
 *	$ DELETE SUBR_SUBMIT.OBJ;*
 *
 * Author:
 *
 *	02/23/88 - B. Craig Larsen
 *
 * Modification history:
 *
 *	08/10/88 - Kevin Handy
 *		Modified to submit directly, instead of going
 *		through a detached job.
 *
 *	12/09/88 - Kevin Handy
 *		Finally fixed random crashing by using SYS$SNDJBCW
 *		instead of SYS$SNDJBC.
 *
 *	07/06/89 - Kevin Handy
 *		Modified so function will work in sharable library.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, string.h, sjcdef.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <descrip>

/* #include "func_include:sndjbc.h" */
#include <sjcdef.h>
extern long sys$bintim();
extern long sys$sndjbcw();

/*
 * Local functions
 */
static int findchunk(struct dsc$descriptor *stx, int *start, int *end);

/*
 * Declare vars
 */
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


void subr_submit(struct dsc$descriptor *ddata, int *status)
{
	long sys_stat;
	struct itemlst spool[8];	/* List created of spooler options */
	int Item = 0;			/* Pointer into spool list */
	struct iosb iosstr;
	char AfterQuad[8];		/* Quad word - after time */

	int start = 0;			/* Start position in input string */
	int end;			/* End position in input string */

	struct dsc$descriptor_s dsc_mesg;
					/* Working string variable */
	char errortext[80];
	int flag;

	/*
	 * Queue name
	 *
	 *	bufadr is really a pointer, but defined as long in the header
	 */
	spool[Item].code	= SJC$_QUEUE;
	spool[Item].bufadr	= (long)((char*)"SYS$BATCH");
	spool[Item].buflen	= strlen((char*)spool[0].bufadr);
	spool[Item].retlenadr	= 0;

	/*
	 * Delete the file on completion
	 */
	Item++;
	spool[Item].buflen	= 0;
	spool[Item].code	= SJC$_DELETE_FILE;
	spool[Item].bufadr	= 0;
	spool[Item].retlenadr	= 0;

	/*
	 * No log file
	 */
	Item++;
	spool[Item].buflen	= 0;
	spool[Item].code	= SJC$_NO_LOG_SPECIFICATION;
	spool[Item].bufadr	= 0;
	spool[Item].retlenadr	= 0;

	while (findchunk(ddata, &start, &end))
	{
		if (ddata->dsc$a_pointer[start] != '/')
		{
			/*
			 * File name
			 */
			Item++;
			spool[Item].code	= SJC$_FILE_SPECIFICATION;
			spool[Item].buflen	= end - start + 1;
			spool[Item].bufadr	= (long)(ddata->dsc$a_pointer + start);
			spool[Item].retlenadr	= 0;
			start = end + 1;
		}
		else
		{
			switch (ddata->dsc$a_pointer[start + 1])
			{
			/*
			 * After time
			 */
			case 'A':
			case 'a':
				dsc_mesg.dsc$w_length = end - start - 1;
				dsc_mesg.dsc$a_pointer =
					ddata->dsc$a_pointer + start + 2;
				dsc_mesg.dsc$b_class = DSC$K_CLASS_S;
				dsc_mesg.dsc$b_dtype = DSC$K_DTYPE_T;

				sys_stat = sys$bintim(&dsc_mesg, &AfterQuad);

				if (sys_stat & 1)
				{
					Item++;
					spool[Item].buflen	= 8;
					spool[Item].code	= SJC$_AFTER_TIME;
					spool[Item].bufadr	= (long)AfterQuad;
					spool[Item].retlenadr	= 0;
				}
			}
		}

		/*
		 * Point to next chunk
		 */
		start = end + 1;
	}

	/*
	 * End of list
	 */
	Item++;
	spool[Item].buflen	= 0;
	spool[Item].code	= 0;
	spool[Item].bufadr	= 0;
	spool[Item].retlenadr	= 0;
	
	iosstr.stat, iosstr.zero = 0;

	sys_stat = sys$sndjbcw(0L, SJC$_ENTER_FILE,
		0L, spool, &iosstr, 0L, 0L, 0L);

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

		entr_message(&scope, &dsc_mesg, &flag);
	}
#endif
	*status = sys_stat;

}

static int findchunk(struct dsc$descriptor *ddata, int *start, int *end)
{
	/*
	 * Anything left to look at?
	 */
	if (*start >= ddata->dsc$w_length)
	{
		return 0;
	}

	/*
	 * set up end position. Make sure we get at least on character
	 */
	*end = *start + 1;

	/*
	 * Search for a break
	 */
	while (*end < ddata->dsc$w_length)
	{
		if (ddata->dsc$a_pointer[*end] == '/')
		{
			break;
		}
		(*end)++;
	}

	/*
	 * Must have hit the end of the string
	 */
	(*end)--;
	return 1;
}

