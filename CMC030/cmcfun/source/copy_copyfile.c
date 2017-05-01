/* %TITLE "Copy file function"
 */
#pragma module copy_copyfile "V3.6 Calico"

/*
 * ++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This function copies one file into a ^*new\* file
 *	using a block-by-block copy.
 *	This function will probibly end up replacing COPY_COPYRECORDS
 *	eventually.
 *
 * Index:
 *
 * Parameters:
 *
 *	SOURCE_FILE$
 *		The source file to read from.
 *
 *	DEST_FILE$
 *		The destination file to be created.
 *
 *	Returned value
 *		A status of 1 if returned when sucussful, and
 *		a VMS status code otherwise.
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:COPY_COPYFILE
 *	$ LIB FUNC_LIB:CMCFUN/REP COPY_COPYFILE
 *	$ DELETE COPY_COPYFILE.OBJ;*
 *
 * AUTHOR:
 *
 *	01/01/00 - Unknown,possibly Carl Lydoc
 *
 * MODIFICATION HISTORY:
 *
 *	10/16/99 - Kevin Handy
 *		Stole from internet, changed name from notlib_copy to
 *		copy_copyfile to be compatible with existing functions,
 *		and added several external definitions so that it
 *		wouldn't generate copmpile-time errors. Also some
 *		format changes.
 * --
 */
#include <descrip.h>
#include <rms.h>

extern long SYS$CLOSE();
extern long SYS$CONNECT();
extern long SYS$CREATE();
extern long SYS$OPEN();
extern long SYS$READ();
extern long SYS$WRITE();

long copy_copyfile(struct dsc$descriptor *inp_file, struct dsc$descriptor *out_file)
{
	struct FAB inp_fab, out_fab;
	struct RAB inp_rab, out_rab;
	char buffer[32256];
	long stat;

	inp_fab = cc$rms_fab;
	inp_fab.fab$b_fac = FAB$M_BIO | FAB$M_GET;
	inp_fab.fab$l_fna = inp_file->dsc$a_pointer;
	inp_fab.fab$b_fns = inp_file->dsc$w_length;
	inp_fab.fab$l_fop = FAB$M_SQO;
	inp_fab.fab$b_shr = FAB$M_SHRPUT | FAB$M_UPI;
	if (((stat = SYS$OPEN(&inp_fab)) & 7) != 1)
		return stat;
	inp_rab = cc$rms_rab;
	inp_rab.rab$l_fab = &inp_fab;
	inp_rab.rab$l_rop = RAB$M_BIO;
	if (((stat = SYS$CONNECT(&inp_rab)) & 7) != 1)
	{
		SYS$CLOSE(&inp_fab);
		return stat;
	}
	out_fab = inp_fab;
	out_fab.fab$b_fac = FAB$M_BIO | FAB$M_PUT;
	out_fab.fab$l_fna = out_file->dsc$a_pointer;
	out_fab.fab$b_fns = out_file->dsc$w_length;
	out_fab.fab$w_ifi = 0;
	out_fab.fab$b_shr = FAB$M_SHRPUT | FAB$M_UPI;
	if (((stat = SYS$CREATE(&out_fab)) & 7) != 1)
	{
		SYS$CLOSE(&inp_fab);
		return stat;
	}
	out_rab = inp_rab;
	out_rab.rab$l_fab = &out_fab;
	out_rab.rab$w_isi = 0;
	if (((stat = SYS$CONNECT(&out_rab)) & 7) != 1)
	{
		SYS$CLOSE(&inp_fab);
		SYS$CLOSE(&out_fab);
		return stat;
	}
	inp_rab.rab$l_ubf = buffer;
	inp_rab.rab$w_usz = 32256;
	out_rab.rab$l_rbf = buffer;
	while(1)
	{
		if ((((stat = SYS$READ(&inp_rab)) & 7) != 1) && stat != RMS$_EOF)
		{
			SYS$CLOSE(&inp_fab);
			SYS$CLOSE(&out_fab);
			return stat;
		}
		else if(stat == RMS$_EOF)
		{
			SYS$CLOSE(&inp_fab);
			SYS$CLOSE(&out_fab);
			return RMS$_NORMAL;
		}
		else
		{
			out_rab.rab$w_rsz = inp_rab.rab$w_rsz;
			if (((stat = SYS$WRITE(&out_rab)) & 7) != 1)
			{
				SYS$CLOSE(&inp_fab);
				SYS$CLOSE(&out_fab);
				return stat;
			}
		}
	}

	/*
	 * ? Shouldn't reach here ?
	 */
	return stat;
}
