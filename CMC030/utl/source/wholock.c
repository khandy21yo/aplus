/*
**++
**  MODULE DESCRIPTION:
**
**      Determine File Locks in ARA Applications
**
**      This routine is intended to help determine who or what is causing file
**      access conflicts, specifically record locks.  This program does not deal
**      with file locks, since most ARA software opens files for shared access.
**
**      This routine is limited in scope, using unsupported VMS features,
**      specifically the structure of RMS locks and resource names.  It locates
**      what appear to be RMS record locks, and traces them back to the file in
**      question, attempting to identify it by name.  It will miss any non-RMS
**      locks, and may confuse locks that look like RMS locks (though this
**      should be rare). It will notice only locks on a single node in a
**      cluster.
**
**      When it can, it identifies the file as one it "recognizes" and attempts
**      to display the primary key of the record locked (if indexed), otherwise
**      it displays the RFA.  It displays the file name by translation of the
**      FID.  This translation assumes that the disk is mounted with a logical
**      name equivilent to its label (the default).  The name given may not take
**      into account synonym entries.
**
**      Note that this program makes no attempt to determine if a lock is
**      actually blocking something, since COBOL does not wait for locks to
**      clear in a way indicates a block.  It displays all locks, which may
**      be rather lengthy.
**
**      To make the display shorter, it will accept a list of file names as
**      a parameter or as input between each loop. This list is of the name
**      part of the file spec only, and can be separated by commas or spaces.
**
**      The program must be run with or installed with: CMEXEC to access the
**      executive mode RMS locks, WORLD privilege to access process names in
**      other UIC's, SYSPRV or READALL or BYPASS to allow decoding of file names
**      and lookup of the record's primary key information.
**
**      New files may be added to the list of "recognized" files by adding the
**      record layout as an #include file, and adding an IF clause with
**      appropriate formatting of the output to the routine identify_record.
**      
**  AUTHOR:
**
**      Linwood Ferguson
**
**      (Some code and techniques used with permission from a related program by
**       John Briggs, Vitro Corp, posted on DECUServe, VMS, 604.13 24-Aug-1990)
**       
**  CREATION DATE:  24-Aug-1990
**
**  MODIFICATION HISTORY:
**
**        Date     Author     Description
**
**      
**--
*/

/*  System include files */

#include <lkidef.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fiddef.h>
#include <psldef.h>
#include <lckdef.h>
#include <descrip.h>
#include <stdlib.h>
#include <starlet.h>
#include <jpidef.h>
#include <ssdef.h>
#include <rms.h>
#include <ctype.h>

/*  These are the files recognized where we can find the primary key locked */



/*****          *** Global Data Structures ***                          *****/


static struct iosb_kind         /* IOSB from GETLKI call                    */
{
	int stat,unusued;
} iosb; 


/*  The following is the structure for each active record lock block.       */
/*  These are created and hung off the file blocks (below).  These queues   */
/*  and items are created and destroyed with each pass through the locks.   */


static struct record_kind
{
	struct record_kind *flink,*blink;   /* Forward and backward links       */
	short int rfa[3];                   /* RFA of lock                      */
	long int pid;                       /* Process holding lock             */
	char username[13];                  /* Username of process locking      */
};
  
/*  The following is the structure (and starting pointer) for a list        */
/*  of files.  Each unique FID/DEVICE is linked in here in no particular    */
/*  order.  These in turn have file locks hung off of them (above)          */
/*  These items remain through subsequent passes through the locks, and     */
/*  serve as a FID->name cache so we don't have to translate again each     */
/*  time through.                                                           */

static struct file_kind
{	struct file_kind *flink,*blink;     /* Forward and backwards pointers   */
	struct fiddef fid;                  /* FID this file                    */
	char device[18];                    /* Device name logical              */
	struct record_kind *rflink,*rblink; /* Forward/backward record pointers */
	char file_name[256];                /* C String name                    */
};

static struct file_kind *file_head,*file_tail; /* Pointers to file queue    */
 

/*  Itemlist for wildcard GETLKI                                            */
/*                                                                          */
/*  This item list is used to do the search for RMS record locks            */
  
static long int search_lockid;         /* Lockid to search for (wildcard)   */
static long int holder_pid;            /* PID Of holder of lock             */
static struct statef state;            /* State of lock                     */
static struct namspace namespace;      /* Namespace                         */
static long int parent;                /* Parent of lock                    */
static char resnam[31];                /* Resource name (not printable)     */
static long int resnam_len;    

static struct wild_kind
{
	short int length_1;             /* Item LKI$_RESNAM                     */
	short int code_1;
	char *resnam_ptr;
	long int *resnam_len_ptr;
	short int length_2;             /* Item LKI$_PID                        */
	short int code_2;
	long int *holder_pid_ptr;
	long int holder_pid_zero;
	short int length_3;             /* Item LKI$_STATE                      */
	short int code_3;
	struct statef *state_ptr;
	long int state_zero;
	short int length_4;             /* Item LKI$_NAMSPACE                   */
	short int code_4;
	struct namspace *namespace_ptr;
	long int namespace_zero;
	short int length_5;             /* Item LKI$_PARENT                     */
	short int code_5;
	long int *parent_ptr;
	long int parent_zero;
	long int zero;                  /* End of list                          */
} wild_itmlst = 
{
	31,LKI$_RESNAM,resnam,&resnam_len,
	4,LKI$_PID,&holder_pid,0,
	3,LKI$_STATE,&state,0,
	4,LKI$_NAMSPACE,&namespace,0,
	4,LKI$_PARENT,&parent,0,
	0
};


/*  Item List for Parent Resource GETLKI call                               */
/*                                                                          */
/*  This itemlist is used when a potential RMS record lock is found to      */
/*  evaluate the lock state on that file.                                   */

static char parent_resnam[31];      /* Resource name of parent lock             */
static long int parent_resnam_len;  /* Length of parent resource name           */
static struct parent_kind
{
	short int length_1;             /* Item LKI$_RESNAM                     */
	short int code_1;
	char *parent_resnam_ptr;
	long int *parent_resnam_len_ptr;
	long int zero;                  /* End of list                          */
} parent_itmlst = 
{
	31,LKI$_RESNAM,parent_resnam,&parent_resnam_len,0
};

/*  Itemlist for CMEXEC call for Wildcard search                            */

static struct
{
	long int num_args;
	long int efn;
	long int *lock_ptr;
	struct wild_kind *item_ptr;
	struct iosb_kind *iosb_ptr;
	long int ast_routine;
	long int ast_param;
	long int null_arg;
} wild_cmexec_args =
{
	7,0,&search_lockid,&wild_itmlst,&iosb,0,0,0
};
   
   
/*  Itemlist for CMEXEC call for Parent Lookup of Lock                      */

static struct
{
	long int num_args;
	long int efn;
	long int *lock_ptr;
	struct parent_kind *item_ptr;
	struct iosb_kind *iosb_ptr;
	long int ast_routine;
	long int ast_param;
	long int null_arg;
} parent_cmexec_args =
{
	7,0,&parent,&parent_itmlst,&iosb,0,0,0
};
   
/*   Itemlist for GETJPI of Process Name from PID                           */

static struct
{
	short int length_1;                 /* Ask for JPI$_USERNAME            */
	short int code_1;
	char *username_ptr;                 /* Must be filled in prior to call  */
	long int *username_len_ptr;         /* Don't ask for this- always 12    */
	long int zero;
} getjpi_itmlst =
{
	12,JPI$_USERNAME,0,0,0
};

/*  These structures are used for the RMS access to the files to locate     */
/*  (where possible) the primary key.  These are global to leave the file   */
/*  open between processing each lock for efficiency.                       */

static struct FAB fab;
static struct RAB rab;

static char open_file[256];             /* Name of file (if not open="")    */

/*  These structures hold the current filtering information                 */

static char inp_str[255];           /* Input string                         */
static char files[50][60];          /* Files to display, 50 of 60 char ea   */
static int files_given;             /* -1=all, n=number to filter base zero */


/*
**++
**  BRIEF DESCRIPTION:
**
**      Attempt to read primary key of special indexed file
**
**  FUNCTIONAL DESCRIPTION:
**
**      This routine is called when a file is recognized as one this program
**      understands.  It attempts to open it and read the indicated locked
**      record (Regardless of Lock, of course) so that the caller can display
**      the key.
**
**  CALLING PROTOCOL:
**
**      open_indexed_read_rfa(file_ptr,record_ptr,size_of_buffer)
**
**  FACILITY: INTERNAL
**
**  FORMAL PARAMETERS:
**
**      file_ptr.r.v:    file_kind pointer to file in question (for file spec)
**      record_ptr.r.v:  record_kind pointer to record lock
**      size.r.v:        size of record buffer
**
**  RETURN VALUE:
**
**      Integer = 1 = success, 0 = anykind of failure
**
**  IMPLICIT INPUTS:
**
**      Status of file control blocks that are global
**
**  IMPLICIT OUTPUTS AND SIDE EFFECTS:
**
**      May open file; updates global file control blocks and variables
**
**--
*/
static int open_indexed_read_rfa (struct file_kind *file_ptr,
	struct record_kind *record_ptr,
	int size_of_buffer)
                                      
{
	long int ret;                   /* Misc RMS return                      */

	if (strcmp(open_file,file_ptr->file_name)!=0)  /* see if already open   */
	{
		if (open_file[0])  /* See if prior one open */
		{
			sys$disconnect(&rab);
			sys$close(&rab);
			open_file[0]=0;
			free(rab.rab$l_ubf);
		}
		fab=cc$rms_fab; 
		fab.fab$b_fac=FAB$M_GET;
		fab.fab$l_fna=file_ptr->file_name;
		fab.fab$b_fns=strlen(file_ptr->file_name);
		fab.fab$b_shr=(FAB$M_SHRDEL|FAB$M_SHRGET|FAB$M_SHRPUT|FAB$M_SHRUPD);
		ret=sys$open(&fab);
		if ((ret&1)!=1)
			return 0;     /* Exit if can't open it              */
		rab=cc$rms_rab;
		rab.rab$l_fab = &fab;
		ret=sys$connect(&rab);
		if ((ret&1)!=1)
		{
			sys$close(&fab);
			return 0;
		} /* Exit if can't      */
		strcpy(open_file,file_ptr->file_name);
		rab.rab$b_rac=RAB$C_RFA;   /* We are going to always read by RFA    */
	}
	memmove(rab.rab$w_rfa,&record_ptr->rfa[0],6);  /* Set up and do the read*/
	rab.rab$l_rop|=(RAB$M_NLK|RAB$M_RRL);
	rab.rab$l_ubf = malloc(size_of_buffer);
	rab.rab$w_usz=size_of_buffer;
	ret=sys$get(&rab);
	if ((ret&1)!=1)
		return 0;
	else
		return 1;
}

/*
**++
**  BRIEF DESCRIPTION:
**
**      Attempt to identify a record lock on a file
**
**  FUNCTIONAL DESCRIPTION:
**
**      This procedure recognizes certain special files, and attempts to
**      identify the record locked by key.  If it cannot (or it is not
**      a special file) it exits with a failure code.
**
**  CALLING PROTOCOL:
**
**      identify_record(file_ptr,record_ptr)
**
**  FACILITY: INTERNAL
**
**  FORMAL PARAMETERS:
**
**      file_ptr.r.v:   file_kind pointer to file queue item
**      record_ptr.r.v: record_kind pointer to record lock queue item
**
**  RETURN VALUE:
**
**      integer 0=failed to identify, 1=succeeded
**
**  IMPLICIT INPUTS:
**
**      Current file that may be open.
**
**      The current print line is paused after showing the user name.  It should
**      continue from there.
**
**  IMPLICIT OUTPUTS AND SIDE EFFECTS:
**
**      May open file and try to read it
**
**--
*/
static int identify_record (struct file_kind *file_ptr,
	struct record_kind *record_ptr)
{
	return(0);
}

/*
**++
**  BRIEF DESCRIPTION:
**
**      Process One Record Lock
**
**  FUNCTIONAL DESCRIPTION:
**
**      On entry, lock information about an RMS record lock and its parent are
**      loaded in the global variables.  This routine processes it as needed
**      into the linked lists of locks that are finally printed.
**
**      We porocess all locks here so files are cached for later use (we would
**      have to look up the file name anyway to decide to save the file or not,
**      so we might as well hang on to it)
**      
**  CALLING PROTOCOL:
**
**      process_this_lock()
**
**  FACILITY: INTERNAL
**
**  FORMAL PARAMETERS:
**
**      None
**
**  RETURN VALUE:
**
**      None
**
**  IMPLICIT INPUTS:
**
**      Contents of various global fields for the lock data
**
**  IMPLICIT OUTPUTS AND SIDE EFFECTS:
**
**      Lock is added to the list of locks
**--
*/
static void process_this_lock (void)
{
	struct fiddef this_fid;             /* The FID of this file             */
	char this_device[18];               /* The device for this file         */
	struct record_kind *record_ptr;     /* Running ptr to record lock queue */
	struct file_kind *file_ptr;         /* Running ptr to file queue        */
#pragma nostandard
	$DESCRIPTOR(this_device_dx,this_device);  /* Descriptor for device name */
	$DESCRIPTOR(file_spec_dx,"");       /* Template filespec dx)            */
#pragma standard
	long int ret;                       /* Return from VMS                  */
	long int i;                         /* Running ptr in device name       */
	long int j;                         /* Running ptr in DISK$ logical     */

	memmove(&this_fid,&parent_resnam[4],sizeof(this_fid)); 
    
	/* For device name logical as DISK$ plus whatever in resource           */

	strcpy(this_device,"DISK$");
	for (i=11,j=5;i<=22;i++)
		if (parent_resnam[i]>='$'&&parent_resnam[i]<='_')
			this_device[j++]=parent_resnam[i];
	this_device[j]=0;
	this_device_dx.dsc$w_length=j;
        
	/* See if it already is in the list                                     */

	file_ptr = file_head;
	while (file_ptr != (struct file_kind *)&file_head) 
	{
		if (memcmp(&(file_ptr->fid),&this_fid,sizeof(this_fid))==0 && 
			strcmp(file_ptr->device,this_device)==0) break;
			file_ptr=file_ptr->flink;
	}
	if (file_ptr==(struct file_kind *)&file_head)    /* Not found */
	{
		/* Allocate and fill in */

		file_ptr=malloc(sizeof(struct file_kind));
		if (file_ptr==NULL)
		{
			puts("Unable to MALLOC");
			exit(2);
		}
		memmove(&(file_ptr->fid),&this_fid,sizeof(this_fid));
		strcpy(file_ptr->device,this_device);
		file_ptr->rflink=(struct record_kind*)&(file_ptr->rflink);
		file_ptr->rblink=file_ptr->rflink;
		file_spec_dx.dsc$a_pointer=file_ptr->file_name;
		file_spec_dx.dsc$w_length=sizeof(file_ptr->file_name);
		ret=lib$fid_to_name(&this_device_dx,&this_fid,
			&file_spec_dx,&file_spec_dx.dsc$w_length);
		if ((ret&1)!=1) 
			sprintf(file_ptr->file_name,"** File not found ** %s (%d,%d,%d)",
				this_device,this_fid.fid$w_num,
				this_fid.fid$w_seq,this_fid.fid$w_rvn);
		else
			file_ptr->file_name[file_spec_dx.dsc$w_length]=0;

		/* Insert in queue list at tail of all file entries                 */

		file_ptr->blink = file_tail;
		file_ptr->flink = (struct file_kind *)&file_head;
		if (file_head != (struct file_kind *)&file_head)
			file_tail->flink = file_ptr;
		else
			file_head = file_ptr;
		file_tail=file_ptr;
	}

	/* Fill in record lock item in allocated block (not yet linked)         */

	record_ptr=malloc(sizeof(struct record_kind));
	if (record_ptr==NULL)
	{
		puts("Unable to MALLOC");
		exit(2);
	}
	memmove(&(record_ptr->rfa[0]),&resnam[4],4);  /* RFA is in              */
	memmove(&(record_ptr->rfa[2]),&resnam[0],2);  /*    strange format      */
	record_ptr->pid=holder_pid;
	getjpi_itmlst.username_ptr=record_ptr->username;
	ret=sys$getjpiw(0,&holder_pid,0,&getjpi_itmlst,0,0,0);
	if ((ret&1)!=1)
		strcpy(record_ptr->username,"*not found*");
	else
		record_ptr->username[12]=0;

	/* Insert record lock item in queue for this file                       */

	record_ptr->blink = file_ptr->rblink;
	record_ptr->flink=(struct record_kind*)&(file_ptr->rflink);
	if (file_ptr->rflink != (struct record_kind*)&(file_ptr->rflink))
		(file_ptr->rflink)->flink = record_ptr;
	else
		file_ptr->rflink = record_ptr;
	file_ptr->rblink=record_ptr;
}

/*
**++
**  BRIEF DESCRIPTION:
**
**      Do one Pass through all Locks
**
**  FUNCTIONAL DESCRIPTION:
**
**      This routine makes a single pass through all locks, and handles as
**      appropriate for the type of lock or situation encountered.
**
**      It identifies a RMS record lock because the base lock is:
**
**          - In Executive mode
**          - System wide
**          - Greater than a NL lock
**          - Has a parent (we get parentless file locks indirectly)
**          - Has a resource name length of 8 (to avoid VBN locks)
**          - The parent of the lock must also:
**              . Have a resource name longer than 23 bytes
**              . Start with RMS$
**
**      The format of an RMS file lock resource block is:
**
**          RMS$ + file id + disk name (less DISK$)
**
**      An RMS record lock consists of the RFA being locked as a child lock
**      to the parent file lock.  It may also contain "APPENDER" if the
**      user is simply appending sequentially on the file.  These we ignore
**      because they do not lock a particular record.
**
**      The RFA is not in the usual format.  It is:
**
**          RFA4 + 0X0000 + RFA0
**
**          Example: RFA 123456789012 would be 90120000012345678
**
**
**  CALLING PROTOCOL:
**
**      do_one_pass()
**
**  FACILITY: INTERNAL
**
**  FORMAL PARAMETERS:
**
**      None
**
**  RETURN VALUE:
**
**      None
**
**  IMPLICIT INPUTS:
**
**      Status of locks in VMS
**
**  IMPLICIT OUTPUTS AND SIDE EFFECTS:
**
**      None
**
**--
*/
static void do_one_pass (void)
{
	long int status;                /* Return from GETLKI                   */
	struct file_kind *file_ptr;     /* Running ptr to file                  */
	struct record_kind *record_ptr; /* Running ptr or record lock lists     */

	printf("\n\n**** Current locks Recognized **** \n\n"); search_lockid= -1;
	do
	{
		status = sys$cmexec(sys$getlkiw,&wild_cmexec_args);
		if ((status & 1))
			status = iosb.stat;
		if ((status & 1) != 1)
		{
			if (status != SS$_NOMORELOCK)
				printf("\nDone, but completion status=%s\n",
					strerror(EVMSERR,status));
			break;
		}
		if ( (namespace.lki$b_rmod==PSL$C_EXEC)  &&    /* Exec mode lock    */
/*			namespace.lki$v_sysnam &&                /* System wide       */
			(state.lki$b_state_grmode>LCK$K_NLMODE) &&/* real lock         */
			parent  &&                               /* With a parent     */
			resnam_len == 8 &&                       /* right length      */
			memcmp(resnam,"APPENDER",8))             /* Not seq write lck */
		{
			status = sys$cmexec(sys$getlkiw,&parent_cmexec_args);
			if (status&1)
				status = iosb.stat;
			if (status&1 &&                            /* Found parent      */
				parent_resnam_len>23 &&                /* Long enough res   */
				memcmp(parent_resnam,"RMS$",4) == 0)   /* and RMS           */        
				process_this_lock();                   /* .. so do it       */
		}
	} while(1);

	/* Loop through files and records; free all record blocks as we go      */
	/* Print the file names and then each record lock on those              */

	file_ptr = file_head;
	while (file_ptr != (struct file_kind*)&file_head)
	{
		int do_this_one,i;

		/* See if this file should be displayed                             */

		if (files_given<0)
			do_this_one=1;
		else
		{
			do_this_one=0;
			for (i=0;i<=files_given;i++)
			if (strstr(file_ptr->file_name,files[i])!=NULL)
				do_this_one=1;
		}

		/*  Process all files, but don't print if flag not set              */

		if (file_ptr->rflink!=(struct record_kind*)&file_ptr->rflink && do_this_one)
			puts(file_ptr->file_name);
		record_ptr = file_ptr->rflink;
		while (record_ptr != (struct record_kind*)&(file_ptr->rflink))
		{
			if (do_this_one)
				printf("  %08.8X %12.12s ",
					record_ptr->pid,record_ptr->username);
			if (do_this_one && !identify_record(file_ptr,record_ptr))
				printf("RFA: %04.4X %04.4X %04.4X\n",
					record_ptr->rfa[0],record_ptr->rfa[1],
					record_ptr->rfa[2]);
			free(record_ptr);
			record_ptr = record_ptr->flink;
		}
		file_ptr->rblink=(file_ptr->rflink = (struct record_kind*)&file_ptr->rflink);
		file_ptr = file_ptr->flink;
	}
}

/*
**++
**  BRIEF DESCRIPTION:
**
**      Main Routine - Who is Locking Records
**
**  FUNCTIONAL DESCRIPTION:
**
**      Loop through one pass through the records, and allow
**      the user to either abort or do it again.  
**
**      On each pass (including the first pass by way of command line
**      parameters) the user is allowed to select what files to display
**      (blank = all). These are selected by the file name part
**      only.
**
**      This routine also provides a brief HELP message if the user enters
**      HELP either as a command line parameter or on a command on any
**      loop.
**      
**  CALLING PROTOCOL:
**
**      main()
**
**  FACILITY: INTERNAL
**
**  FORMAL PARAMETERS:
**
**      None
**
**  RETURN VALUE:
**
**      None
**
**  IMPLICIT INPUTS:
**
**      Current status of VMS locks
**
**  IMPLICIT OUTPUTS AND SIDE EFFECTS:
**
**      None
**
**--
*/

long int main (int argc, char *argv[])
{
	char *ret,*p1;
	int i;

	strcpy(inp_str,"");
	for (i=2;i<=argc;i++)
	{
		strcat(inp_str,argv[i-1]);
		strcat(inp_str," ");
	}
	file_head = (struct file_kind*)&file_head;
	file_tail=file_head;
	open_file[0]=0;
	do 
	{ /*  Break up command into file names, add "]" and ".DAT;            */

		p1=inp_str; files_given = -1;
		do 
		{
			ret=strtok(p1," ,;\t");
			p1=NULL;
			if (ret!=NULL) 
			{
				strcpy(files[++files_given],"]");
				strcat(files[files_given],ret);
				strcat(files[files_given],".DAT;");
			}
			i=0;
			while (files[files_given][i])
			{
				files[files_given][i]=
					toupper(files[files_given][i]);
				i++;
			}
		} while (ret!=NULL);

		/* Process files or HELP            */

		if (files_given>=0 && strcmp(files[0],"]HELP.DAT;")==0)  /* If HELP       */
		{
			printf("\n\nThis routine will display locks held on records in shared files on the\n");
			printf("current node.  It will attempt to translate the file names, and for\n");
			printf("certain 'recognized' files it will attempt to show you the primary key\n");
			printf("of the locked record.\n");
			printf("\nTo reduce the length of the list, you may specify by name (e.g. TITLE,\n");
			printf("BULK, etc.) a list of files of interest.  These may be separated by commas\n");
			printf("and may be specified on the command line, or between each display.\n");
			printf("\nUse Control-Z to exit from this program.\n");
			printf("Files:");
		}
		else  /* Finally do the list */
		{
			do_one_pass();
			printf("\nFiles to process (Return=ALL, Control-Z=exit)\nFiles:");
			if (open_file[0])  /* Close any files while we wait             */
			{
				sys$disconnect(&rab);
				sys$close(&rab);
				open_file[0]=0;
			}
		}
		ret=gets(inp_str);
	} while (ret!=NULL);
}
