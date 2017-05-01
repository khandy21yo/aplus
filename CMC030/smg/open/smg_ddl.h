/*
 * File Layout for: SMG.SMG_DDL on 11-Mar-99
 *
 * Data Definition Array Communication
 */

struct smg_ddl_cdd
{
/* Element =
   Description = Description for file */
	char descr[80];
/* Element =
   Description = Number of fields in file */
	int field_num;
/* Element =
   Description = Field Name */
	char field_name[256][20];
/* Element =
   Description = Field attribute */
	char field_attribute[256][20];
/* Element =
   Description = Field type */
	char field_type[256][20];
/* Element =
   Description = Field element */
	char field_element[256][20];
/* Element =
   Description = Field size */
	char field_size[256][4];
/* Element =
   Description = Field description */
	char field_desc[256][40];
};
