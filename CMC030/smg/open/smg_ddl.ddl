DEFINE RECORD CDD$TOP.SMG.SMG_DDL

        DESCRIPTION IS /*Data Definition Array Communication*/.

        SMG_DDL_CDD STRUCTURE.

        /* Element =
        Description = Description for file */
        DESCR                   DATATYPE IS TEXT SIZE IS 80.

        /* Element =
        Description = Number of fields in file */
        FIELD_NUM               DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Field Name */
        FIELD_NAME              ARRAY 0:255 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Field attribute */
        FIELD_ATTRIBUTE         ARRAY 0:255 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Field type */
        FIELD_TYPE              ARRAY 0:255 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Field element */
        FIELD_ELEMENT           ARRAY 0:255 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Field size */
        FIELD_SIZE              ARRAY 0:255 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Field description */
        FIELD_DESC              ARRAY 0:255 DATATYPE IS TEXT SIZE IS 40.

        END SMG_DDL_CDD STRUCTURE.

END SMG_DDL.
