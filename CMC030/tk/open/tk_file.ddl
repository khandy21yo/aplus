DEFINE RECORD CDD$TOP.TK.TK_FILE

        DESCRIPTION IS /*File Structure Description*/.

        TK_FILE_CDD STRUCTURE.

        /* Element = FILESTRNAME
        Description = Record structure name */
        STRUCT                  DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = Sequence number */
        SEQUENCE                DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Field name */
        FLDNAME                 DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = Database */
        DATABASE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Field classifier */
        CLASSIFIER              DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Data array (y/n) */
        DATAARRAY               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Date type */
        DATETYPE                DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Size */
        DATASIZE                DATATYPE IS SIGNED LONGWORD.

        /* Element = DATE
        Description = Creating date */
        CDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Creating time */
        CTIME                   DATATYPE IS TEXT SIZE IS 6.

        END TK_FILE_CDD STRUCTURE.

END TK_FILE.
