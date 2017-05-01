DEFINE RECORD CDD$TOP.UTL.UTL_DOC_DEST

        DESCRIPTION IS /*Printer Destinations*/.

        UTL_DOC_DEST_CDD STRUCTURE.

        /* Element =
        Description = Name for combination */
        PNAME                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Output device */
        DEST                    DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Printer type */
        PTYPE                   DATATYPE IS TEXT SIZE IS 8.

        END UTL_DOC_DEST_CDD STRUCTURE.

END UTL_DOC_DEST.
