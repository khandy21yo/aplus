DEFINE RECORD CDD$TOP.TK.TK_MODULE

        DESCRIPTION IS /*Module Description*/.

        TK_MODULE_CDD STRUCTURE.

        /* Element =
        Description = Module name */
        MODNAME                 DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = */
        CATEGORY                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Module extension */
        EXTENSION               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Language module is written in */
        LANGUAGE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Directory Location */
        DIRECTORY               DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = */
        MODTYPE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = MODNUM
        Description = Module id number */
        MODNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Creation date */
        CDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Creation time */
        CTIME                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Is module sharable */
        SHAREABLE               DATATYPE IS TEXT SIZE IS 1.

        END TK_MODULE_CDD STRUCTURE.

END TK_MODULE.
