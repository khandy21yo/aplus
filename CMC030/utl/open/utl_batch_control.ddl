DEFINE RECORD CDD$TOP.UTL.UTL_BATCH_CONTROL

        DESCRIPTION IS /*Batch Control for Posting.*/.

        UTL_BATCH_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        PROGRAMNAME             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = */
        BFILE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        DSTART                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        TSTART                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        USTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        UTLFILE                 DATATYPE IS TEXT SIZE IS 7.

        /* Element =
        Description = */
        U1FILE                  DATATYPE IS TEXT SIZE IS 7.

        END UTL_BATCH_CONTROL_CDD STRUCTURE.

END UTL_BATCH_CONTROL.
