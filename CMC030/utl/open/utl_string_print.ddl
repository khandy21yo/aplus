DEFINE RECORD CDD$TOP.UTL.UTL_STRING_PRINT

        DESCRIPTION IS /*String Print Definitions*/.

        UTL_STRING_PRINT_CDD STRUCTURE.

        /* Element =
        Description = System For Reports (AR,PR,GL...) */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = User defined grouping */
        GROUPING                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Report sequence number */
        REPSEQ                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Name to select by */
        TITLES                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Number of report in report file */
        REPNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Settings Flag (Ignore,Set,Query) */
        FLAGS                   ARRAY 0:9 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = For query, code name to ask for */
        CODES                   ARRAY 0:9 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = For Query, title. For Set, value */
        DESCRS                  ARRAY 0:9 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Output Device */
        OUTDEV                  DATATYPE IS TEXT SIZE IS 20.

        END UTL_STRING_PRINT_CDD STRUCTURE.

END UTL_STRING_PRINT.
