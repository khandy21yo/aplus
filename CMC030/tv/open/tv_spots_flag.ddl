DEFINE RECORD CDD$TOP.TV.TV_SPOTS_FLAG

        DESCRIPTION IS /*Spots Status Flag Table*/.

        TV_SPOTS_FLAG_CDD STRUCTURE.

        /* Element =
        Description = S-schedule, N-not run, R-run */
        FLAG                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Scheduled to run code */
        SCHD_RUN_CODE           DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Code description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END TV_SPOTS_FLAG_CDD STRUCTURE.

END TV_SPOTS_FLAG.
