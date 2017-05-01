DEFINE RECORD CDD$TOP.TV.TV_LOG_SOURCE

        DESCRIPTION IS /*TV Log Source Table*/.

        TV_LOG_SOURCE_CDD STRUCTURE.

        /* Element =
        Description = Log source code */
        SOURCE                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Log source description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END TV_LOG_SOURCE_CDD STRUCTURE.

END TV_LOG_SOURCE.
