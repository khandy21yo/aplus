DEFINE RECORD CDD$TOP.TV.TV_COMMER_CONFLICT

        DESCRIPTION IS /*TV Commercial Conflict Table*/.

        TV_COMMER_CONFLICT_CDD STRUCTURE.

        /* Element =
        Description = Conflict code */
        CODE                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Major grouping code */
        MAJOR_CODE              DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        END TV_COMMER_CONFLICT_CDD STRUCTURE.

END TV_COMMER_CONFLICT.
