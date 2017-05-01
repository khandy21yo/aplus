DEFINE RECORD CDD$TOP.TV.TV_FILL

        DESCRIPTION IS /*TV Fill Master File*/.

        TV_FILL_CDD STRUCTURE.

        /* Element =
        Description = Fill number */
        FILNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Fill class */
        FCLASS                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = From date */
        FROM_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = To date */
        TO_DATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Number of runs */
        RUNS                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Number of cuts on tape */
        CUTS                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Current cut */
        CURRENT_CUT             DATATYPE IS SIGNED WORD.

        END TV_FILL_CDD STRUCTURE.

END TV_FILL.
