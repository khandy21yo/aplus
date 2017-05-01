DEFINE RECORD CDD$TOP.UTL.UTL_PERIOD

        DESCRIPTION IS /*Period Timekeeper*/.

        UTL_PERIOD_CDD STRUCTURE.

        /* Element = ERA
        Description = Era code */
        ERA                     DATATYPE IS TEXT SIZE IS 2.

        /* Element = YEAR
        Description = Physical year (YYYY) */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = CYCLE
        Description = Period in the physical year */
        CYCLE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Period description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Period status flag */
        PERIOD_STATUS           DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Beginning date (YYYYMMDD) */
        BEG_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = End date (YYYYMMDD) */
        END_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Period sequential number */
        AGE                     DATATYPE IS TEXT SIZE IS 4.

        END UTL_PERIOD_CDD STRUCTURE.

END UTL_PERIOD.
