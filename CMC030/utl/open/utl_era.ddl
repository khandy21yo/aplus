DEFINE RECORD CDD$TOP.UTL.UTL_ERA

        DESCRIPTION IS /*Accounting Era Description*/.

        UTL_ERA_CDD STRUCTURE.

        /* Element = ERA
        Description = Era code */
        ERA                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Era description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        /* Element = DATE
        Description = Beginning date */
        BEG_DATE                DATATYPE IS TEXT SIZE IS 8.

        END UTL_ERA_CDD STRUCTURE.

END UTL_ERA.
