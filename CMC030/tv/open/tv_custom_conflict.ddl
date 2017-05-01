DEFINE RECORD CDD$TOP.TV.TV_CUSTOM_CONFLICT

        DESCRIPTION IS /*Conflict Codes for a Customer*/.

        TV_CUSTOM_CONFLICT_CDD STRUCTURE.

        /* Element =
        Description = Customer number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Conflict code */
        CONFLICT                DATATYPE IS TEXT SIZE IS 8.

        END TV_CUSTOM_CONFLICT_CDD STRUCTURE.

END TV_CUSTOM_CONFLICT.
