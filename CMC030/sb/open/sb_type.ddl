DEFINE RECORD CDD$TOP.SB.SB_TYPE

        DESCRIPTION IS /*Subaccount Type Description*/.

        SB_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION5
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END SB_TYPE_CDD STRUCTURE.

END SB_TYPE.
