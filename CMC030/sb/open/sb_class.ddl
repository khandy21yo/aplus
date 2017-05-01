DEFINE RECORD CDD$TOP.SB.SB_CLASS

        DESCRIPTION IS /*Subaccount Class Description*/.

        SB_CLASS_CDD STRUCTURE.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION5
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END SB_CLASS_CDD STRUCTURE.

END SB_CLASS.
