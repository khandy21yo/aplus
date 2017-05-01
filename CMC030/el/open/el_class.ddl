DEFINE RECORD CDD$TOP.EL.EL_CLASS

        DESCRIPTION IS /*Equipment Ledger Class Description File*/.

        EL_CLASS_CDD STRUCTURE.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END EL_CLASS_CDD STRUCTURE.

END EL_CLASS.
