DEFINE RECORD CDD$TOP.EL.EL_TYPE

        DESCRIPTION IS /*Equipment Ledger Type Description File*/.

        EL_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END EL_TYPE_CDD STRUCTURE.

END EL_TYPE.
