DEFINE RECORD CDD$TOP.SA.SA_TYPE

        DESCRIPTION IS /*SA Type File*/.

        SA_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION5
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END SA_TYPE_CDD STRUCTURE.

END SA_TYPE.
