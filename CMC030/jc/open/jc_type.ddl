DEFINE RECORD CDD$TOP.JC.JC_TYPE

        DESCRIPTION IS /*Job Type Description file*/.

        JC_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END JC_TYPE_CDD STRUCTURE.

END JC_TYPE.
