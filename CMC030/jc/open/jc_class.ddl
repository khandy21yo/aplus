DEFINE RECORD CDD$TOP.JC.JC_CLASS

        DESCRIPTION IS /*Job Class Description File*/.

        JC_CLASS_CDD STRUCTURE.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END JC_CLASS_CDD STRUCTURE.

END JC_CLASS.
