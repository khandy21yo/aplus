DEFINE RECORD CDD$TOP.AD.AD_CONTROLOBJ

        DESCRIPTION IS /*Object Control File*/.

        AD_CONTROLOBJ_CDD STRUCTURE.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = ERA
        Description = Era code */
        ERA                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Last period updated */
        LASTPER                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Last period depreciated */
        LASTDEP                 DATATYPE IS TEXT SIZE IS 6.

        /* Element = STATUS_FLAG
        Description = Status flag in the control files */
        STATUS_FLAG             DATATYPE IS TEXT SIZE IS 1.

        END AD_CONTROLOBJ_CDD STRUCTURE.

END AD_CONTROLOBJ.
