DEFINE RECORD CDD$TOP.AD.AD_CONTROL

        DESCRIPTION IS /*Asset Depreciation Control File*/.

        AD_CONTROL_CDD STRUCTURE.

        /* Element = DEP_OBJECT
        Description = Depreciation object to the GL */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Last period updated */
        LASTPER                 DATATYPE IS TEXT SIZE IS 6.

        END AD_CONTROL_CDD STRUCTURE.

END AD_CONTROL.
