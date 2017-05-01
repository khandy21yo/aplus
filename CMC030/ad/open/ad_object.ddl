DEFINE RECORD CDD$TOP.AD.AD_OBJECT

        DESCRIPTION IS /*Depreciation Object Desription*/.

        AD_OBJECT_CDD STRUCTURE.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END AD_OBJECT_CDD STRUCTURE.

END AD_OBJECT.
