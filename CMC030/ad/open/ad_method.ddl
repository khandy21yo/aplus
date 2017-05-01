DEFINE RECORD CDD$TOP.AD.AD_METHOD

        DESCRIPTION IS /*Depreciation Method Description*/.

        AD_METHOD_CDD STRUCTURE.

        /* Element = DEP_METHOD
        Description = Depreciation method */
        DEP_METHOD              DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Method description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Method calculation code */
        CALCULATION             DATATYPE IS TEXT SIZE IS 2.

        END AD_METHOD_CDD STRUCTURE.

END AD_METHOD.
