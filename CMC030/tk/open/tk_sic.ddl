DEFINE RECORD CDD$TOP.TK.TK_SIC

        DESCRIPTION IS /*Standard Industrial Classification Codes*/.

        TK_SIC_CDD STRUCTURE.

        /* Element = SIC
        Description = Standard industrial classification code */
        SIC                     DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Division */
        DIVISION                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Business type */
        BUSINESSTYPE            DATATYPE IS TEXT SIZE IS 2.

        END TK_SIC_CDD STRUCTURE.

END TK_SIC.
