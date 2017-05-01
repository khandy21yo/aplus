DEFINE RECORD CDD$TOP.PR.PR_TAX_TABLE

        DESCRIPTION IS /*Payroll Tax Table*/.

        PR_TAX_TABLE_CDD STRUCTURE.

        /* Element =
        Description = Federal,State,County,Munic.,District */
        AUTH                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Used only if AUTH <> "Federal" */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        TSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Employer FICA percentage OASDI */
        FICA_EMPR_PCT           DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Employee FICA percent OASDI */
        FICA_EMPE_PCT           DATATYPE IS SIGNED WORD.

        /* Element =
        Description = FICA limit OASDI */
        FICA_LIMIT              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Federal or <blank> */
        CALC_BASIS              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        BASIS_PCT               DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        OT_ANL_MIN              DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        OT_ANL_PCT              DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        OT_ANL_MAX              DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        STD_WH                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        ADJ_GRS_PCT             DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        MIN_STD_ADJ             DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        MAX_STD_ADJ             DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        PR_EX                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        OVER                    ARRAY 0:11 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        TAXAMT                  ARRAY 0:11 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        PLUS                    ARRAY 0:11 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        SUI_MIN                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        SUI_PCT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        SUI_MAX                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        OT_DED_MAX              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Employer FICA HI percentage */
        FICA_EMPR_PCT_HI        DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Employee FICA HI percentage */
        FICA_EMPE_PCT_HI        DATATYPE IS SIGNED WORD.

        /* Element =
        Description = FICA HI limit */
        FICA_LIMIT_HI           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Personal Exemption for Additional Exempt */
        PR_EX_ADD               DATATYPE IS G_FLOATING.

        /* Element =
        Description = How to round resulting tax */
        DROUNDING               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Old field: Federal Exemption Threshold */
        THRESHOLD               DATATYPE IS TEXT SIZE IS 7.

        /* Element =
        Description = Minimum Annual Prorated Tax */
        MINTAX                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Low income Exemption */
        LOW_INCOME              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Credit for Federal Witholding */
        FED_CREDIT_PCT          DATATYPE IS G_FLOATING.

        END PR_TAX_TABLE_CDD STRUCTURE.

END PR_TAX_TABLE.
