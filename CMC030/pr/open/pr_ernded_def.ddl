DEFINE RECORD CDD$TOP.PR.PR_ERNDED_DEF

        DESCRIPTION IS /*Payroll ERNDED Definition File*/.

        PR_ERNDED_DEF_CDD STRUCTURE.

        /* Element =
        Description = Payment,Deduction,noncompensaTion,Memo */
        ETYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = ERNDED code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description of ERNDED */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Debit/Credit account for ERNDED */
        DRCR_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Accrual account to credir if accrual */
        ACCRUAL_ACCT            DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Post to GL in summary (Y/N) */
        SUMMARY                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to federal taxes (Y/N) */
        TAXABLE_FWH             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to fica employee taxes */
        TAXABLE_FIE             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to FICA employer taxes (Y/N) */
        TAXABLE_FIR             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to federal unempl. taxes (Y/N) */
        TAXABLE_FUI             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to state taxes (Y/N) */
        TAXABLE_SWH             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to state unempl. taxes (Y/N) */
        TAXABLE_SUI             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to other state taxes (Y/N) */
        TAXABLE_OST             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to city taxes (Y/N) */
        TAXABLE_CWH             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to county taxes (Y/N) */
        TAXABLE_DWH             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Subject to school taxes (Y/N) */
        TAXABLE_EWH             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to federal (Y/N) */
        REPORTABLE_FWH          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to SOC. SEC. admin (Y/N0 */
        REPORTABLE_FIE          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to SOC. SEC. admin */
        REPORTABLE_FIR          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to federal unemployment */
        REPORTABLE_FUI          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to state */
        REPORTABLE_SWH          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to state employement */
        REPORTABLE_SUI          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to state */
        REPORTABLE_OST          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to city */
        REPORTABLE_CWH          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to county */
        REPORTABLE_DWH          DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Reportable to school */
        REPORTABLE_EWH          DATATYPE IS TEXT SIZE IS 1.

        /* Element = FLAG
        Description = Subject to wc */
        SUBJ_WC                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Location to be displayed on W2's */
        W2LOCATION              DATATYPE IS TEXT SIZE IS 4.

        END PR_ERNDED_DEF_CDD STRUCTURE.

END PR_ERNDED_DEF.
