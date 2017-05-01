DEFINE RECORD CDD$TOP.PR.PR_EMP_MASTER

        DESCRIPTION IS /*Employee Master File*/.

        PR_EMP_MASTER_CDD STRUCTURE.

        /* Element =
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Employee name */
        EMPNAME                 DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Street address */
        ADD1                    DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Post office box */
        ADD2                    DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = FIPS postal abbreviation */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Telephone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Social security number */
        SSN                     DATATYPE IS TEXT SIZE IS 12.

        /* Element =
        Description = Alpha sort key */
        SORT                    DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Default sub account number for costing */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Default account number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Default trade */
        TRADE                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Default Union Code */
        UNION                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Default department */
        DEPT                    DATATYPE IS TEXT SIZE IS 6.

        /* Element = WORK_CENTER
        Description = Work Center */
        WORK_CENTER             DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Employee Skill */
        EMP_SKILL               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Employee grade */
        EMP_GRADE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Disabled (Y/N) */
        DISABLED                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Number of pay periods in a year */
        PAYFREQ                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Default rate type */
        RATE_TYPE               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Default rate code */
        RATE_CDE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = State unemployment code */
        SUI_SW                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Tax Package */
        TAX_PKG                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = CODE
        Description = Workmen comp code */
        WC                      DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = W2 = N, 1099 = Y */
        W2_1099                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Employee birthday */
        BIRTH                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Employee hire date */
        HIREDAY                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Employee termination date */
        TERMDAY                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = FLAG
        Description = Rehire this person flag (Y/N) */
        REHIRE_FLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = M = Male, F = Female */
        SEX                     DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = B=Black,H=Hisp,O=Orien,I=Indian,W=White */
        RACE                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = US Citizen (Y/N) */
        USCIT                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Work permit number */
        WRKPERMIT               DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Home country */
        HOMCNTRY                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Active (Y/N) */
        ACTIVE_FLAG             DATATYPE IS TEXT SIZE IS 1.

        END PR_EMP_MASTER_CDD STRUCTURE.

END PR_EMP_MASTER.
