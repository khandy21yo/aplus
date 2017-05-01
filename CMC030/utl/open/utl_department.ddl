DEFINE RECORD CDD$TOP.UTL.UTL_DEPARTMENT

        DESCRIPTION IS /*Department Profile*/.

        UTL_DEPARTMENT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPT_NUM
        Description = Department number */
        DEPT_NUM                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Department name */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = DEPGROUP
        Description = Department group number */
        DEPGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Department Supervisor */
        SUPERVISOR              DATATYPE IS TEXT SIZE IS 30.

        END UTL_DEPARTMENT_CDD STRUCTURE.

END UTL_DEPARTMENT.
