! HACK assembler written in FORTRAN95
! Compiled using GNU Fortran 9.3.0

MODULE UTIL
IMPLICIT NONE
    ! There is no easy way to use REGEX, so check each character
    CONTAINS
    SUBROUTINE STRIP(STRING)
    IMPLICIT NONE
        CHARACTER(LEN=64), INTENT(INOUT) :: STRING

        CHARACTER(LEN=64) :: RET
        INTEGER :: I, INDEX, ORD

        INDEX = 1
        RET = ""
        DO I=1,LEN_TRIM(STRING)
            ORD = IACHAR(STRING(I:I))
            IF (STRING(I:I) .EQ. '/') THEN
                EXIT
            ELSE IF (ORD .NE. INT(Z"20") .AND. ORD .NE. INT(Z"09")) THEN
                RET(INDEX:INDEX) = STRING(I:I)
                INDEX = INDEX + 1
            ENDIF
        ENDDO

        WRITE (STRING, "(A)") TRIM(ADJUSTL(RET))
    END SUBROUTINE STRIP

    SUBROUTINE STRUP(STRING)
        CHARACTER(LEN=*), INTENT(INOUT) :: STRING

        INTEGER :: I, ORD

        DO I=1,LEN_TRIM(STRING)
            ORD = IACHAR(STRING(I:I))

            IF (ORD .GE. INT(Z"61") .AND. ORD .LE. INT(Z"7A")) THEN
                STRING(I:I) = ACHAR(ORD - 32)
            ENDIF
        ENDDO
    END SUBROUTINE STRUP
END MODULE UTIL

MODULE EXCEPT
IMPLICIT NONE
    CONTAINS
    SUBROUTINE FAIL(STRING)
    IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: STRING

        WRITE (6, "(A)") TRIM(ADJUSTL(STRING))
        CALL EXIT(0)
    END SUBROUTINE FAIL
END MODULE EXCEPT

MODULE ARRAY
IMPLICIT NONE
INTERFACE PUSH_BACK
    MODULE PROCEDURE PUSH_BACK_INT
    MODULE PROCEDURE PUSH_BACK_STR
END INTERFACE
CONTAINS
    SUBROUTINE PUSH_BACK_INT(VEC, INPUT, CUR)
    IMPLICIT NONE
        INTEGER, INTENT(IN) :: INPUT
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: VEC
        INTEGER, INTENT(INOUT) :: CUR

        INTEGER, DIMENSION(:), ALLOCATABLE :: COPY
        INTEGER :: CAPACITY, I

        IF (ALLOCATED(VEC)) THEN
            CAPACITY = SIZE(VEC)
            IF (CUR + 1 .GT. CAPACITY) THEN
                ALLOCATE(COPY(CAPACITY * 2))

                DO I = 1, CAPACITY
                    COPY(I) = VEC(I)
                ENDDO

                DEALLOCATE(VEC)
                CALL MOVE_ALLOC(COPY, VEC)
            ENDIF

            CUR = CUR + 1
            VEC(CUR) = INPUT
        ELSE
            ALLOCATE(VEC(1))
            VEC(1) = INPUT
            CUR = 1
        ENDIF
    END SUBROUTINE PUSH_BACK_INT
    SUBROUTINE PUSH_BACK_STR(VEC, INPUT, CUR)
    IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: INPUT
        CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: VEC
        INTEGER, INTENT(INOUT) :: CUR

        CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE :: COPY
        INTEGER :: CAPACITY, I

        IF (ALLOCATED(VEC)) THEN
            CAPACITY = SIZE(VEC)
            IF (CUR + 1 .GT. CAPACITY) THEN
                ALLOCATE(COPY(CAPACITY * 2))

                DO I = 1, CAPACITY
                    COPY(I) = VEC(I)
                ENDDO

                DEALLOCATE(VEC)
                CALL MOVE_ALLOC(COPY, VEC)
            ENDIF

            CUR = CUR + 1
            VEC(CUR) = INPUT
        ELSE
            ALLOCATE(VEC(1))
            VEC(1) = INPUT
        ENDIF
    END SUBROUTINE PUSH_BACK_STR
END MODULE ARRAY
MODULE ASSEMBLER
USE EXCEPT
USE ARRAY
USE UTIL
IMPLICIT NONE
    INTEGER, DIMENSION(:), ALLOCATABLE :: LADDRS
    CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE :: LABELS
    CHARACTER(LEN=3), DIMENSION(8) :: DESTS = [CHARACTER(LEN=3) :: "","M","D","MD","A","AM","AD","AMD"]
    CHARACTER(LEN=3), DIMENSION(8) :: JUMPS = [CHARACTER(LEN=3) :: "","JGT","JEQ","JGE","JLT","JNE","JLE","JMP"]
    CHARACTER(LEN=3), DIMENSION(34) :: COMPS = [CHARACTER(LEN=3) :: "0","1","-1","D","A","M","!D",&
    "!A","!M","-D","-A","-M","D+1","A+1","M+1","D-1","A-1","M-1","D+A","D+M","D-A","D-M","A-D",&
    "M-D","D&A","D&M","D|A", "D|M","A+D","M+D","A&D","M&D","A|D","M|D"]
    INTEGER, DIMENSION(34) :: COMP_CODES = [ INTEGER :: INT(Z"2A"), INT(Z"3F"), INT(Z"3A"),&
    INT(Z"0C"), INT(Z"30"), INT(Z"70"), INT(Z"0D"), INT(Z"31"), INT(Z"71"), INT(Z"0F"), INT(Z"33"),&
    INT(Z"73"), INT(Z"1F"), INT(Z"37"), INT(Z"77"), INT(Z"0E"), INT(Z"32"), INT(Z"72"), INT(Z"02"),&
    INT(Z"42"), INT(Z"13"), INT(Z"53"), INT(Z"07"), INT(Z"47"), INT(Z"00"), INT(Z"40"), INT(Z"15"),&
    INT(Z"55"), INT(Z"02"), INT(Z"42"), INT(Z"00"), INT(Z"40"), INT(Z"15"), INT(Z"55")]
    CONTAINS
    SUBROUTINE ASSEMBLE(LINES, BINARY, BINARY_SZ)
    IMPLICIT NONE
        CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: LINES
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BINARY
        INTEGER, INTENT(INOUT) :: BINARY_SZ
        INTEGER :: I, J, L, LN
        INTEGER :: LADDRS_SZ = 1, LABELS_SZ = 1
        INTEGER :: INSTRUCTION
        INTEGER :: COUNTER = 0
        CHARACTER(LEN=64) :: TEMP, TEMP2, DEST, COMP, JUMP
        LOGICAL :: EXISTS

        BINARY_SZ = 1

        IF (ALLOCATED(BINARY)) DEALLOCATE(BINARY)

        CALL PUSH_BACK(LABELS, "SCREEN", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 16384, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "KBD", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 24576, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "SP", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 0, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "LCL", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 1, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "ARG", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 2, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "THIS", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 3, LADDRS_SZ)
        CALL PUSH_BACK(LABELS, "THAT", LABELS_SZ)
        CALL PUSH_BACK(LADDRS, 4, LADDRS_SZ)

        DO I=0,15
            WRITE (TEMP2, 30) I
            WRITE (TEMP, "(A,A)") "R", TRIM(ADJUSTL(TEMP2))
            CALL PUSH_BACK(LABELS, TEMP, LABELS_SZ)
            CALL PUSH_BACK(LADDRS, I, LADDRS_SZ)
        ENDDO

        DO I=1,SIZE(LINES)
            CALL STRIP(LINES(I))
        ENDDO

        ! FIRST PASS USED TO FIND LABELS
        DO I=1,SIZE(LINES)
            EXISTS = .FALSE.
            WRITE (TEMP, 20) LINES(I)
            WRITE (TEMP2, 20) ""

            LN = LEN_TRIM(TEMP)

            IF (LN .EQ. 0) THEN
                CYCLE
            ELSE IF (TEMP(1:1) .NE. "(") THEN
                COUNTER = COUNTER + 1
                CYCLE
            ENDIF

            IF (TEMP(LN:LN) .EQ. ")") THEN
                TEMP(1:LN-2) = TEMP(2:LN-1)
                TEMP(LN-1:LN) = ""
                TEMP = TRIM(ADJUSTL(TEMP))

                ! Fail if label is empty
                IF (LEN_TRIM(TEMP) .EQ. 0) CALL FAIL("ERROR: expression expected")

                DO J=1,SIZE(LABELS)
                    IF (LABELS(J) .EQ. TEMP) THEN
                        EXISTS = .TRUE.
                        EXIT
                    ENDIF
                ENDDO

                IF (.NOT. EXISTS) THEN
                    CALL PUSH_BACK(LABELS, TEMP, LABELS_SZ)
                    CALL PUSH_BACK(LADDRS, COUNTER, LADDRS_SZ)
                ENDIF
            ENDIF
        ENDDO

        COUNTER = 16
        DO I=1,SIZE(LINES)
            WRITE (TEMP, 20) LINES(I)
            WRITE (TEMP2, 20) ""

            LN = LEN_TRIM(TEMP)
            IF (LN .EQ. 0 .OR. TEMP(1:1) .NE. "@") CYCLE

            IF (TEMP(LN:LN) .EQ. ";") &
                CALL FAIL("ERROR: end of line expected but semicolon is found")

            IF (TEMP(2:2) .EQ. "-" .OR. LN .EQ. 1) &
                CALL FAIL("ERROR: expression expected")

            WRITE (TEMP, 20) TEMP(2:LN)

            LN = LEN_TRIM(TEMP)
            L = VERIFY(LINES(I)(2:LEN_TRIM(LINES(I))), "0123456789")

            IF (L .EQ. 0) CYCLE

            EXISTS = .FALSE.
            DO J=1,SIZE(LABELS)
                IF (LABELS(J) .EQ. TEMP) THEN
                    EXISTS = .TRUE.
                    WRITE (TEMP2, 30) LADDRS(J)
                    WRITE (LINES(I), 50) "@", TRIM(ADJUSTL(TEMP2))
                    EXIT
                ENDIF
            ENDDO

            IF (.NOT. EXISTS) THEN
                CALL PUSH_BACK(LABELS, TEMP, LABELS_SZ)
                CALL PUSH_BACK(LADDRS, COUNTER, LADDRS_SZ)

                WRITE (TEMP2, 30) COUNTER
                WRITE (LINES(I), 50) "@", TRIM(ADJUSTL(TEMP2))

                COUNTER = COUNTER + 1
            ENDIF
        ENDDO

        DO I=1,SIZE(LINES)
            INSTRUCTION = 0
            WRITE (DEST, 20) ""
            WRITE (COMP, 20) ""
            WRITE (JUMP, 20) ""
            WRITE (TEMP, 20) LINES(I)

            IF (TEMP .EQ. ";" .OR. TEMP(1:1) .EQ. ";") &
                CALL FAIL("ERROR: end of line expected but semicolon is found")

            CALL STRUP(TEMP)

            LN = LEN_TRIM(TEMP)
            IF (LN .EQ. 0) CYCLE

            IF (TEMP(1:1) .EQ. "@") THEN
                READ (TEMP(2:LN), 30) INSTRUCTION
                CALL PUSH_BACK(BINARY, INSTRUCTION, BINARY_SZ)
                CYCLE
            ENDIF

            L = VERIFY(TEMP(1:1), "AMD01-!")
            IF (L .NE. 0) CYCLE

            INSTRUCTION = IOR(INSTRUCTION, LSHIFT(INT(B"111"), 13))

            L = -1
            DO J=1,LN
                IF (TEMP(J:J) .EQ. "=") THEN
                    L = J
                    EXIT
                ENDIF
            ENDDO

            IF (L .NE. -1) THEN
                DEST = TRIM(ADJUSTL(LINES(I)(1:L-1)))
                COMP = TRIM(ADJUSTL(LINES(I)(L+1:LN)))
            ELSE
                COMP = TEMP
            ENDIF

            LN = LEN_TRIM(COMP)
            L = -1
            DO J=1,LN
                IF (COMP(J:J) .EQ. ";") THEN
                    L = J
                    EXIT
                ENDIF
            ENDDO

            IF (L .EQ. LN) THEN
                CALL FAIL("ERROR: end of line expected but semicolon is found")
            ELSE IF (L .NE. -1) THEN
                JUMP = TRIM(ADJUSTL(COMP(L+1:LN)))
                COMP = TRIM(ADJUSTL(COMP(1:L-1)))
            ENDIF

            IF (LEN_TRIM(COMP) .EQ. 0) CALL FAIL("ERROR: expression expected")

            DO J=1,8
                IF (DESTS(J) .EQ. DEST) THEN
                    INSTRUCTION = IOR(INSTRUCTION, LSHIFT(J - 1, 3))
                    EXIT
                ENDIF
            ENDDO

            DO J=1,8
                IF (JUMPS(J) .EQ. JUMP) THEN
                    INSTRUCTION = IOR(INSTRUCTION, J - 1)
                    EXIT
                ENDIF
            ENDDO

            DO J=1,34
                IF (COMPS(J) .EQ. TRIM(ADJUSTL(COMP))) THEN
                    INSTRUCTION = IOR(INSTRUCTION, LSHIFT(COMP_CODES(J), 6))
                    EXIT
                ENDIF
            ENDDO

            CALL PUSH_BACK(BINARY, INSTRUCTION, BINARY_SZ)
        ENDDO
        20 FORMAT (A)
        30 FORMAT (I16)
        50 FORMAT (A,A)
    END SUBROUTINE ASSEMBLE
END MODULE ASSEMBLER

PROGRAM HACK_ASSEMBLER
USE EXCEPT
USE ARRAY
USE ASSEMBLER
IMPLICIT NONE

    CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE :: LINES
    INTEGER, DIMENSION(:), ALLOCATABLE :: BINARY

    CHARACTER(LEN=64) :: INPUT
    INTEGER :: I, LINES_SZ = 1, BINARY_SZ = 1

    READ (5, 20) INPUT
    DO WHILE(INPUT .NE. "EOF")
        CALL PUSH_BACK(LINES, INPUT, LINES_SZ)
        READ (5, 20) INPUT
    ENDDO

    CALL ASSEMBLE(LINES, BINARY, BINARY_SZ)

    IF (SIZE(BINARY) .EQ. 0) CALL EXIT(0)

    DO I=1,BINARY_SZ
        WRITE (6, 40) BINARY(I)
    ENDDO

    20 FORMAT(A)
    40 FORMAT(B0.16)
END PROGRAM HACK_ASSEMBLER