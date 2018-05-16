PROGRAM bissec

    IMPLICIT NONE

    INTEGER :: N = 1000, i
    DOUBLE PRECISION :: in,a,b,x, E = 0.00000001
    CHARACTER(len = 16) :: arg
    CALL get_command_argument(1, arg)
    READ(arg, *) in
    a = 0
    b = in
    do i=0, N
        x = (a+b)/2
        if (x**2 - in < 0) then
            a = x
        else if (x**2 - in > 0) then
            b = x
        else
            print*,real(x)
            EXIT
        end if
    end do
END PROGRAM