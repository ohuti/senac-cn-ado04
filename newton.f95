program newton
    implicit none

    integer :: i, N = 100
    double PRECISION :: p0,p,p1,E=0.0000001
    character(len=16)::arg
    call getarg(1,arg)
    read(arg,*)p0
    p = p0
    do i=0, N
        p1=(p+(p0/p))/2
        if (ABS(p1-p)<E .or. p1**2 == p0) then
            print*, REAL(p)
            EXIT
        end if
        p = p1
    end do
end program newton